# KSeF Integration - System Architecture

## Document Purpose
This document defines the strategic architecture for the complete KSeF integration system. All implementations MUST follow this architecture. When creating new classes or methods, Claude Code should strictly adhere to this structure.

---

## 1. System Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         SAP ABAP System                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌────────────────────┐              ┌────────────────────┐            │
│  │  Outbound Cockpit  │              │  Inbound Cockpit   │            │
│  │  (ALV Dual Screen) │              │  (ALV Single)      │            │
│  │  Key: KSEF_ID      │              │  Key: KSEF_NUMBER  │            │
│  └──────────┬─────────┘              └──────────┬─────────┘            │
│             │                                    │                       │
│             ├────────────────┬──────────────────┤                       │
│             │                │                  │                       │
│  ┌──────────▼──────┐  ┌─────▼──────┐  ┌───────▼────────┐             │
│  │ Send Orchestrator│  │ Get        │  │ XML            │             │
│  │                  │  │ Orchestrator│  │ Orchestrator   │             │
│  └──────────┬───────┘  └──────┬─────┘  └────────┬───────┘             │
│             │                  │                  │                      │
│             └─────────┬────────┴──────────────────┘                     │
│                       │                                                  │
│         ┌─────────────▼─────────────────────────────┐                  │
│         │        Foundation Layer                     │                  │
│         ├──────────────┬──────────────┬──────────────┤                  │
│         │  API Client  │ XML Service  │ DB Repository│                  │
│         │  + Auth      │              │              │                  │
│         └──────┬───────┴──────┬───────┴──────┬───────┘                  │
│                │              │              │                           │
│         ┌──────▼──────┐ ┌────▼─────┐ ┌──────▼──────┐                  │
│         │ Certificate │ │ QR Code  │ │ PDF Service │                  │
│         │ Manager     │ │ Service  │ │             │                  │
│         └──────┬──────┘ └────┬─────┘ └──────┬──────┘                  │
│                │              │              │                           │
│         ┌──────▼──────────────▼──────────────▼──────┐                  │
│         │         Infrastructure Layer                │                  │
│         ├──────────────┬──────────────┬──────────────┤                  │
│         │ HTTP Client  │ Logger       │ Config Mgr   │                  │
│         └──────────────┴──────────────┴──────────────┘                  │
│                                                                          │
└──────────────────────────────────────┬───────────────────────────────────┘
                                       │
                              ┌────────▼────────┐
                              │  KSeF API       │
                              │  (Ministry of   │
                              │   Finance)      │
                              └─────────────────┘
```

---

## 2. Naming Conventions

### Class Naming Pattern
```
ZCL_KSEF_<LAYER>_<COMPONENT>
```

**Examples:**
- `ZCL_KSEF_INFRA_HTTP_CLIENT` - Infrastructure layer
- `ZCL_KSEF_FOUND_API_CLIENT` - Foundation layer  
- `ZCL_KSEF_ORCH_SEND` - Orchestrator layer
- `ZCL_KSEF_UI_OUTBOUND_COCKPIT` - UI layer

### Program Naming Pattern
```
ZKSEF_<TYPE>_<PURPOSE>
```

**Examples:**
- `ZKSEF_COCKPIT_OUTBOUND` - Outbound cockpit ALV
- `ZKSEF_COCKPIT_INBOUND` - Inbound cockpit ALV
- `ZKSEF_TEST_API_CLIENT` - Test program

### Table Naming Pattern
```
ZKSEF_<ENTITY>
```

**Examples:**
- `ZKSEF_OUT` - Outbound invoices
- `ZKSEF_IN` - Inbound invoices
- `ZKSEF_CONFIG` - Configuration
- `ZKSEF_BATCH` - Batch processing
- `ZKSEF_QR_CACHE` - QR code cache

---

## 3. Layer Architecture (Bottom-Up)

### 3.1 Infrastructure Layer (Lowest - No Business Logic)

**Purpose:** Reusable technical utilities with no business knowledge

#### ZCL_KSEF_INFRA_HTTP_CLIENT ✅ IMPLEMENTED
**Responsibility:** Generic HTTP communication
```abap
METHODS:
  get( iv_path, it_headers ) -> rs_response
  post( iv_path, iv_body, it_headers ) -> rs_response
  put( iv_path, iv_body, it_headers ) -> rs_response
  delete( iv_path, it_headers ) -> rs_response
```
**Raises:** `ZCX_KSEF_HTTP`

#### ZIF_KSEF_LOG_MANAGER (Interface)
**Responsibility:** Application logging manager with BAL backend. One implementation parameterized by object type.
```abap
METHODS:
  " Initialize - find or create daily MAIN log
  initialize
    IMPORTING iv_object TYPE balobj_d    " KSEFOUT or KSEFIN
    RAISING   zcx_ksef_log_error.

  " Get/create log for specific subobject + external_id
  get_log
    IMPORTING iv_subobject   TYPE balsubobj
              iv_external_id TYPE balnrext
    RETURNING VALUE(rv_log_handle) TYPE balloghndl
    RAISING   zcx_ksef_log_error.

  " Add message to a specific log
  add_msg
    IMPORTING iv_log_handle TYPE balloghndl
              iv_type       TYPE symsgty     " S/E/W/I
              iv_text       TYPE string
    RAISING   zcx_ksef_log_error.

  " Add message to MAIN daily log (convenience)
  add_main_msg
    IMPORTING iv_type TYPE symsgty
              iv_text TYPE string
    RAISING   zcx_ksef_log_error.

  " Save all modified logs to DB
  save RAISING zcx_ksef_log_error.

  " Display log(s) via BAL transaction
  display
    IMPORTING iv_subobject   TYPE balsubobj OPTIONAL
              iv_external_id TYPE balnrext OPTIONAL
    RAISING   zcx_ksef_log_error.
```

#### ZCL_KSEF_INFRA_LOG_MANAGER (Implements ZIF_KSEF_LOG_MANAGER)
**Responsibility:** BAL-based log manager. Constructor receives `iv_object` (KSEFOUT/KSEFIN).
**Behavior:**
- On `initialize`: searches for existing log with object/MAIN/current_day; creates if not found (TTL 3 days); caches handle.
- On `get_log`: checks in-memory cache (hash table subobject+external_id -> handle); searches BAL if not cached; creates if not found.
- On `add_msg`: calls BAL_LOG_MSG_ADD.
- On `save`: calls BAL_DB_SAVE for all modified logs.

**Log Object/Subobject Structure:**
```
KSEFOUT (Outbound):
  MAIN    - ext_id=YYYYMMDD  - Daily cockpit session log (TTL 3 days)
  OVERALL - ext_id=ksef_id   - Per-invoice lifecycle events (TTL 30 days)
  XML     - ext_id=ksef_id   - XML creation & validation (TTL 30 days)
  API     - ext_id=ksef_id   - API send/status/response (TTL 30 days)
  UPO     - ext_id=ksef_id   - UPO retrieval (TTL 30 days)

KSEFIN (Inbound):
  MAIN    - ext_id=YYYYMMDD  - Daily cockpit session log
  OVERALL - ext_id=ksef_number - Per-invoice lifecycle
  GET     - ext_id=timestamp  - Invoice retrieval batches
```
**Uses:** Standard BAL (Business Application Log)
**Raises:** `ZCX_KSEF_LOG_ERROR`

#### ZCL_KSEF_INFRA_CONFIG_MGR ✅ IMPLEMENTED
**Responsibility:** Configuration management
```abap
METHODS:
  get_value( iv_key ) -> rv_value
  set_value( iv_key, iv_value, iv_description )
  exists( iv_key ) -> rv_exists
```
**Raises:** `ZCX_KSEF_API_ERROR`

---

### 3.2 Foundation Layer (Business Utilities)

**Purpose:** Business-specific reusable components

#### ZCL_KSEF_FOUND_DB_REPOSITORY
**Responsibility:** CRUD operations for KSeF tables
```abap
METHODS:
  " Outbound
  save_outbound_invoice( is_invoice ) -> rv_ksef_id
  get_outbound_invoice( iv_ksef_id ) -> rs_invoice
  update_outbound_status( iv_ksef_id, iv_status, iv_details )
  get_outbound_list( is_filter ) -> rt_invoices
  
  " Inbound
  save_inbound_invoice( is_invoice ) -> rv_ksef_number
  get_inbound_invoice( iv_ksef_number ) -> rs_invoice
  update_inbound_status( iv_ksef_number, iv_status )
  get_inbound_list( is_filter ) -> rt_invoices
  
  " Batch
  save_batch( is_batch ) -> rv_batch_id
  get_batch_invoices( iv_batch_id ) -> rt_invoices
  update_batch_status( iv_batch_id, iv_status )
```
**Raises:** `ZCX_KSEF_DB_ERROR`

#### ZCL_KSEF_FOUND_XML_SERVICE
**Responsibility:** XML creation, validation, and parsing. Works in batch mode with per-invoice logging.

**Batch Create & Validate Flow:**
1. Read invoice data from ZKSEF_OUT by input ksef_ids
2. Select master data (BKPF, BSEG, KNA1, LFA1, etc.)
3. For each invoice:
   - Log to OVERALL: "Creation of XML started"
   - Get/create XML log (KSEFOUT/XML/ksef_id); clear messages if log exists
   - Create XML string (errors -> XML log)
   - Validate XML against XSD (errors -> XML log)
   - Add to result table with status (success/failed)

```abap
TYPES:
  BEGIN OF ty_xml_result,
    ksef_id    TYPE zksef_id,
    xml_string TYPE string,
    log_handle TYPE balloghndl,
    status     TYPE char1,  " S=success, F=failed
  END OF ty_xml_result,
  tt_xml_results TYPE STANDARD TABLE OF ty_xml_result WITH KEY ksef_id.

METHODS:
  " Batch creation and validation (main entry point for cockpit)
  create_and_validate_xmls
    IMPORTING it_ksef_ids TYPE zksef_id_table
              io_logger   TYPE REF TO zif_ksef_log_manager
    RETURNING VALUE(rt_results) TYPE tt_xml_results
    RAISING   zcx_ksef_xml_error.

  " Individual operations
  generate_xml( is_invoice_data, is_config ) -> rv_xml
  validate_xml_xsd( iv_xml, iv_xsd_path ) -> rt_errors
  validate_xml_business( iv_xml ) -> rt_errors
  parse_xml_to_structure( iv_xml, is_config ) -> rs_invoice_data
  format_xml( iv_xml ) -> rv_formatted_xml
  extract_xml_element( iv_xml, iv_xpath ) -> rv_value
```
**Raises:** `ZCX_KSEF_XML_ERROR`
**Logs:** Object=KSEFOUT, Subobject=XML, External_ID=KSEF_ID

#### ZCL_KSEF_FOUND_QR_SERVICE
**Responsibility:** QR code generation
```abap
METHODS:
  create_invoice_qr( iv_ksef_number ) -> rv_qr_code_xstring
  create_offline_qr( is_cert_data ) -> rv_qr_code_xstring
  get_qr_as_base64( iv_qr_xstring ) -> rv_base64
```
**Raises:** `ZCX_KSEF_QR_ERROR`

#### ZCL_KSEF_FOUND_PDF_SERVICE
**Responsibility:** PDF generation
```abap
METHODS:
  create_outbound_pdf( iv_ksef_id ) -> rv_pdf_xstring
  create_inbound_pdf( iv_ksef_number ) -> rv_pdf_xstring
  attach_qr_to_pdf( iv_pdf, iv_qr_code ) -> rv_pdf_with_qr
```
**Raises:** `ZCX_KSEF_PDF_ERROR`
**Logs:** Object=KSEF_OUT/KSEF_IN, Subobject=PDF

#### ZCL_KSEF_FOUND_API_CLIENT ✅ IMPLEMENTED (partially)
**Responsibility:** KSeF API business operations (delegates auth to cert_manager)

**Session Management:**
The API client maintains interactive session state internally:
- `mv_session_reference` (string) - current session reference number
- `mv_session_valid_until` (timestamp) - session expiry (12h from creation)
- `mv_cipher_key/iv` (xstring) - AES encryption key/IV for current session
- `ensure_session()` - checks cached session validity, opens new if needed
- `close_session()` - explicitly closes current session
- Session info persisted to ZKSEF_SESSION table for recovery

**Important:** Authentication session (accessToken) and interactive session are separate.
Status checking and UPO retrieval require only accessToken, NOT an active interactive session.

```abap
METHODS:
  " Authentication (delegated to cert_manager)
  get_access_token( ) -> rv_token
  is_token_valid( ) -> rv_valid
  revoke_auth_session( )

  " Interactive Session Management
  ensure_session( is_form_code ) -> rs_session_info  " Opens if needed
  close_session( )
  get_session_info( ) -> rs_session_info

  " Offline Certificate
  get_offline_cert( iv_nip ) -> rs_certificate

  " Invoices - Send (uses current interactive session)
  send_invoice
    IMPORTING iv_xml_content   TYPE string
              iv_offline_mode  TYPE abap_bool DEFAULT abap_false
    RETURNING VALUE(rs_response) TYPE ty_send_invoice_response
    RAISING   zcx_ksef_api_error.

  send_invoices_interactive  " Opens session, sends all, optionally closes
    IMPORTING it_invoice_xml   TYPE string_table
              is_form_code     TYPE ty_form_code OPTIONAL
              iv_offline_mode  TYPE abap_bool DEFAULT abap_false
              iv_close_session TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(rt_responses) TYPE tt_invoice_responses
    RAISING   zcx_ksef_api_error.

  " Invoice Status (requires only accessToken, not interactive session)
  get_invoice_status
    IMPORTING iv_session_reference TYPE string
              iv_invoice_reference TYPE string
    RETURNING VALUE(rs_status)     TYPE ty_invoice_status
    RAISING   zcx_ksef_api_error.

  get_session_invoices  " Paginated list of all invoices in session
    IMPORTING iv_session_reference TYPE string
              iv_page_size         TYPE i DEFAULT 100
              iv_continuation_token TYPE string OPTIONAL
    RETURNING VALUE(rs_response)   TYPE ty_session_invoices_response
    RAISING   zcx_ksef_api_error.

  get_session_status
    IMPORTING iv_session_reference TYPE string
    RETURNING VALUE(rs_status)     TYPE ty_session_status
    RAISING   zcx_ksef_api_error.

  " UPO (requires only accessToken)
  get_invoice_upo_by_ref
    IMPORTING iv_session_reference TYPE string
              iv_invoice_reference TYPE string
    RETURNING VALUE(rv_upo_xml)    TYPE string
    RAISING   zcx_ksef_api_error.

  get_invoice_upo_by_ksef
    IMPORTING iv_session_reference TYPE string
              iv_ksef_number       TYPE string
    RETURNING VALUE(rv_upo_xml)    TYPE string
    RAISING   zcx_ksef_api_error.

  get_session_upo
    IMPORTING iv_session_reference TYPE string
              iv_upo_reference     TYPE string
    RETURNING VALUE(rv_upo_xml)    TYPE string
    RAISING   zcx_ksef_api_error.

  " Session Queries
  list_sessions
    IMPORTING iv_session_type TYPE string  " Online | Batch
              iv_status       TYPE string OPTIONAL  " InProgress|Succeeded|Failed|Cancelled
    RETURNING VALUE(rt_sessions) TYPE tt_sessions
    RAISING   zcx_ksef_api_error.

  " Invoices - Inbound
  get_invoices( is_filter, iv_page_size, iv_page_token ) -> rs_response

  send_batch( it_invoice_xml ) -> rs_batch_response
```

**Key Types:**
```abap
TYPES:
  BEGIN OF ty_send_invoice_response,
    invoice_reference TYPE string,  " invoiceReferenceNumber
  END OF ty_send_invoice_response.

  BEGIN OF ty_invoice_status,
    ordinal_number      TYPE i,
    invoice_number      TYPE string,
    ksef_number         TYPE string,   " NULL until status=200
    reference_number    TYPE string,
    status_code         TYPE i,        " 100,150,200,405-550
    status_description  TYPE string,
    acquisition_date    TYPE string,   " Date KSeF number assigned
    upo_download_url    TYPE string,
    invoicing_mode      TYPE string,   " online | offline
  END OF ty_invoice_status.

  " Invoice status codes:
  " 100 = Acceptance (received)
  " 150 = Processing
  " 200 = Success (ksefNumber assigned)
  " 405+ = Various rejection reasons
```

**Raises:** `ZCX_KSEF_API_ERROR`
**Depends on:** `ZCL_KSEF_FOUND_CERT_MANAGER` (for authentication/tokens), `ZCL_KSEF_INFRA_CRYPTO` (for encryption)

#### ZCL_KSEF_FOUND_CERT_MANAGER ✅ IMPLEMENTED
**Responsibility:** XAdES authentication, certificate and token lifecycle management
```abap
METHODS:
  " XAdES Authentication Flow
  prepare_auth_request( ) -> rs_challenge    " Get challenge + build unsigned XML
  authenticate_with_signed_xml( iv_signed_xml ) -> rs_tokens  " Submit signed XML, redeem tokens
  check_auth_status( iv_reference ) -> rv_status  " Poll auth status (100=pending, 200=done)

  " Token Management
  get_access_token( ) -> rv_token           " Returns cached or refreshed token
  check_token_validity( ) -> rv_valid       " Check if access token is still valid
  refresh_access_token( ) -> rs_new_tokens  " Refresh using refresh token
  is_access_token_expired( ) -> rv_expired
  is_refresh_token_expired( ) -> rv_expired
  get_token_expiry( ) -> rv_expiry_timestamp

  " Token Storage (via config manager)
  store_tokens( is_tokens )
  retrieve_tokens( ) -> rs_tokens
```
**Raises:** `ZCX_KSEF_AUTH_ERROR`
**Note:** API client delegates all authentication to this class. XAdES signing itself is external (user signs XML outside SAP).

---

### 3.3 Orchestrator Layer (Business Workflows)

**Purpose:** Coordinate multiple foundation components

#### ZCL_KSEF_ORCH_XML_CREATE
**Responsibility:** XML generation workflow
```abap
METHODS:
  create_xml_for_invoices( it_ksef_ids ) -> rt_results
  
PRIVATE SECTION:
  METHODS:
    get_enhanced_data( iv_ksef_id ) -> rs_data
    map_to_xml_structure( is_data ) -> rs_xml_struct
    apply_corrections( is_xml_struct ) -> rs_corrected
    merge_to_xml_string( is_xml_struct ) -> rv_xml
```
**Logs:** Object=KSEF_OUT, Subobject=XML, External_ID=KSEF_ID
**Raises:** `ZCX_KSEF_ORCH_ERROR`

#### ZCL_KSEF_ORCH_SEND
**Responsibility:** Invoice sending workflow with session management, status polling, retry logic.

**Session Strategy:**
- Uses API client's `ensure_session()` to reuse valid session or open new one
- Caches session data in API client + persists to ZKSEF_SESSION table
- Closes session when explicitly requested or when cockpit closes
- Each session uses unique AES key/IV for encryption

**Status Polling After Send:**
1. Send invoice -> 202 (get invoiceReferenceNumber)
2. Wait 3 seconds
3. GET invoice status
4. If code=200: SUCCESS, store ksefNumber
5. If code=100/150: mark as SENT (user can update later)
6. If code>=400: mark as REJECTED

**Retry Logic:**
- Configurable max attempts (KSEF_SEND_MAX_ATTEMPTS, default 3)
- Respect Retry-After header on 429
- Increment SEND_ATTEMPTS counter per invoice
- If attempts >= max: set FAILED status

```abap
METHODS:
  " Interactive Mode - sends invoices, polls status, updates DB
  send_interactive
    IMPORTING it_ksef_ids      TYPE zksef_id_table
              iv_offline_mode  TYPE abap_bool DEFAULT abap_false
              iv_close_session TYPE abap_bool DEFAULT abap_true
    RETURNING VALUE(rt_results) TYPE tt_send_results
    RAISING   zcx_ksef_orch_error.

  " Update status for previously sent invoices
  update_invoice_statuses
    IMPORTING it_ksef_ids TYPE zksef_id_table
    RETURNING VALUE(rt_results) TYPE tt_status_results
    RAISING   zcx_ksef_orch_error.

  " Get UPO for accepted invoices
  get_upo_for_invoices
    IMPORTING it_ksef_ids TYPE zksef_id_table
    RETURNING VALUE(rt_results) TYPE tt_upo_results
    RAISING   zcx_ksef_orch_error.

  " Batch Mode
  send_batch( it_ksef_ids ) -> rt_results

  " Offline Mode - generate QR codes + schedule background job
  send_offline
    IMPORTING it_ksef_ids TYPE zksef_id_table
    RETURNING VALUE(rt_results) TYPE tt_send_results
    RAISING   zcx_ksef_orch_error.

  " Close current session explicitly
  close_session
    RAISING zcx_ksef_orch_error.
```
**Logs:** Object=KSEFOUT, Subobject=API, External_ID=KSEF_ID
**Raises:** `ZCX_KSEF_ORCH_ERROR`
**Depends on:** `ZCL_KSEF_FOUND_API_CLIENT`, `ZCL_KSEF_FOUND_DB_REPOSITORY`, `ZIF_KSEF_LOG_MANAGER`

#### ZCL_KSEF_ORCH_GET
**Responsibility:** Invoice retrieval workflow
```abap
METHODS:
  get_invoices_batch( is_filter, iv_max_invoices ) -> rt_invoices
  
PRIVATE SECTION:
  METHODS:
    fetch_from_api( is_filter, iv_page_token ) -> rs_response
    parse_api_response( is_response ) -> rt_invoices
    save_to_database( it_invoices )
    update_processing_status( it_invoices )
```
**Logs:** Object=KSEF_IN, Subobject=GET, External_ID=timestamp
**Raises:** `ZCX_KSEF_ORCH_ERROR`

---

### 3.4 UI Layer (User Interface)

**Purpose:** User interaction screens

#### ZKSEF_COCKPIT_OUTBOUND (Program with dual ALV)
**Responsibility:** Outbound invoice management UI

**Key:** KSEF_ID (runtime generated unique ID)

**ALV Structure:**
```
┌─────────────────────────────────────────────────────────────┐
│ Top ALV: Invoice Overview                                   │
│ Columns: KSEF_ID | Document No | Date | Status | XML |...  │
└─────────────────────────────────────────────────────────────┘
│ Bottom ALV: Invoice Details (Multi-selection for batch)    │
│ Columns: Item | Description | Amount | VAT | Status |...   │
└─────────────────────────────────────────────────────────────┘
```

**Status Types:**

1. **Overall Invoice Status:**
   - NEW - Created, not ready
   - READY - Ready to send
   - SENDING - In progress
   - SENT - Sent to KSeF
   - OFFLINE - In offline mode
   - EMERGENCY - In emergency mode
   - FAILED - Send failed
   - ACCEPTED - KSeF accepted
   - COMPLETED - UPO received

2. **XML Generation Status:**
   - NOT_GENERATED
   - GENERATING
   - GENERATED
   - VALIDATION_FAILED
   - VALIDATED

3. **API Send Status:**
   - PENDING
   - SENT
   - ACCEPTED
   - REJECTED
   - COMM_ERROR

4. **UPO Status:**
   - PENDING
   - SUCCESS
   - FAILED

**Buttons:**
```abap
" XML Operations
BUTTON: Create XML
BUTTON: Show XML
BUTTON: Validate XML

" Send Operations
BUTTON: Send Interactive (single)
BUTTON: Send Batch (multiple)
BUTTON: Send Offline
BUTTON: Send Emergency

" Status Operations
BUTTON: Update Status from KSeF
BUTTON: Get UPO

" Utilities
BUTTON: Show Logs
BUTTON: Export PDF
BUTTON: Resend Failed
```

**Event Handlers:**
```abap
ON_CREATE_XML      -> ZCL_KSEF_ORCH_XML_CREATE->create_xml_for_invoices
ON_SEND_INTERACTIVE-> ZCL_KSEF_ORCH_SEND->send_interactive
ON_SEND_BATCH      -> ZCL_KSEF_ORCH_SEND->send_batch
ON_SEND_OFFLINE    -> ZCL_KSEF_ORCH_SEND->send_offline
ON_UPDATE_STATUS   -> ZCL_KSEF_ORCH_SEND->update_invoice_statuses
ON_GET_UPO         -> ZCL_KSEF_ORCH_SEND->get_upo_for_invoices
ON_SHOW_LOGS       -> ZIF_KSEF_LOG_MANAGER->display
```

**Cockpit Initialization Flow:**
```
1. Create log manager: NEW zcl_ksef_infra_log_manager('KSEFOUT')
2. Initialize: searches/creates MAIN log for current day
3. Find new invoices from selection screen filters
4. Add to main table with generated ksef_id
5. Log "Invoice belnr/gjahr added" to MAIN log
6. Display ALV
```

**Button: Create XML:**
```
1. For each selected invoice (status=NEW):
   a. Log to MAIN: "XML creation for belnr started"
   b. Create OVERALL log header (KSEFOUT/OVERALL/ksef_id)
   c. Store log handle in main table
2. Call XML service with table of ksef_ids
3. XML service creates XML log (KSEFOUT/XML/ksef_id) per invoice
4. Loop results: update main table (xml_content, xml_status)
5. On success: OVERALL_STATUS=READY, XML_STATUS=VALIDATED
6. On error: XML_STATUS=VALIDATION_FAILED, log errors to OVERALL
7. Refresh ALV
```

**Button: Send Interactive:**
```
1. Call ZCL_KSEF_ORCH_SEND->send_interactive(ksef_ids)
2. Orchestrator:
   a. ensure_session() - reuse valid or open new
   b. For each invoice: encrypt, send (202), poll status (3s delay)
   c. Store session_reference, invoice_reference, api_status in DB
   d. Log to KSEFOUT/API/ksef_id
   e. Close session if requested
3. Update main table: OVERALL_STATUS, API_STATUS_CODE, KSEF_NUMBER
4. Refresh ALV
```

**Button: Update Status:**
```
1. For selected invoices with OVERALL_STATUS=SENT (api_status 100/150):
2. Call ZCL_KSEF_ORCH_SEND->update_invoice_statuses(ksef_ids)
3. Uses stored session_reference + invoice_reference
4. GET /sessions/{ref}/invoices/{invoiceRef}
5. If status=200: OVERALL_STATUS=ACCEPTED, store ksef_number
6. If status>=400: OVERALL_STATUS=REJECTED
7. Log to KSEFOUT/API/ksef_id
8. Refresh ALV
```

**Button: Get UPO:**
```
1. For selected invoices with OVERALL_STATUS=ACCEPTED:
2. Call ZCL_KSEF_ORCH_SEND->get_upo_for_invoices(ksef_ids)
3. GET /sessions/{ref}/invoices/{invoiceRef}/upo
4. Store UPO XML, set UPO_STATUS=SUCCESS, OVERALL_STATUS=COMPLETED
5. Log to KSEFOUT/UPO/ksef_id
6. Refresh ALV
```

**Logs:** Object=KSEFOUT, Subobject=OVERALL, External_ID=KSEF_ID

#### ZKSEF_COCKPIT_INBOUND (Program with single ALV)
**Responsibility:** Inbound invoice management UI

**Key:** KSEF_NUMBER (KSeF system assigned)

**Status Types:**
- NEW - Retrieved from KSeF
- SENT_BY_EMAIL - Email sent successfully
- EMAIL_FAILED - Email send failed
- PARKED_SUCCESS - Posted to FI
- PARKING_FAILED - FI posting failed

**Buttons:**
```abap
BUTTON: Get New from KSeF
BUTTON: Show XML
BUTTON: Send by Email
BUTTON: Park Invoice (BAPI)
BUTTON: Show Logs
BUTTON: Export PDF
```

**Event Handlers:**
```abap
ON_GET_NEW -> ZCL_KSEF_ORCH_GET->get_invoices_batch
ON_SEND_EMAIL -> send_pdf_by_email
ON_PARK -> park_invoice_bapi
ON_SHOW_LOGS -> ZCL_KSEF_INFRA_LOGGER->display_log
```

**Logs:** Object=KSEF_IN, Subobject=OVERALL, External_ID=KSEF_NUMBER

---

## 4. Database Schema

### ZKSEF_OUT (Outbound Invoices)
```abap
KSEF_ID            TYPE ZKSEF_ID       " Primary Key - UUID
DOCUMENT_NUMBER    TYPE BELNR_D        " SAP document number
FISCAL_YEAR        TYPE GJAHR          " Fiscal year
DOCUMENT_TYPE      TYPE BLART          " Document type
COMPANY_CODE       TYPE BUKRS
NIP                TYPE ZKSEF_NIP
INVOICE_DATE       TYPE DATUM
AMOUNT             TYPE WRBTR
CURRENCY           TYPE WAERS

" Status Fields
OVERALL_STATUS     TYPE ZKSEF_STATUS   " NEW, READY, SENDING, SENT, ACCEPTED, REJECTED, OFFLINE, FAILED, COMPLETED
XML_STATUS         TYPE ZKSEF_STATUS   " NOT_GENERATED, GENERATING, GENERATED, VALIDATION_FAILED, VALIDATED
API_STATUS_CODE    TYPE INT4           " KSeF status code: 100,150,200,405-550
API_STATUS_DESC    TYPE STRING         " KSeF status description
UPO_STATUS         TYPE ZKSEF_STATUS   " PENDING, SUCCESS, FAILED

" Session & API Reference Fields
SESSION_REFERENCE  TYPE STRING         " Session referenceNumber (from open session)
INVOICE_REFERENCE  TYPE STRING         " Invoice referenceNumber (from 202 Accepted)
KSEF_NUMBER        TYPE ZKSEF_NUM      " Final KSeF number (35 chars, assigned on status=200)
ACQUISITION_DATE   TYPE TIMESTAMP      " Date KSeF number was assigned
BATCH_ID           TYPE ZKSEF_BATCH_ID

" XML Data
XML_CONTENT        TYPE STRING         " Generated XML
XML_HASH           TYPE ZKSEF_HASH
UPO_CONTENT        TYPE STRING         " UPO XML document
UPO_DOWNLOAD_URL   TYPE STRING         " Direct UPO download URL (expires)
UPO_URL_EXPIRY     TYPE TIMESTAMP      " URL expiration date

" Offline Mode
QR_CODE_INVOICE    TYPE XSTRING        " QR Code I - invoice verification
QR_CODE_COMPANY    TYPE XSTRING        " QR Code II - identity confirmation
OFFLINE_JOB_ID     TYPE TBTCJOB
OFFLINE_MODE       TYPE ABAP_BOOL      " offlineMode flag sent to API

" Retry & Tracking
SEND_ATTEMPTS      TYPE INT4           " Number of send attempts
MAX_ATTEMPTS       TYPE INT4           " Max attempts before FAILED
LAST_STATUS_CHECK  TYPE TIMESTAMP      " Last time status was polled

" Log Handles (BAL)
LOG_OVERALL        TYPE BALLOGHNDL     " KSEFOUT/OVERALL/ksef_id
LOG_XML            TYPE BALLOGHNDL     " KSEFOUT/XML/ksef_id
LOG_API            TYPE BALLOGHNDL     " KSEFOUT/API/ksef_id
LOG_UPO            TYPE BALLOGHNDL     " KSEFOUT/UPO/ksef_id

" Timestamps
CREATED_AT         TYPE TIMESTAMP
CREATED_BY         TYPE SYUNAME
CHANGED_AT         TYPE TIMESTAMP
CHANGED_BY         TYPE SYUNAME
SENT_AT            TYPE TIMESTAMP
```

### ZKSEF_IN (Inbound Invoices)
```abap
KSEF_NUMBER        TYPE ZKSEF_NUM      " Primary Key
REFERENCE_NUMBER   TYPE ZKSEF_REF
INVOICE_NUMBER     TYPE BELNR_D
SELLER_NIP         TYPE ZKSEF_NIP
SELLER_NAME        TYPE NAME1
BUYER_NIP          TYPE ZKSEF_NIP
INVOICE_DATE       TYPE DATUM
AMOUNT             TYPE WRBTR
CURRENCY           TYPE WAERS

" Status
PROCESSING_STATUS  TYPE ZKSEF_STATUS   " NEW, SENT_BY_EMAIL, etc.

" XML Data
XML_CONTENT        TYPE STRING

" System Integration
FI_DOCUMENT        TYPE BELNR_D        " Posted document
EMAIL_SENT         TYPE ABAP_BOOL
EMAIL_DATE         TYPE TIMESTAMP

" Timestamps
RECEIVED_AT        TYPE TIMESTAMP
PROCESSED_AT       TYPE TIMESTAMP
```

### ZKSEF_BATCH (Batch Processing)
```abap
BATCH_ID           TYPE ZKSEF_BATCH_ID " Primary Key - UUID
BATCH_TYPE         TYPE CHAR1          " S=Send, R=Receive
BATCH_SIZE         TYPE I
STARTED_AT         TYPE TIMESTAMP
COMPLETED_AT       TYPE TIMESTAMP
STATUS             TYPE ZKSEF_STATUS   " PROCESSING, COMPLETED, FAILED

" KSeF Response
REFERENCE_NUMBER   TYPE ZKSEF_REF
```

### ZKSEF_SESSION (Interactive Session Tracking)
```abap
SESSION_REFERENCE  TYPE STRING         " Primary Key - session referenceNumber
VALID_UNTIL        TYPE TIMESTAMP      " Session expiry (12h from creation)
STATUS             TYPE CHAR10         " OPEN, CLOSED, EXPIRED
FORM_CODE_SYSTEM   TYPE STRING         " e.g. "FA (3)"
FORM_CODE_SCHEMA   TYPE STRING         " e.g. "1-0E"
FORM_CODE_VALUE    TYPE STRING         " e.g. "FA"
INVOICE_COUNT      TYPE INT4           " Total invoices sent in session
SUCCESS_COUNT      TYPE INT4           " Successfully processed
FAILED_COUNT       TYPE INT4           " Failed processing
CREATED_BY         TYPE SYUNAME
CREATED_AT         TYPE TIMESTAMP
CLOSED_AT          TYPE TIMESTAMP
```
**Note:** AES cipher key/IV are kept only in memory during session lifetime for security reasons.
They are not persisted to the database.

### ZKSEF_CONFIG (Configuration) ✅ IMPLEMENTED
```abap
CONFIG_KEY         TYPE STRING         " Primary Key
CONFIG_VALUE       TYPE STRING
DESCRIPTION        TYPE STRING
ACTIVE             TYPE ABAP_BOOL
CREATED_BY         TYPE SYUNAME
CREATED_AT         TYPE TIMESTAMP
CHANGED_BY         TYPE SYUNAME
CHANGED_AT         TYPE TIMESTAMP
```

### ZKSEF_XML_CONFIG (XML Generation Config)
```abap
CONFIG_ID          TYPE ZKSEF_CFG_ID   " Primary Key
INVOICE_TYPE       TYPE VBTYP          " Document type
TAG_NAME           TYPE STRING
TAG_PATH           TYPE STRING         " XPath
TAG_LEVEL          TYPE I              " Hierarchy level
PARENT_TAG         TYPE STRING
DATA_SOURCE        TYPE STRING         " Table.field
TRANSFORMATION     TYPE STRING         " Conversion rule
MANDATORY          TYPE ABAP_BOOL
DEFAULT_VALUE      TYPE STRING
```

---

## 5. Logging Strategy

### Log Objects (BAL)
```
KSEFOUT - Outbound invoices
KSEFIN  - Inbound invoices
```

### Log Subobjects and External IDs

| Object | Subobject | External ID | Purpose | TTL |
|--------|-----------|-------------|---------|-----|
| KSEFOUT | MAIN | YYYYMMDD (current day) | Daily cockpit log: invoice additions, session events, errors | 3 days |
| KSEFOUT | OVERALL | ksef_id | Per-invoice lifecycle: all major status changes | 30 days |
| KSEFOUT | XML | ksef_id | XML creation & validation details | 30 days |
| KSEFOUT | API | ksef_id | API send, status check, response details | 30 days |
| KSEFOUT | UPO | ksef_id | UPO retrieval actions | 30 days |
| KSEFIN | MAIN | YYYYMMDD | Daily inbound cockpit log | 3 days |
| KSEFIN | OVERALL | ksef_number | Per-invoice lifecycle | 30 days |
| KSEFIN | GET | timestamp | Invoice retrieval batches | 30 days |

### Logger Manager Behavior
1. **Cockpit starts** -> `log_manager->initialize('KSEFOUT')` -> finds/creates MAIN log for today
2. **New invoice found** -> `log_manager->add_main_msg('I', 'Invoice belnr/gjahr added with ksef_id')`
3. **Create XML** -> `log_manager->get_log('OVERALL', ksef_id)` -> creates OVERALL log for invoice
4. **XML Service** -> `log_manager->get_log('XML', ksef_id)` -> creates/clears XML log for details
5. **Send** -> `log_manager->get_log('API', ksef_id)` -> creates API log for send/status details
6. **Get UPO** -> `log_manager->get_log('UPO', ksef_id)` -> creates UPO log
7. **After each operation** -> `log_manager->save()` -> persists all modified logs

### Example Log Entries

**MAIN log (KSEFOUT/MAIN/20260129):**
```
[I] Invoice 5100000001/2026 added to cockpit (ksef_id: ABC-123)
[I] Invoice 5100000002/2026 added to cockpit (ksef_id: DEF-456)
[I] XML creation started for 2 invoices
[I] Interactive session opened: ref=20260129-SO-xxxxx, valid until 2026-01-30 00:30:00
[E] Session open failed: Connection timeout
```

**OVERALL log (KSEFOUT/OVERALL/ABC-123):**
```
[I] Invoice added to cockpit
[I] XML creation started
[S] XML created and validated
[I] Send interactive started
[S] Invoice sent: invoiceRef=20260129-FI-xxxxx, status=202
[I] Status check: code=150 (Processing)
[S] Status check: code=200 (Success), ksefNumber=5265877635-20260129-0100001AF629-AF
[S] UPO retrieved successfully
```

**XML log (KSEFOUT/XML/ABC-123):**
```
[I] XML string creation started
[S] XML string created (size: 4523 bytes)
[I] XSD validation started
[S] XML validated against FA(3) schema v1-0E
```

**API log (KSEFOUT/API/ABC-123):**
```
[I] Sending to session 20260129-SO-xxxxx
[I] POST /sessions/online/{ref}/invoices -> 202 Accepted
[I] invoiceReferenceNumber: 20260129-FI-xxxxx
[I] Polling status (attempt 1, delay 3s)
[I] GET /sessions/{ref}/invoices/{invoiceRef} -> status 150
[I] Polling status (attempt 2, delay 5s)
[S] Status 200: ksefNumber=5265877635-20260129-0100001AF629-AF
```

---

## 6. Exception Hierarchy

```
ZCX_KSEF_BASE (abstract)
├── ZCX_KSEF_HTTP (HTTP/network errors)
├── ZCX_KSEF_API_ERROR (KSeF API business errors)
├── ZCX_KSEF_AUTH_ERROR (Authentication/authorization)
├── ZCX_KSEF_CRYPTO (Encryption/decryption/hashing errors)
├── ZCX_KSEF_XML_ERROR (XML generation/validation)
├── ZCX_KSEF_DB_ERROR (Database operations)
├── ZCX_KSEF_LOG_ERROR (BAL logging errors)
├── ZCX_KSEF_PDF_ERROR (PDF generation)
├── ZCX_KSEF_QR_ERROR (QR code generation)
└── ZCX_KSEF_ORCH_ERROR (Orchestration workflows)
```

**Common Attributes:**
```abap
error_message  TYPE string  " Human-readable
http_code      TYPE i       " HTTP status (if applicable)
error_code     TYPE string  " Technical error code
error_details  TYPE string  " Full technical details
timestamp      TYPE timestamp
previous       TYPE REF TO cx_root  " Chain exceptions
```

---

## 7. Offline Mode Workflow

### Trigger
User clicks "Send Offline" button in outbound cockpit

### Process
```
1. Generate 2 QR codes:
   - QR_CODE_INVOICE: Invoice validation QR
   - QR_CODE_COMPANY: Company certificate QR (offline mode proof)

2. Update ZKSEF_OUT:
   - OVERALL_STATUS = 'OFFLINE'
   - QR_CODE_INVOICE = generated_qr_1
   - QR_CODE_COMPANY = generated_qr_2

3. Create background job:
   - Job name: ZKSEF_OFFLINE_RETRY_{KSEF_ID}
   - Start time: Immediate
   - Repeat: Every 30 minutes
   - End time: End of next day (23:59:59)
   - Program: ZKSEF_OFFLINE_PROCESSOR
   - Variant: Contains KSEF_ID

4. Job attempts:
   - Try to send invoice to KSeF API
   - If success: 
     * Update status to SENT
     * Cancel remaining job steps
   - If failed:
     * Log attempt
     * Continue to next scheduled run
   - At end time:
     * If still not sent, mark as OFFLINE_EXPIRED
```

### Background Job Program
```abap
REPORT zksef_offline_processor.

PARAMETERS: p_ksef_id TYPE zksef_id.

START-OF-SELECTION.
  DATA(lo_send) = NEW zcl_ksef_orch_send( ).
  
  TRY.
      DATA(ls_result) = lo_send->send_offline( p_ksef_id ).
      
      IF ls_result-success = abap_true.
        " Cancel job - success!
        MESSAGE 'Invoice sent successfully' TYPE 'S'.
      ENDIF.
      
    CATCH zcx_ksef_orch_error INTO DATA(lx_error).
      " Log and continue
      MESSAGE lx_error->error_message TYPE 'E'.
  ENDTRY.
```

---

## 8. Emergency Mode Workflow

### Trigger
User clicks "Send Emergency" button

### Process
```
1. Validate prerequisites:
   - System must be in declared emergency state
   - User must have emergency authorization
   - Invoice must meet emergency criteria

2. Generate emergency XML:
   - Add emergency mode flag
   - Add declaration number
   - Add emergency justification

3. Send to KSeF with emergency endpoint:
   - POST /invoices/emergency
   - Special authorization token

4. Update status:
   - OVERALL_STATUS = 'EMERGENCY'
   - Log emergency declaration

5. Follow-up requirements:
   - Must submit regular invoice within X days
   - Emergency invoice gets special marking
```

---

## 9. Batch Processing

### Outbound Batch Send
```
Selection Criteria:
- Multiple invoices selected in bottom ALV
- All must have OVERALL_STATUS = 'READY'
- All must have validated XML

Process:
1. Group invoices into batch (max 100)
2. Create ZKSEF_BATCH record
3. Call ZCL_KSEF_ORCH_SEND->send_batch
4. API sends all in single request
5. Update individual statuses from response
6. Log batch results
```

### Inbound Batch Receive
```
Parameters:
- Date range filter
- Amount filter
- NIP filter
- Page size (max 100)

Process:
1. Call ZCL_KSEF_ORCH_GET->get_invoices_batch
2. API returns paginated results
3. Parse each invoice
4. Save to ZKSEF_IN
5. Continue pagination until complete
6. Log summary
```

---

## 10. API Client Methods Mapping (KSeF API v2)

### Authentication (via cert_manager)
```
POST /auth/challenge                          -> prepare_auth_request
POST /auth/xades-signature                    -> authenticate_with_signed_xml
GET  /auth/{referenceNumber}                  -> check_auth_status
POST /auth/token/redeem                       -> (internal - redeem tokens)
POST /auth/token/refresh                      -> refresh_access_token
GET  /auth/sessions                           -> (list auth sessions)
DELETE /auth/sessions/current                 -> revoke_auth_session
```

### Interactive Session Lifecycle
```
POST /sessions/online                         -> ensure_session / open_session (201)
POST /sessions/online/{ref}/invoices          -> send_invoice (202)
POST /sessions/online/{ref}/close             -> close_session (204)
```

### Session Status & Invoice Tracking (requires only accessToken)
```
GET  /sessions                                -> list_sessions (query by type/status)
GET  /sessions/{ref}                          -> get_session_status
GET  /sessions/{ref}/invoices                 -> get_session_invoices (paginated)
GET  /sessions/{ref}/invoices/{invoiceRef}    -> get_invoice_status
GET  /sessions/{ref}/invoices/failed          -> (get failed invoices)
```

### UPO Retrieval (requires only accessToken)
```
GET /sessions/{ref}/invoices/{invoiceRef}/upo -> get_invoice_upo_by_ref
GET /sessions/{ref}/invoices/ksef/{ksefNum}/upo -> get_invoice_upo_by_ksef
GET /sessions/{ref}/upo/{upoRef}             -> get_session_upo (bulk)
```

### Inbound Invoices
```
POST /invoices/query/metadata                 -> query invoice metadata
GET  /invoices/ksef/{ksefNumber}              -> get single invoice by KSeF number
POST /invoices/exports                        -> export invoice batch
```

### Certificates
```
GET /certificates/public-key                  -> get_symmetric_encryption_cert
```

### Key Insight: Authentication vs Interactive Session
- **Auth session** (/auth/*): provides accessToken, needed for ALL protected endpoints
- **Interactive session** (/sessions/online/*): for SENDING invoices only
- Status checking, UPO retrieval, session queries use /sessions/* (NOT /sessions/online/*)
  and require only a valid accessToken

---

## 11. Configuration Keys

### Core Settings
```
KSEF_API_URL                  - Base API URL
KSEF_API_URL_DEMO            - Demo system URL
KSEF_API_URL_PROD            - Production system URL
KSEF_ENVIRONMENT             - TEST | DEMO | PROD
KSEF_NIP                     - Company NIP
KSEF_HTTP_TIMEOUT            - HTTP timeout (seconds)
KSEF_SSL_IDENTITY            - SSL identity
```

### Authentication
```
KSEF_ACCESS_TOKEN            - Current access token
KSEF_REFRESH_TOKEN           - Current refresh token
KSEF_ACCESS_TOKEN_VALID_UNTIL
KSEF_REFRESH_TOKEN_VALID_UNTIL
KSEF_CERT_EXPIRY             - Certificate expiry date
```

### Batch Processing
```
KSEF_BATCH_SIZE              - Max invoices per batch
KSEF_RETRY_COUNT             - Retry attempts
KSEF_RETRY_DELAY             - Delay between retries (sec)
```

### Offline Mode
```
KSEF_OFFLINE_RETRY_INTERVAL  - Minutes between retries
KSEF_OFFLINE_MAX_ATTEMPTS    - Max retry attempts
```

---

## 12. Development Guidelines

### Creating New Classes

**1. Determine Layer:**
- Infrastructure: Technical utilities, no business logic
- Foundation: Business utilities, reusable
- Orchestrator: Workflows, coordinates foundation
- UI: User interaction only

**2. Naming:**
```abap
CLASS zcl_ksef_<layer>_<component> DEFINITION.
```

**3. Structure:**
```abap
PUBLIC SECTION.
  " Public interface methods only
  
PRIVATE SECTION.
  " Private implementation methods
  DATA: mo_logger TYPE REF TO zcl_ksef_infra_logger.
  
  METHODS: initialize.
```

**4. Error Handling:**
```abap
RAISING zcx_ksef_<layer>_error
```

**5. Logging:**
```abap
" Always log significant steps
mo_logger->add_message(
  iv_type = 'I'
  iv_message = 'Operation started'
  iv_detail = 'Technical details'
).
```

### Creating New Methods

**1. Method Signature:**
```abap
METHODS method_name
  IMPORTING
    iv_param TYPE type
  EXPORTING
    ev_param TYPE type
  RETURNING
    VALUE(rv_result) TYPE type
  RAISING
    zcx_ksef_xxx_error.
```

**2. Documentation:**
```abap
"! Method description
"! @parameter iv_param | Parameter description
"! @parameter rv_result | Return value description
"! @raising zcx_ksef_xxx_error | When error occurs
```

**3. Implementation Pattern:**
```abap
METHOD method_name.
  " 1. Initialize
  DATA: lv_log_handle TYPE balloghndl.
  
  " 2. Validate input
  IF iv_param IS INITIAL.
    RAISE EXCEPTION TYPE zcx_ksef_xxx_error
      EXPORTING error_message = 'Parameter required'.
  ENDIF.
  
  " 3. Log start
  lv_log_handle = mo_logger->create_log(
    iv_object = 'KSEF_XXX'
    iv_subobject = 'YYY'
  ).
  
  TRY.
      " 4. Execute logic
      rv_result = perform_operation( iv_param ).
      
      " 5. Log success
      mo_logger->add_message(
        iv_log_handle = lv_log_handle
        iv_type = 'S'
        iv_message = 'Operation successful'
      ).
      
    CATCH cx_root INTO DATA(lx_error).
      " 6. Log error
      mo_logger->add_message(
        iv_log_handle = lv_log_handle
        iv_type = 'E'
        iv_message = lx_error->get_text( )
      ).
      
      " 7. Re-raise as appropriate exception
      RAISE EXCEPTION TYPE zcx_ksef_xxx_error
        EXPORTING
          previous = lx_error
          error_message = lx_error->get_text( ).
  ENDTRY.
  
  " 8. Save log
  mo_logger->save_log( lv_log_handle ).
ENDMETHOD.
```

---

## 13. Testing Strategy

### Unit Tests
**Location:** Include programs or test classes

**Pattern:**
```abap
CLASS ltc_test_class DEFINITION FOR TESTING.
  PRIVATE SECTION.
    DATA: cut TYPE REF TO zcl_ksef_xxx.
    
    METHODS: setup.
    METHODS: test_method_success FOR TESTING.
    METHODS: test_method_error FOR TESTING.
ENDCLASS.
```

### Integration Tests
**Programs:** `ZKSEF_TEST_*`

### Test Data
**Tables:** `ZKSEF_TEST_*` (for test data)

---

## 14. Migration and Deployment

### Phase 1: Infrastructure (Week 1)
```
✅ ZCX_KSEF_* exceptions
✅ ZCL_KSEF_INFRA_HTTP_CLIENT
✅ ZCL_KSEF_INFRA_CONFIG_MGR
✅ ZKSEF_CONFIG table
□ ZCL_KSEF_INFRA_LOGGER
```

### Phase 2: Foundation (Week 2-3)
```
✅ ZCL_KSEF_FOUND_API_CLIENT (partial)
✅ ZCL_KSEF_FOUND_CERT_MANAGER (partial)
□ ZCL_KSEF_FOUND_DB_REPOSITORY
□ ZCL_KSEF_FOUND_XML_SERVICE
□ ZCL_KSEF_FOUND_QR_SERVICE
□ ZCL_KSEF_FOUND_PDF_SERVICE
□ ZKSEF_OUT, ZKSEF_IN, ZKSEF_BATCH tables
□ ZKSEF_XML_CONFIG table
```

### Phase 3: Orchestrators (Week 4)
```
□ ZCL_KSEF_ORCH_XML_CREATE
□ ZCL_KSEF_ORCH_SEND
□ ZCL_KSEF_ORCH_GET
□ ZKSEF_OFFLINE_PROCESSOR program
```

### Phase 4: UI (Week 5-6)
```
□ ZKSEF_COCKPIT_OUTBOUND
□ ZKSEF_COCKPIT_INBOUND
```

### Phase 5: Testing (Week 7)
```
□ Integration tests
□ End-to-end testing
□ Performance testing
```

### Phase 6: Production (Week 8)
```
□ Production deployment
□ Monitoring setup
□ User training
```

---

## 15. How to Apply This Architecture in Claude Code

### Step 1: Load Architecture
```
1. Start Claude Code session
2. Upload this file: KSEF_ARCHITECTURE.md
3. Instruct Claude:
   "Always follow the architecture defined in KSEF_ARCHITECTURE.md
    when creating new classes or methods"
```

### Step 2: Reference During Development
```
User: "Create the DB Repository class"

Claude: [Reads KSEF_ARCHITECTURE.md]
        "I'll create ZCL_KSEF_FOUND_DB_REPOSITORY following 
         section 3.2 Foundation Layer guidelines..."
```

### Step 3: Validation
```
User: "Validate this class against architecture"

Claude: [Checks against KSEF_ARCHITECTURE.md]
        "✓ Class name follows pattern: ZCL_KSEF_FOUND_*
         ✓ Layer is correct: Foundation
         ✓ Exception handling is correct
         ✗ Missing logging - adding mo_logger"
```

### Step 4: Update Architecture
```
When architecture changes:
1. Update this file
2. Re-upload to Claude Code
3. Claude will follow new guidelines
```

### Best Practices for Claude Code
```
1. Always reference section numbers:
   "Implement section 3.2 - ZCL_KSEF_FOUND_DB_REPOSITORY"

2. Ask Claude to validate:
   "Does this follow the architecture in section 3.3?"

3. Request architecture-compliant generation:
   "Create X following the pattern in section 12"

4. Have Claude check dependencies:
   "What foundation classes does this orchestrator need?"
```

---

## 16. Architecture Validation Checklist

**Before Creating Any New Component:**

- [ ] Is the layer correct? (Infrastructure/Foundation/Orchestrator/UI)
- [ ] Does the naming follow pattern? (ZCL_KSEF_LAYER_COMPONENT)
- [ ] Are exceptions appropriate? (ZCX_KSEF_LAYER_ERROR)
- [ ] Is logging implemented? (mo_logger)
- [ ] Are all dependencies from lower layers?
- [ ] Is documentation complete? ("! comments)
- [ ] Does it follow ABAP 7.50+ syntax?
- [ ] Are method signatures correct? (IMPORTING, EXPORTING, RETURNING, RAISING)
- [ ] Is error handling comprehensive?
- [ ] Are unit tests needed?

---

## Document Maintenance

**Version:** 1.0  
**Date:** 2026-01-23  
**Status:** Active  
**Owner:** Development Team

**Change Log:**
- 2026-01-23: Initial architecture definition
- 2026-01-27: Moved XAdES auth + token management to CERT_MANAGER; API_CLIENT now delegates auth and uses send_invoices_interactive for batch-in-session sending
- 2026-01-29: Major update based on OpenAPI v2 spec research:
  - Added session management strategy (reuse sessions, 12h validity, ZKSEF_SESSION table)
  - Added invoice status checking (GET /sessions/{ref}/invoices/{invoiceRef}, status codes 100-550)
  - Added UPO retrieval (3 methods: by invoice ref, by KSeF number, session bulk)
  - Clarified auth session vs interactive session separation
  - Redesigned logging: ZIF_KSEF_LOG_MANAGER interface, BAL-based, KSEFOUT object with MAIN/OVERALL/XML/API/UPO subobjects
  - Updated ZKSEF_OUT table: added session_reference, invoice_reference, api_status_code, send_attempts, log handles
  - Added ZKSEF_SESSION table for session tracking
  - Updated API client with session management, status polling, UPO methods
  - Updated orchestrator with retry logic, status update, UPO retrieval
  - Detailed cockpit button flows: Create XML, Send Interactive, Update Status, Get UPO
  - Added API rate limits reference
  - See KSEF_API_FINDINGS.md for detailed research

---

**END OF ARCHITECTURE DOCUMENT**

This architecture is MANDATORY for all KSeF integration development.
Claude Code must strictly follow these guidelines when generating code.
