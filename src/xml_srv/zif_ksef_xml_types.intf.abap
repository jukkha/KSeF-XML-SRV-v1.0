INTERFACE zif_ksef_xml_types
  PUBLIC .

  TYPES:
    BEGIN OF ty_xml_request,
      ksef_id TYPE zlx_ksef_id,
    END OF ty_xml_request,
    tt_xml_request TYPE STANDARD TABLE OF ty_xml_request WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_message,
      severity   TYPE symsgty,
      code       TYPE string,
      text       TYPE string,
      ksef_id    TYPE zlx_ksef_id,
      item_no    TYPE string,
      field_name TYPE string,
    END OF ty_message,
    tt_message TYPE STANDARD TABLE OF ty_message WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_xml_result,
      ksef_id    TYPE zlx_ksef_id,
      xml_string TYPE string,
      status     TYPE char1,
      messages   TYPE tt_message,
    END OF ty_xml_result,
    tt_xml_result TYPE STANDARD TABLE OF ty_xml_result WITH KEY ksef_id.

  TYPES:
    ty_diff_mode TYPE c LENGTH 11.

  CONSTANTS:
    gc_diff_mode_full        TYPE ty_diff_mode VALUE 'FULL',
    gc_diff_mode_totals_only TYPE ty_diff_mode VALUE 'TOTALS_ONLY'.

  TYPES:
    ty_diff_key  TYPE string,
    tt_diff_keys TYPE STANDARD TABLE OF ty_diff_key WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_diff_result,
      changed_items         TYPE abap_bool,
      changed_zal_items     TYPE abap_bool,
      changed_parties       TYPE abap_bool,
      changed_podmiot1      TYPE abap_bool,
      changed_podmiot2      TYPE abap_bool,
      changed_podmiot3      TYPE abap_bool,
      changed_amounts       TYPE abap_bool,
      changed_totals        TYPE abap_bool,
      changed_item_keys     TYPE tt_diff_keys,
      changed_zal_item_keys TYPE tt_diff_keys,
      changed_podmiot3_keys TYPE tt_diff_keys,
    END OF ty_diff_result.

  TYPES:
    BEGIN OF ty_invoice_header,
      ksef_id TYPE zlx_ksef_id,
    END OF ty_invoice_header.

  TYPES:
    ty_invoice_item  TYPE zksef_s_item,
    tt_invoice_items TYPE STANDARD TABLE OF ty_invoice_item WITH EMPTY KEY.

  TYPES:
    ty_podmiot TYPE zlx_ksef_podmiot,
    tt_podmiot TYPE STANDARD TABLE OF ty_podmiot WITH EMPTY KEY.

  TYPES:
    ty_zal_item  TYPE zlx_ksef_zal_items,
    tt_zal_items TYPE STANDARD TABLE OF ty_zal_item WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_correction_context,
      xml_old TYPE string,
    END OF ty_correction_context.

  TYPES:
    BEGIN OF ty_invoice,
      header             TYPE ty_invoice_header,
      podmiot1           TYPE ty_podmiot,
      podmiot1k          TYPE ty_podmiot,
      podmiot2           TYPE ty_podmiot,
      podmiot2k          TYPE tt_podmiot,
      podmiot3           TYPE tt_podmiot,
      items              TYPE tt_invoice_items,
      zal_items          TYPE tt_zal_items,
      correction_context TYPE ty_correction_context,
    END OF ty_invoice.

  TYPES:
    BEGIN OF ty_repo_invoice,
      ksef_id TYPE zlx_ksef_id,
    END OF ty_repo_invoice,
    tt_repo_invoices TYPE STANDARD TABLE OF ty_repo_invoice WITH EMPTY KEY.

ENDINTERFACE.