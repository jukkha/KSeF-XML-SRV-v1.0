CLASS zcl_ksef_found_xml_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_xml_request,
        ksef_id TYPE zlx_ksef_id,
      END OF ty_xml_request,
      tt_xml_request TYPE STANDARD TABLE OF ty_xml_request WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_xml_result,
        ksef_id    TYPE zlx_ksef_id,
        xml_string TYPE string,
        status     TYPE char1,
        messages   TYPE zkstg_t_message,
      END OF ty_xml_result,
      tt_xml_result TYPE STANDARD TABLE OF ty_xml_result WITH KEY ksef_id.

    METHODS constructor.

    METHODS create_and_validate_xmls
      IMPORTING it_ksef_ids TYPE zkstg_t_inv_key
                io_logger   TYPE REF TO zif_ksef_log_manager
      RETURNING VALUE(rt_results) TYPE tt_xml_result
      RAISING   zcx_ksef_xml_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_invoice_header,
        ksef_id TYPE zlx_ksef_id,
      END OF ty_invoice_header.

    TYPES:
      BEGIN OF ty_invoice_item,
        item_no TYPE numc6,
      END OF ty_invoice_item,
      tt_invoice_items TYPE STANDARD TABLE OF ty_invoice_item WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_podmiot,
        role TYPE string,
      END OF ty_podmiot,
      tt_podmiot TYPE STANDARD TABLE OF ty_podmiot WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_zal_item,
        item_no TYPE numc6,
      END OF ty_zal_item,
      tt_zal_items TYPE STANDARD TABLE OF ty_zal_item WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_correction_context,
        xml_old TYPE string,
      END OF ty_correction_context.

    TYPES:
      BEGIN OF ty_invoice,
        header             TYPE ty_invoice_header,
        podmiot1           TYPE ty_podmiot,
        podmiot2           TYPE ty_podmiot,
        podmiot3           TYPE tt_podmiot,
        items              TYPE tt_invoice_items,
        zal_items           TYPE tt_zal_items,
        correction_context TYPE ty_correction_context,
      END OF ty_invoice.

    TYPES:
      BEGIN OF ty_repo_invoice,
        ksef_id TYPE zlx_ksef_id,
      END OF ty_repo_invoice,
      tt_repo_invoices TYPE STANDARD TABLE OF ty_repo_invoice WITH EMPTY KEY.

    CLASS lcl_repository DEFINITION DEFERRED.
    CLASS lcl_invoice_assembler DEFINITION DEFERRED.
    CLASS lcl_xml_reader DEFINITION DEFERRED.
    CLASS lcl_diff_engine DEFINITION DEFERRED.
    CLASS lcl_correction_builder DEFINITION DEFERRED.
    CLASS lcl_renderer DEFINITION DEFERRED.
    CLASS lcl_validator DEFINITION DEFERRED.

    DATA mo_repository         TYPE REF TO lcl_repository.
    DATA mo_assembler          TYPE REF TO lcl_invoice_assembler.
    DATA mo_xml_reader         TYPE REF TO lcl_xml_reader.
    DATA mo_diff_engine        TYPE REF TO lcl_diff_engine.
    DATA mo_correction_builder TYPE REF TO lcl_correction_builder.
    DATA mo_renderer           TYPE REF TO lcl_renderer.
    DATA mo_validator          TYPE REF TO lcl_validator.

ENDCLASS.

CLASS zcl_ksef_found_xml_service IMPLEMENTATION.

  METHOD constructor.
    mo_repository = NEW lcl_repository( ).
    mo_assembler = NEW lcl_invoice_assembler( ).
    mo_xml_reader = NEW lcl_xml_reader( ).
    mo_diff_engine = NEW lcl_diff_engine( ).
    mo_correction_builder = NEW lcl_correction_builder( ).
    mo_renderer = NEW lcl_renderer( ).
    mo_validator = NEW lcl_validator( ).
  ENDMETHOD.

  METHOD create_and_validate_xmls.
    DATA(lt_repo_invoices) = mo_repository->read_batch( it_ksef_ids ).
    LOOP AT lt_repo_invoices ASSIGNING FIELD-SYMBOL(<ls_repo_invoice>).
      DATA(ls_invoice) = mo_assembler->assemble( <ls_repo_invoice> ).
      DATA(lv_xml) = mo_renderer->render( ls_invoice ).
      DATA(lt_messages) = mo_validator->validate( lv_xml ).
      APPEND VALUE #( ksef_id = <ls_repo_invoice>-ksef_id
                      xml_string = lv_xml
                      status = space
                      messages = lt_messages ) TO rt_results.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_repository DEFINITION.
  PUBLIC SECTION.
    METHODS read_batch
      IMPORTING it_ksef_ids TYPE zkstg_t_inv_key
      RETURNING VALUE(rt_invoices) TYPE zcl_ksef_found_xml_service=>tt_repo_invoices.
ENDCLASS.

CLASS lcl_repository IMPLEMENTATION.
  METHOD read_batch.
    CLEAR rt_invoices.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_invoice_assembler DEFINITION.
  PUBLIC SECTION.
    METHODS assemble
      IMPORTING is_repo_invoice TYPE zcl_ksef_found_xml_service=>ty_repo_invoice
      RETURNING VALUE(rs_invoice) TYPE zcl_ksef_found_xml_service=>ty_invoice.
ENDCLASS.

CLASS lcl_invoice_assembler IMPLEMENTATION.
  METHOD assemble.
    CLEAR rs_invoice.
    rs_invoice-header-ksef_id = is_repo_invoice-ksef_id.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_xml_reader DEFINITION.
  PUBLIC SECTION.
    METHODS read_podmiot
      IMPORTING iv_xml     TYPE string
                iv_tagname TYPE string
      RETURNING VALUE(rs_podmiot) TYPE zcl_ksef_found_xml_service=>ty_podmiot.

    METHODS read_podmiot3_list
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_podmiot) TYPE zcl_ksef_found_xml_service=>tt_podmiot.

    METHODS read_items
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_items) TYPE zcl_ksef_found_xml_service=>tt_invoice_items.

    METHODS read_zal_items
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_items) TYPE zcl_ksef_found_xml_service=>tt_zal_items.

    METHODS read_simple_tag
      IMPORTING iv_xml     TYPE string
                iv_tagname TYPE string
      RETURNING VALUE(rv_value) TYPE string.
ENDCLASS.

CLASS lcl_xml_reader IMPLEMENTATION.
  METHOD read_podmiot.
    CLEAR rs_podmiot.
  ENDMETHOD.

  METHOD read_podmiot3_list.
    CLEAR rt_podmiot.
  ENDMETHOD.

  METHOD read_items.
    CLEAR rt_items.
  ENDMETHOD.

  METHOD read_zal_items.
    CLEAR rt_items.
  ENDMETHOD.

  METHOD read_simple_tag.
    CLEAR rv_value.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_diff_engine DEFINITION.
  PUBLIC SECTION.
    METHODS diff_podmiot
      IMPORTING is_old TYPE zcl_ksef_found_xml_service=>ty_podmiot
                is_new TYPE zcl_ksef_found_xml_service=>ty_podmiot
      RETURNING VALUE(rv_diff) TYPE abap_bool.

    METHODS diff_items
      IMPORTING it_old TYPE zcl_ksef_found_xml_service=>tt_invoice_items
                it_new TYPE zcl_ksef_found_xml_service=>tt_invoice_items
      RETURNING VALUE(rv_diff) TYPE abap_bool.

    METHODS diff_zal_items
      IMPORTING it_old TYPE zcl_ksef_found_xml_service=>tt_zal_items
                it_new TYPE zcl_ksef_found_xml_service=>tt_zal_items
      RETURNING VALUE(rv_diff) TYPE abap_bool.
ENDCLASS.

CLASS lcl_diff_engine IMPLEMENTATION.
  METHOD diff_podmiot.
    rv_diff = abap_false.
  ENDMETHOD.

  METHOD diff_items.
    rv_diff = abap_false.
  ENDMETHOD.

  METHOD diff_zal_items.
    rv_diff = abap_false.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_correction_builder DEFINITION.
  PUBLIC SECTION.
    METHODS build
      IMPORTING is_old TYPE zcl_ksef_found_xml_service=>ty_invoice
                is_new TYPE zcl_ksef_found_xml_service=>ty_invoice
      RETURNING VALUE(rs_invoice) TYPE zcl_ksef_found_xml_service=>ty_invoice.
ENDCLASS.

CLASS lcl_correction_builder IMPLEMENTATION.
  METHOD build.
    rs_invoice = is_new.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_renderer DEFINITION.
  PUBLIC SECTION.
    METHODS render
      IMPORTING is_invoice TYPE zcl_ksef_found_xml_service=>ty_invoice
      RETURNING VALUE(rv_xml) TYPE string.
ENDCLASS.

CLASS lcl_renderer IMPLEMENTATION.
  METHOD render.
    CLEAR rv_xml.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_validator DEFINITION.
  PUBLIC SECTION.
    METHODS validate
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_messages) TYPE zkstg_t_message.
ENDCLASS.

CLASS lcl_validator IMPLEMENTATION.
  METHOD validate.
    CLEAR rt_messages.
  ENDMETHOD.
ENDCLASS.
