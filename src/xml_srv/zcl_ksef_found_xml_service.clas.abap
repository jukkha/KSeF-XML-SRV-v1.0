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
        item_no TYPE posnr,
      END OF ty_invoice_item,
      tt_invoice_items TYPE STANDARD TABLE OF ty_invoice_item WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_podmiot,
        role TYPE string,
      END OF ty_podmiot,
      tt_podmiot TYPE STANDARD TABLE OF ty_podmiot WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_zal_item,
        item_no TYPE posnr,
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

    DATA mo_repository         TYPE REF TO zcl_ksef_found_xml_repository.
    DATA mo_assembler          TYPE REF TO zcl_ksef_found_xml_assembler.
    DATA mo_xml_reader         TYPE REF TO zcl_ksef_found_xml_reader.
    DATA mo_diff_engine        TYPE REF TO zcl_ksef_found_xml_diff.
    DATA mo_correction_builder TYPE REF TO zcl_ksef_found_xml_cor_builder.
    DATA mo_renderer           TYPE REF TO zcl_ksef_found_xml_renderer.
    DATA mo_validator          TYPE REF TO zcl_ksef_found_xml_validator.

ENDCLASS.

CLASS zcl_ksef_found_xml_service IMPLEMENTATION.

  METHOD constructor.
    mo_repository = NEW zcl_ksef_found_xml_repository( ).
    mo_assembler = NEW zcl_ksef_found_xml_assembler( ).
    mo_xml_reader = NEW zcl_ksef_found_xml_reader( ).
    mo_diff_engine = NEW zcl_ksef_found_xml_diff( ).
    mo_correction_builder = NEW zcl_ksef_found_xml_cor_builder( ).
    mo_renderer = NEW zcl_ksef_found_xml_renderer( ).
    mo_validator = NEW zcl_ksef_found_xml_validator( ).
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
