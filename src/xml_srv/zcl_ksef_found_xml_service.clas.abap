CLASS zcl_ksef_found_xml_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS create_and_validate_xmls
      IMPORTING it_ksef_ids       TYPE zkstg_t_inv_key
                io_logger         TYPE REF TO zif_ksef_log_manager
      RETURNING VALUE(rt_results) TYPE zif_ksef_xml_types=>tt_xml_result
      RAISING   zcx_ksef_xml_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

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
      DATA(lv_has_error) = xsdbool( line_exists( lt_messages[ severity = 'E' ] ) ).
      DATA(lv_status) = COND char1( WHEN lv_has_error = abap_true THEN 'F' ELSE 'S' ).

      IF io_logger IS BOUND.
        DATA(lv_log_handle) = io_logger->get_log(
          iv_subobject   = 'XML'
          iv_external_id = CONV #( <ls_repo_invoice>-ksef_id ) ).

        LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
          io_logger->add_msg(
            iv_log_handle = lv_log_handle
            iv_type       = <ls_message>-severity
            iv_text       = <ls_message>-text ).
        ENDLOOP.

        io_logger->save( ).
      ENDIF.

      APPEND VALUE #( ksef_id = <ls_repo_invoice>-ksef_id
                      xml_string = lv_xml
                      status = lv_status
                      messages = lt_messages ) TO rt_results.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
