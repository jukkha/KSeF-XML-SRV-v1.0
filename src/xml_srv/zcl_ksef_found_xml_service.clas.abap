CLASS zcl_ksef_found_xml_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor.

    METHODS create_xml
      IMPORTING iv_ksef_id       TYPE zlx_ksef_id
                io_logger        TYPE REF TO zif_ksef_log_manager OPTIONAL
      RETURNING VALUE(rs_result) TYPE zif_ksef_xml_types=>ty_xml_result
      RAISING   zcx_ksef_xml_error.

    METHODS create_and_validate_xmls
      IMPORTING it_ksef_ids       TYPE zkstg_t_inv_key
                io_logger         TYPE REF TO zif_ksef_log_manager
      RETURNING VALUE(rt_results) TYPE zif_ksef_xml_types=>tt_xml_result
      RAISING   zcx_ksef_xml_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_processing_mode TYPE c LENGTH 10.

    CONSTANTS:
      gc_processing_normal     TYPE ty_processing_mode VALUE 'NORMAL',
      gc_processing_correction TYPE ty_processing_mode VALUE 'CORRECTION'.

    DATA mo_repository         TYPE REF TO zcl_ksef_found_xml_repository.
    DATA mo_assembler          TYPE REF TO zcl_ksef_found_xml_assembler.
    DATA mo_xml_reader         TYPE REF TO zcl_ksef_found_xml_reader.
    DATA mo_diff_engine        TYPE REF TO zcl_ksef_found_xml_diff.
    DATA mo_correction_builder TYPE REF TO zcl_ksef_found_xml_cor_builder.
    DATA mo_renderer           TYPE REF TO zcl_ksef_found_xml_renderer.
    DATA mo_validator          TYPE REF TO zcl_ksef_found_xml_validator.

    METHODS process_repo_invoice
      IMPORTING is_repo_invoice  TYPE zif_ksef_xml_types=>ty_repo_invoice
                io_logger        TYPE REF TO zif_ksef_log_manager OPTIONAL
      RETURNING VALUE(rs_result) TYPE zif_ksef_xml_types=>ty_xml_result.

    METHODS build_old_invoice
      IMPORTING iv_xml_old        TYPE string
                is_header         TYPE zif_ksef_xml_types=>ty_invoice_header
      RETURNING VALUE(rs_invoice) TYPE zif_ksef_xml_types=>ty_invoice.

    METHODS determine_processing_mode
      IMPORTING is_invoice     TYPE zif_ksef_xml_types=>ty_invoice
      RETURNING VALUE(rv_mode) TYPE ty_processing_mode.

    METHODS append_message
      IMPORTING iv_severity TYPE symsgty
                iv_code     TYPE string
                iv_text     TYPE string
                iv_ksef_id  TYPE zlx_ksef_id OPTIONAL
      CHANGING  ct_messages TYPE zif_ksef_xml_types=>tt_message.

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

  METHOD create_xml.
    DATA lt_ksef_ids TYPE zkstg_t_inv_key.
    APPEND VALUE #( ksef_id = iv_ksef_id ) TO lt_ksef_ids.

    DATA(lt_results) = me->create_and_validate_xmls(
      EXPORTING
        it_ksef_ids = lt_ksef_ids
        io_logger   = io_logger ).

    READ TABLE lt_results INTO rs_result WITH KEY ksef_id = iv_ksef_id.
    IF sy-subrc <> 0.
      CLEAR rs_result.
      rs_result-ksef_id = iv_ksef_id.
      rs_result-status = 'E'.
      me->append_message(
        EXPORTING
          iv_severity = 'E'
          iv_code     = 'XML_RESULT_MISSING'
          iv_text     = 'No result produced for requested KSeF ID.'
          iv_ksef_id  = iv_ksef_id
        CHANGING
          ct_messages = rs_result-messages ).
    ENDIF.
  ENDMETHOD.

  METHOD create_and_validate_xmls.
    CLEAR rt_results.

    IF it_ksef_ids IS INITIAL.
      RETURN.
    ENDIF.

    " Partial success behavior: each ksef_id is processed independently and returned with its own result.
    DATA(lt_repo_invoices) = mo_repository->read_batch(
      EXPORTING
        it_ksef_ids = it_ksef_ids ).

    LOOP AT it_ksef_ids INTO DATA(ls_ksef_id).
      DATA(ls_result) = VALUE zif_ksef_xml_types=>ty_xml_result( ).

      TRY.
          READ TABLE lt_repo_invoices INTO DATA(ls_repo_invoice) WITH KEY ksef_id = ls_ksef_id-ksef_id.
          IF sy-subrc <> 0.
            ls_result-ksef_id = ls_ksef_id-ksef_id.
            ls_result-status = 'E'.
            me->append_message(
              EXPORTING
                iv_severity = 'E'
                iv_code     = 'XML_REPO_MISSING'
                iv_text     = 'Repository did not return data for KSeF ID.'
                iv_ksef_id  = ls_ksef_id-ksef_id
              CHANGING
                ct_messages = ls_result-messages ).
            IF io_logger IS BOUND.
              DATA(lv_log_overall) = io_logger->get_log(
                iv_subobject   = 'OVERALL'
                iv_external_id = CONV #( ls_ksef_id-ksef_id ) ).
              DATA(lv_log_xml) = io_logger->get_log(
                iv_subobject   = 'XML'
                iv_external_id = CONV #( ls_ksef_id-ksef_id ) ).
              io_logger->add_msg(
                iv_log_handle = lv_log_overall
                iv_type       = 'E'
                iv_text       = 'XML creation failed: repository data missing.' ).
              io_logger->add_msg(
                iv_log_handle = lv_log_xml
                iv_type       = 'E'
                iv_text       = 'Repository did not return data for requested KSeF ID.' ).
              io_logger->save( ).
            ENDIF.
          ELSE.
            ls_result = me->process_repo_invoice(
              EXPORTING
                is_repo_invoice = ls_repo_invoice
                io_logger       = io_logger ).
          ENDIF.
        CATCH zcx_ksef_xml_error INTO DATA(lx_xml).
          ls_result-ksef_id = ls_ksef_id-ksef_id.
          ls_result-status = 'E'.
          me->append_message(
            EXPORTING
              iv_severity = 'E'
              iv_code     = 'XML_PROCESS_ERROR'
              iv_text     = lx_xml->get_text( )
              iv_ksef_id  = ls_ksef_id-ksef_id
            CHANGING
              ct_messages = ls_result-messages ).
          IF io_logger IS BOUND.
            DATA(lv_log_overall_exc) = io_logger->get_log(
              iv_subobject   = 'OVERALL'
              iv_external_id = CONV #( ls_ksef_id-ksef_id ) ).
            DATA(lv_log_xml_exc) = io_logger->get_log(
              iv_subobject   = 'XML'
              iv_external_id = CONV #( ls_ksef_id-ksef_id ) ).
            io_logger->add_msg(
              iv_log_handle = lv_log_overall_exc
              iv_type       = 'E'
              iv_text       = lx_xml->get_text( ) ).
            io_logger->add_msg(
              iv_log_handle = lv_log_xml_exc
              iv_type       = 'E'
              iv_text       = lx_xml->get_text( ) ).
            io_logger->save( ).
          ENDIF.
        CATCH cx_root INTO DATA(lx_root).
          ls_result-ksef_id = ls_ksef_id-ksef_id.
          ls_result-status = 'E'.
          me->append_message(
            EXPORTING
              iv_severity = 'E'
              iv_code     = 'XML_PROCESS_ERROR'
              iv_text     = lx_root->get_text( )
              iv_ksef_id  = ls_ksef_id-ksef_id
            CHANGING
              ct_messages = ls_result-messages ).
          IF io_logger IS BOUND.
            DATA(lv_log_overall_root) = io_logger->get_log(
              iv_subobject   = 'OVERALL'
              iv_external_id = CONV #( ls_ksef_id-ksef_id ) ).
            DATA(lv_log_xml_root) = io_logger->get_log(
              iv_subobject   = 'XML'
              iv_external_id = CONV #( ls_ksef_id-ksef_id ) ).
            io_logger->add_msg(
              iv_log_handle = lv_log_overall_root
              iv_type       = 'E'
              iv_text       = lx_root->get_text( ) ).
            io_logger->add_msg(
              iv_log_handle = lv_log_xml_root
              iv_type       = 'E'
              iv_text       = lx_root->get_text( ) ).
            io_logger->save( ).
          ENDIF.
      ENDTRY.

      APPEND ls_result TO rt_results.
    ENDLOOP.
  ENDMETHOD.

  METHOD process_repo_invoice.
    CLEAR rs_result.
    rs_result-ksef_id = is_repo_invoice-ksef_id.

    DATA lv_log_overall TYPE balloghndl.
    DATA lv_log_xml TYPE balloghndl.

    IF io_logger IS BOUND.
      lv_log_overall = io_logger->get_log(
        iv_subobject   = 'OVERALL'
        iv_external_id = CONV #( is_repo_invoice-ksef_id ) ).
      lv_log_xml = io_logger->get_log(
        iv_subobject   = 'XML'
        iv_external_id = CONV #( is_repo_invoice-ksef_id ) ).

      io_logger->add_msg(
        iv_log_handle = lv_log_overall
        iv_type       = 'I'
        iv_text       = 'XML creation started.' ).
      io_logger->add_msg(
        iv_log_handle = lv_log_xml
        iv_type       = 'I'
        iv_text       = 'Repository data loaded.' ).
    ENDIF.

    DATA(lv_is_correction) = abap_false.
    DATA(lv_has_diff) = abap_false.
    DATA(lt_messages) = VALUE zif_ksef_xml_types=>tt_message( ).

    TRY.
        DATA(ls_invoice) = mo_assembler->assemble(
          EXPORTING
            is_repo_invoice = is_repo_invoice ).
        DATA(lv_xml) TYPE string.
        DATA(ls_render_invoice) TYPE zif_ksef_xml_types=>ty_invoice.

        IF io_logger IS BOUND.
          io_logger->add_msg(
            iv_log_handle = lv_log_xml
            iv_type       = 'I'
            iv_text       = 'Invoice assembled.' ).
        ENDIF.

        DATA(lv_mode) = me->determine_processing_mode(
          EXPORTING
            is_invoice = ls_invoice ).

        IF lv_mode = gc_processing_correction.
          lv_is_correction = abap_true.
          IF io_logger IS BOUND.
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'I'
              iv_text       = 'Processing mode: CORRECTION.' ).
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'I'
              iv_text       = 'Correction step: build old XML snapshot.' ).
          ENDIF.

          DATA(ls_old_invoice) = me->build_old_invoice(
            EXPORTING
              iv_xml_old = ls_invoice-correction_context-xml_old
              is_header  = ls_invoice-header ).

          IF io_logger IS BOUND.
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'I'
              iv_text       = 'Correction step: diff old vs new invoice.' ).
          ENDIF.

          DATA(ls_diff) = mo_diff_engine->diff_invoice(
            EXPORTING
              is_old = ls_old_invoice
              is_new = ls_invoice ).

          lv_has_diff = xsdbool(
            ls_diff-changed_items = abap_true
            OR ls_diff-changed_zal_items = abap_true
            OR ls_diff-changed_parties = abap_true
            OR ls_diff-changed_totals = abap_true ).

          IF io_logger IS BOUND.
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'I'
              iv_text       = COND string( WHEN lv_has_diff = abap_true
                                            THEN 'Correction step: diff detected.'
                                            ELSE 'Correction step: no diff detected.' ) ).
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'I'
              iv_text       = 'Correction step: build correction structures.' ).
          ENDIF.

          ls_render_invoice = mo_correction_builder->build(
            EXPORTING
              is_old = ls_old_invoice
              is_new = ls_invoice ).

          lv_xml = mo_renderer->render(
            EXPORTING
              is_invoice = ls_render_invoice ).
        ELSE.
          IF io_logger IS BOUND.
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'I'
              iv_text       = 'Processing mode: NORMAL.' ).
          ENDIF.

          lv_xml = mo_renderer->render(
            EXPORTING
              is_invoice = ls_invoice ).
        ENDIF.

        IF lv_xml IS INITIAL.
          me->append_message(
            EXPORTING
              iv_severity = 'E'
              iv_code     = 'XML_RENDER_EMPTY'
              iv_text     = 'Rendered XML is empty.'
              iv_ksef_id  = is_repo_invoice-ksef_id
            CHANGING
              ct_messages = lt_messages ).
          IF io_logger IS BOUND.
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'E'
              iv_text       = 'Rendered XML is empty.' ).
          ENDIF.
        ELSE.
          IF io_logger IS BOUND.
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'I'
              iv_text       = 'XML rendered.' ).
            io_logger->add_msg(
              iv_log_handle = lv_log_xml
              iv_type       = 'I'
              iv_text       = 'Validation started.' ).
          ENDIF.

          lt_messages = mo_validator->validate(
            EXPORTING
              iv_xml = lv_xml ).

          LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
            IF <ls_message>-ksef_id IS INITIAL.
              <ls_message>-ksef_id = is_repo_invoice-ksef_id.
            ENDIF.
            IF io_logger IS BOUND.
              io_logger->add_msg(
                iv_log_handle = lv_log_xml
                iv_type       = <ls_message>-severity
                iv_text       = <ls_message>-text ).
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF line_exists( lt_messages[ severity = 'E' ] ).
          rs_result-status = 'E'.
        ELSEIF lv_xml IS INITIAL.
          rs_result-status = 'E'.
        ELSE.
          rs_result-status = 'S'.
          rs_result-xml_string = lv_xml.
        ENDIF.

      CATCH zcx_ksef_xml_error INTO DATA(lx_xml).
        rs_result-status = 'E'.
        me->append_message(
          EXPORTING
            iv_severity = 'E'
            iv_code     = 'XML_PROCESS_ERROR'
            iv_text     = lx_xml->get_text( )
            iv_ksef_id  = is_repo_invoice-ksef_id
          CHANGING
            ct_messages = lt_messages ).
        IF io_logger IS BOUND.
          io_logger->add_msg(
            iv_log_handle = lv_log_overall
            iv_type       = 'E'
            iv_text       = lx_xml->get_text( ) ).
          io_logger->add_msg(
            iv_log_handle = lv_log_xml
            iv_type       = 'E'
            iv_text       = lx_xml->get_text( ) ).
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
        rs_result-status = 'E'.
        me->append_message(
          EXPORTING
            iv_severity = 'E'
            iv_code     = 'XML_PROCESS_ERROR'
            iv_text     = lx_root->get_text( )
            iv_ksef_id  = is_repo_invoice-ksef_id
          CHANGING
            ct_messages = lt_messages ).
        IF io_logger IS BOUND.
          io_logger->add_msg(
            iv_log_handle = lv_log_overall
            iv_type       = 'E'
            iv_text       = lx_root->get_text( ) ).
          io_logger->add_msg(
            iv_log_handle = lv_log_xml
            iv_type       = 'E'
            iv_text       = lx_root->get_text( ) ).
        ENDIF.
    ENDTRY.

    rs_result-messages = lt_messages.
    rs_result-is_correction = lv_is_correction.
    rs_result-has_diff = lv_has_diff.

    IF rs_result-status IS INITIAL.
      rs_result-status = 'E'.
      me->append_message(
        EXPORTING
          iv_severity = 'E'
          iv_code     = 'XML_PROCESS_FAILED'
          iv_text     = 'XML processing finished without a final status.'
          iv_ksef_id  = is_repo_invoice-ksef_id
        CHANGING
          ct_messages = rs_result-messages ).
    ENDIF.

    IF io_logger IS BOUND.
      io_logger->add_msg(
        iv_log_handle = lv_log_overall
        iv_type       = COND #( WHEN rs_result-status = 'S' THEN 'S' ELSE 'E' )
        iv_text       = COND string( WHEN rs_result-status = 'S'
                                     THEN 'XML creation finished successfully.'
                                     ELSE 'XML creation finished with errors.' ) ).
      io_logger->save( ).
    ENDIF.
  ENDMETHOD.

  METHOD build_old_invoice.
    CLEAR rs_invoice.
    rs_invoice-header = is_header.
    rs_invoice-podmiot1 = mo_xml_reader->read_podmiot(
      EXPORTING
        iv_xml     = iv_xml_old
        iv_tagname = 'Podmiot1' ).
    rs_invoice-podmiot2 = mo_xml_reader->read_podmiot(
      EXPORTING
        iv_xml     = iv_xml_old
        iv_tagname = 'Podmiot2' ).
    rs_invoice-podmiot3 = mo_xml_reader->read_podmiot3_list(
      EXPORTING
        iv_xml = iv_xml_old ).
    rs_invoice-items = mo_xml_reader->read_items(
      EXPORTING
        iv_xml = iv_xml_old ).
    rs_invoice-zal_items = mo_xml_reader->read_zal_items(
      EXPORTING
        iv_xml = iv_xml_old ).
  ENDMETHOD.

  METHOD determine_processing_mode.
    rv_mode = gc_processing_normal.
    IF is_invoice-correction_context-xml_old IS NOT INITIAL.
      rv_mode = gc_processing_correction.
    ENDIF.
  ENDMETHOD.

  METHOD append_message.
    APPEND VALUE zif_ksef_xml_types=>ty_message(
      severity = iv_severity
      code     = iv_code
      text     = iv_text
      ksef_id  = iv_ksef_id ) TO ct_messages.
  ENDMETHOD.

ENDCLASS.
