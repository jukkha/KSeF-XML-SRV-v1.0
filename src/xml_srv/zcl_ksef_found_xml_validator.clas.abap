CLASS zcl_ksef_found_xml_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS validate
      IMPORTING iv_xml             TYPE string
      RETURNING VALUE(rt_messages) TYPE zif_ksef_xml_types=>tt_message.

  PRIVATE SECTION.
    CONSTANTS:
      gc_msg_id_header_missing   TYPE string VALUE 'BUS_HEAD_MANDATORY',
      gc_msg_id_party_missing    TYPE string VALUE 'BUS_PARTY_MANDATORY',
      gc_msg_id_party_incomplete TYPE string VALUE 'BUS_PARTY_INCOMPLETE',
      gc_msg_id_item_missing     TYPE string VALUE 'BUS_ITEM_MANDATORY',
      gc_msg_id_item_invalid     TYPE string VALUE 'BUS_ITEM_INVALID',
      gc_msg_id_total_missing    TYPE string VALUE 'BUS_TOTAL_MANDATORY',
      gc_msg_id_total_invalid    TYPE string VALUE 'BUS_TOTAL_INVALID',
      gc_msg_id_correction       TYPE string VALUE 'BUS_CORR_MANDATORY'.

    METHODS add_message
      IMPORTING iv_severity   TYPE symsgty
                iv_code       TYPE string
                iv_text       TYPE string
                iv_ksef_id    TYPE zlx_ksef_id OPTIONAL
                iv_item_no    TYPE string OPTIONAL
                iv_field_name TYPE string OPTIONAL
      CHANGING  ct_messages   TYPE zif_ksef_xml_types=>tt_message.

    METHODS is_podmiot_initial
      IMPORTING is_podmiot           TYPE zif_ksef_xml_types=>ty_podmiot
      RETURNING VALUE(rv_is_initial) TYPE abap_bool.

    METHODS has_party_id
      IMPORTING is_podmiot       TYPE zif_ksef_xml_types=>ty_podmiot
      RETURNING VALUE(rv_has_id) TYPE abap_bool.

    METHODS is_podmiot3_initial
      IMPORTING is_podmiot3           TYPE zif_ksef_xml_types=>ty_podmiot3
      RETURNING VALUE(rv_is_initial) TYPE abap_bool.

    METHODS has_podmiot3_id
      IMPORTING is_podmiot3       TYPE zif_ksef_xml_types=>ty_podmiot3
      RETURNING VALUE(rv_has_id) TYPE abap_bool.

    METHODS is_amount_valid
      IMPORTING iv_value        TYPE string
      RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS get_item_context
      IMPORTING is_item           TYPE zif_ksef_xml_types=>ty_invoice_item
                iv_index          TYPE i
      RETURNING VALUE(rv_item_no) TYPE string.
ENDCLASS.

CLASS zcl_ksef_found_xml_validator IMPLEMENTATION.
  METHOD validate.
    CLEAR rt_messages.

    DATA(lo_reader) = NEW zcl_ksef_found_xml_reader( ).
    DATA(lo_diff) = NEW zcl_ksef_found_xml_diff( ).

    DATA(lv_doc_type) = lo_reader->read_simple_tag( iv_xml = iv_xml iv_tagname = 'RodzajFaktury' ).
    DATA(lv_currency) = lo_reader->read_simple_tag( iv_xml = iv_xml iv_tagname = 'KodWaluty' ).
    DATA(lv_issue_date) = lo_reader->read_simple_tag( iv_xml = iv_xml iv_tagname = 'P_1' ).
    DATA(lv_invoice_no) = lo_reader->read_simple_tag( iv_xml = iv_xml iv_tagname = 'P_2' ).
    DATA(lv_generation_ts) = lo_reader->read_simple_tag( iv_xml = iv_xml iv_tagname = 'DataWytworzeniaFa' ).

    IF lv_doc_type IS INITIAL.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_header_missing
          iv_text       = 'Missing mandatory header field: RodzajFaktury.'
          iv_field_name = 'RodzajFaktury'
        CHANGING
          ct_messages   = rt_messages ).
    ENDIF.

    IF lv_currency IS INITIAL.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_header_missing
          iv_text       = 'Missing mandatory header field: KodWaluty.'
          iv_field_name = 'KodWaluty'
        CHANGING
          ct_messages   = rt_messages ).
    ENDIF.

    IF lv_issue_date IS INITIAL.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_header_missing
          iv_text       = 'Missing mandatory header field: P_1 (issue date).'
          iv_field_name = 'P_1'
        CHANGING
          ct_messages   = rt_messages ).
    ENDIF.

    IF lv_invoice_no IS INITIAL.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_header_missing
          iv_text       = 'Missing mandatory header field: P_2 (invoice number).'
          iv_field_name = 'P_2'
        CHANGING
          ct_messages   = rt_messages ).
    ENDIF.

    IF lv_generation_ts IS INITIAL.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_header_missing
          iv_text       = 'Missing mandatory header field: DataWytworzeniaFa.'
          iv_field_name = 'DataWytworzeniaFa'
        CHANGING
          ct_messages   = rt_messages ).
    ENDIF.

    DATA(ls_podmiot1) = lo_reader->read_podmiot( iv_xml = iv_xml iv_tagname = 'Podmiot1' ).
    DATA(ls_podmiot2) = lo_reader->read_podmiot( iv_xml = iv_xml iv_tagname = 'Podmiot2' ).
    DATA(lt_podmiot3) = lo_reader->read_podmiot3_list( iv_xml = iv_xml ).

    IF me->is_podmiot_initial( ls_podmiot1 ) = abap_true.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_party_missing
          iv_text       = 'Missing mandatory party section: Podmiot1.'
          iv_field_name = 'Podmiot1'
        CHANGING
          ct_messages   = rt_messages ).
    ELSE.
      IF ls_podmiot1-nip IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_party_incomplete
            iv_text       = 'Podmiot1 requires NIP.'
            iv_field_name = 'Podmiot1/NIP'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.
      IF ls_podmiot1-nazwa IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_party_incomplete
            iv_text       = 'Podmiot1 requires Nazwa.'
            iv_field_name = 'Podmiot1/Nazwa'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.
    ENDIF.

    IF me->is_podmiot_initial( ls_podmiot2 ) = abap_true.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_party_missing
          iv_text       = 'Missing mandatory party section: Podmiot2.'
          iv_field_name = 'Podmiot2'
        CHANGING
          ct_messages   = rt_messages ).
    ELSE.
      IF me->has_party_id( ls_podmiot2 ) = abap_false.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_party_incomplete
            iv_text       = 'Podmiot2 requires at least one identifier (NIP/NrID/NrVatUE/IDNabywcy) unless BrakID is provided.'
            iv_field_name = 'Podmiot2/ID'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.
      IF ls_podmiot2-nazwa IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_party_incomplete
            iv_text       = 'Podmiot2 requires Nazwa.'
            iv_field_name = 'Podmiot2/Nazwa'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.
    ENDIF.

    LOOP AT lt_podmiot3 ASSIGNING FIELD-SYMBOL(<ls_pod3>).
      IF me->has_podmiot3_id( <ls_pod3> ) = abap_false AND <ls_pod3>-nazwa IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'W'
            iv_code       = gc_msg_id_party_incomplete
            iv_text       = 'Podmiot3 provided without identifier or name.'
            iv_field_name = 'Podmiot3'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.
    ENDLOOP.

    DATA(lt_items) = lo_reader->read_items( iv_xml = iv_xml ).
    IF lines( lt_items ) = 0.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_item_missing
          iv_text       = 'Invoice must contain at least one FaWiersz item.'
          iv_field_name = 'FaWiersz'
        CHANGING
          ct_messages   = rt_messages ).
    ENDIF.

    DATA(lv_item_index) = 0.
    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      lv_item_index += 1.
      DATA(lv_item_no) = me->get_item_context( is_item = <ls_item> iv_index = lv_item_index ).

      IF <ls_item>-p_7 IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_item_invalid
            iv_text       = 'Item requires description (P_7).'
            iv_item_no    = lv_item_no
            iv_field_name = 'P_7'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.

      IF <ls_item>-p_8a IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_item_invalid
            iv_text       = 'Item requires quantity (P_8A).'
            iv_item_no    = lv_item_no
            iv_field_name = 'P_8A'
          CHANGING
            ct_messages   = rt_messages ).
      ELSEIF me->is_amount_valid( CONV #( <ls_item>-p_8a ) ) = abap_false.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_item_invalid
            iv_text       = 'Item quantity (P_8A) must be numeric.'
            iv_item_no    = lv_item_no
            iv_field_name = 'P_8A'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.

      IF <ls_item>-p_11 IS INITIAL AND <ls_item>-p_12 IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_item_invalid
            iv_text       = 'Item requires amount (P_11 or P_12).'
            iv_item_no    = lv_item_no
            iv_field_name = 'P_11/P_12'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.

      IF <ls_item>-p_11 IS NOT INITIAL AND me->is_amount_valid( CONV #( <ls_item>-p_11 ) ) = abap_false.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_item_invalid
            iv_text       = 'Item net amount (P_11) must be numeric.'
            iv_item_no    = lv_item_no
            iv_field_name = 'P_11'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.

      IF <ls_item>-p_11vat IS NOT INITIAL AND me->is_amount_valid( CONV #( <ls_item>-p_11vat ) ) = abap_false.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_item_invalid
            iv_text       = 'Item VAT amount (P_11Vat) must be numeric.'
            iv_item_no    = lv_item_no
            iv_field_name = 'P_11Vat'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.

      IF <ls_item>-p_12 IS NOT INITIAL AND me->is_amount_valid( CONV #( <ls_item>-p_12 ) ) = abap_false.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_item_invalid
            iv_text       = 'Item gross amount (P_12) must be numeric.'
            iv_item_no    = lv_item_no
            iv_field_name = 'P_12'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.
    ENDLOOP.

    DATA(lv_total_txt) = lo_reader->read_simple_tag( iv_xml = iv_xml iv_tagname = 'P_15' ).
    IF lv_total_txt IS INITIAL.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_total_missing
          iv_text       = 'Missing required total field: P_15.'
          iv_field_name = 'P_15'
        CHANGING
          ct_messages   = rt_messages ).
    ELSEIF me->is_amount_valid( lv_total_txt ) = abap_false.
      me->add_message(
        EXPORTING
          iv_severity   = 'E'
          iv_code       = gc_msg_id_total_invalid
          iv_text       = 'Total P_15 must be numeric.'
          iv_field_name = 'P_15'
        CHANGING
          ct_messages   = rt_messages ).
    ELSE.
      DATA(lv_total_amount) = lo_diff->normalize_amount( lv_total_txt ).
      DATA(lv_items_total) = CONV decfloat34( 0 ).
      DATA(lv_total_items) = 0.

      LOOP AT lt_items ASSIGNING <ls_item>.
        DATA(lv_item_amount) = CONV decfloat34( 0 ).
        DATA(lv_has_amount) = abap_false.
        IF <ls_item>-p_12 IS NOT INITIAL AND me->is_amount_valid( CONV #( <ls_item>-p_12 ) ) = abap_true.
          lv_item_amount = lo_diff->normalize_amount( CONV #( <ls_item>-p_12 ) ).
          lv_has_amount = abap_true.
        ELSEIF <ls_item>-p_11 IS NOT INITIAL AND me->is_amount_valid( CONV #( <ls_item>-p_11 ) ) = abap_true.
          lv_item_amount = lo_diff->normalize_amount( CONV #( <ls_item>-p_11 ) ).
          lv_has_amount = abap_true.
          IF <ls_item>-p_11vat IS NOT INITIAL AND me->is_amount_valid( CONV #( <ls_item>-p_11vat ) ) = abap_true.
            lv_item_amount += lo_diff->normalize_amount( CONV #( <ls_item>-p_11vat ) ).
          ENDIF.
        ENDIF.

        IF lv_has_amount = abap_true.
          lv_items_total += lv_item_amount.
          lv_total_items += 1.
        ENDIF.
      ENDLOOP.

      IF lv_total_items > 0.
        DATA(lv_delta) = abs( lv_items_total - lv_total_amount ).
        IF lv_delta > zcl_ksef_found_xml_diff=>gc_amount_tolerance_default.
          me->add_message(
            EXPORTING
              iv_severity   = 'E'
              iv_code       = gc_msg_id_total_invalid
              iv_text       = |Total P_15 ({ lv_total_amount }) does not match item totals ({ lv_items_total }).|
              iv_field_name = 'P_15'
            CHANGING
              ct_messages   = rt_messages ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_doc_type = 'KOR'.
      DATA(lv_corr_date) = lo_reader->read_simple_tag( iv_xml = iv_xml iv_tagname = 'DataWystFaKorygowanej' ).
      DATA(lv_corr_no) = lo_reader->read_simple_tag( iv_xml = iv_xml iv_tagname = 'NrFaKorygowanej' ).

      IF lv_corr_date IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_correction
            iv_text       = 'Correction invoice requires DataWystFaKorygowanej.'
            iv_field_name = 'DataWystFaKorygowanej'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.

      IF lv_corr_no IS INITIAL.
        me->add_message(
          EXPORTING
            iv_severity   = 'E'
            iv_code       = gc_msg_id_correction
            iv_text       = 'Correction invoice requires NrFaKorygowanej.'
            iv_field_name = 'NrFaKorygowanej'
          CHANGING
            ct_messages   = rt_messages ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD add_message.
    APPEND VALUE zif_ksef_xml_types=>ty_message(
      severity   = iv_severity
      code       = iv_code
      text       = iv_text
      ksef_id    = iv_ksef_id
      item_no    = iv_item_no
      field_name = iv_field_name ) TO ct_messages.
  ENDMETHOD.

  METHOD is_podmiot_initial.
    rv_is_initial = xsdbool(
      is_podmiot-nip IS INITIAL
      AND is_podmiot-kodue IS INITIAL
      AND is_podmiot-nrvatue IS INITIAL
      AND is_podmiot-nrid IS INITIAL
      AND is_podmiot-brakid IS INITIAL
      AND is_podmiot-nazwa IS INITIAL
      AND is_podmiot-kodkraju IS INITIAL
      AND is_podmiot-adresl1 IS INITIAL
      AND is_podmiot-adresl2 IS INITIAL
      AND is_podmiot-gln IS INITIAL
      AND is_podmiot-idnabywcy IS INITIAL ).
  ENDMETHOD.

  METHOD has_party_id.
    rv_has_id = xsdbool(
      is_podmiot-nip IS NOT INITIAL
      OR is_podmiot-nrid IS NOT INITIAL
      OR is_podmiot-nrvatue IS NOT INITIAL
      OR is_podmiot-idnabywcy IS NOT INITIAL
      OR is_podmiot-brakid IS NOT INITIAL ).
  ENDMETHOD.


  METHOD is_podmiot3_initial.
    rv_is_initial = xsdbool(
      is_podmiot3-nip IS INITIAL
      AND is_podmiot3-kodue IS INITIAL
      AND is_podmiot3-nrvatue IS INITIAL
      AND is_podmiot3-nrid IS INITIAL
      AND is_podmiot3-brakid IS INITIAL
      AND is_podmiot3-nazwa IS INITIAL
      AND is_podmiot3-kodkraju IS INITIAL
      AND is_podmiot3-adr_adresl1 IS INITIAL
      AND is_podmiot3-adr_adresl2 IS INITIAL
      AND is_podmiot3-adr_gln IS INITIAL
      AND is_podmiot3-idnabywcy IS INITIAL ).
  ENDMETHOD.

  METHOD has_podmiot3_id.
    rv_has_id = xsdbool(
      is_podmiot3-nip IS NOT INITIAL
      OR is_podmiot3-nrid IS NOT INITIAL
      OR is_podmiot3-nrvatue IS NOT INITIAL
      OR is_podmiot3-idnabywcy IS NOT INITIAL
      OR is_podmiot3-brakid IS NOT INITIAL ).
  ENDMETHOD.

  METHOD is_amount_valid.
    rv_valid = abap_false.
    IF iv_value IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_value) = iv_value.
    CONDENSE lv_value NO-GAPS.
    REPLACE ALL OCCURRENCES OF ',' IN lv_value WITH '.'.

    DATA(lo_regex) = NEW cl_abap_regex( pattern = '^[-+]?\d+(\.\d+)?$' ).
    DATA(lo_matcher) = lo_regex->create_matcher( text = lv_value ).
    IF lo_matcher->match( ) = abap_true.
      rv_valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_item_context.
    rv_item_no = |{ iv_index }|.
    IF is_item-nrwierszafa IS NOT INITIAL.
      rv_item_no = |{ is_item-nrwierszafa }|.
    ELSEIF is_item-uu_id IS NOT INITIAL.
      rv_item_no = |{ is_item-uu_id }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
