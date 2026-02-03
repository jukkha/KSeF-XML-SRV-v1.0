CLASS zcl_ksef_found_xml_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

   PUBLIC SECTION.
    METHODS read_podmiot
      IMPORTING iv_xml     TYPE string
                iv_tagname TYPE string
      RETURNING VALUE(rs_podmiot) TYPE zif_ksef_xml_types=>ty_podmiot.

    METHODS read_podmiot3_list
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_podmiot) TYPE zif_ksef_xml_types=>tt_podmiot.

    METHODS read_items
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_items) TYPE zif_ksef_xml_types=>tt_invoice_items.

    METHODS read_zal_items
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_items) TYPE zif_ksef_xml_types=>tt_zal_items.

    METHODS read_simple_tag
      IMPORTING iv_xml     TYPE string
                iv_tagname TYPE string
      RETURNING VALUE(rv_value) TYPE string.

  PRIVATE SECTION.
    METHODS create_reader
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(ro_reader) TYPE REF TO if_sxml_reader.

    METHODS read_text_value
      IMPORTING io_reader     TYPE REF TO if_sxml_reader
      RETURNING VALUE(rv_val) TYPE string.

    METHODS read_amount_value
      IMPORTING io_reader     TYPE REF TO if_sxml_reader
      RETURNING VALUE(rv_val) TYPE string.
ENDCLASS.

CLASS zcl_ksef_found_xml_reader IMPLEMENTATION.
  METHOD read_podmiot.
    CLEAR rs_podmiot.

    DATA(lo_reader) = me->create_reader( iv_xml ).
    IF lo_reader IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_in_target) = abap_false.
    DATA lv_name TYPE string.

    TRY.
        DO.
          lo_reader->next_node( ).

          lv_name = lo_reader->name.

          CASE lo_reader->node_type.
            WHEN if_sxml_node=>co_nt_element_open.
              IF lv_name = iv_tagname.
                lv_in_target = abap_true.
                CONTINUE.
              ENDIF.

              IF lv_in_target = abap_true.
                CASE lv_name.
                  WHEN 'PrefiksPodatnika'.
                    rs_podmiot-prefikspodatnika = me->read_text_value( lo_reader ).
                  WHEN 'NIP'.
                    rs_podmiot-nip = me->read_text_value( lo_reader ).
                  WHEN 'KodUE'.
                    rs_podmiot-kodue = me->read_text_value( lo_reader ).
                  WHEN 'NrVatUE'.
                    rs_podmiot-nrvatue = me->read_text_value( lo_reader ).
                  WHEN 'NrID'.
                    rs_podmiot-nrid = me->read_text_value( lo_reader ).
                  WHEN 'BrakID'.
                    rs_podmiot-brakid = me->read_text_value( lo_reader ).
                  WHEN 'Nazwa'.
                    rs_podmiot-nazwa = me->read_text_value( lo_reader ).
                  WHEN 'KodKraju'.
                    rs_podmiot-kodkraju = me->read_text_value( lo_reader ).
                  WHEN 'AdresL1'.
                    rs_podmiot-adresl1 = me->read_text_value( lo_reader ).
                  WHEN 'AdresL2'.
                    rs_podmiot-adresl2 = me->read_text_value( lo_reader ).
                  WHEN 'GLN'.
                    rs_podmiot-gln = me->read_text_value( lo_reader ).
                ENDCASE.
              ENDIF.

            WHEN if_sxml_node=>co_nt_element_close.
              IF lv_in_target = abap_true AND lv_name = iv_tagname.
                EXIT.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD read_podmiot3_list.
    CLEAR rt_podmiot.

    DATA(lo_reader) = me->create_reader( iv_xml ).
    IF lo_reader IS INITIAL.
      RETURN.
    ENDIF.

    DATA: lv_name  TYPE string,
          lv_in_p3 TYPE abap_bool VALUE abap_false,
          ls_p3    TYPE zif_ksef_xml_types=>ty_podmiot.

    TRY.
        DO.
          lo_reader->next_node( ).

          lv_name = lo_reader->name.

          CASE lo_reader->node_type.
            WHEN if_sxml_node=>co_nt_element_open.
              IF lv_name = 'Podmiot3'.
                CLEAR ls_p3.
                lv_in_p3 = abap_true.
                CONTINUE.
              ENDIF.

              IF lv_in_p3 = abap_true.
                CASE lv_name.
                  WHEN 'IDNabywcy'.
                    ls_p3-idnabywcy = me->read_text_value( lo_reader ).
                  WHEN 'NIP'.
                    ls_p3-nip = me->read_text_value( lo_reader ).
                  WHEN 'KodUE'.
                    ls_p3-kodue = me->read_text_value( lo_reader ).
                  WHEN 'NrVatUE'.
                    ls_p3-nrvatue = me->read_text_value( lo_reader ).
                  WHEN 'NrID'.
                    ls_p3-nrid = me->read_text_value( lo_reader ).
                  WHEN 'BrakID'.
                    ls_p3-brakid = me->read_text_value( lo_reader ).
                  WHEN 'Nazwa'.
                    ls_p3-nazwa = me->read_text_value( lo_reader ).
                  WHEN 'KodKraju'.
                    ls_p3-kodkraju = me->read_text_value( lo_reader ).
                  WHEN 'AdresL1'.
                    ls_p3-adresl1 = me->read_text_value( lo_reader ).
                  WHEN 'AdresL2'.
                    ls_p3-adresl2 = me->read_text_value( lo_reader ).
                  WHEN 'GLN'.
                    ls_p3-gln = me->read_text_value( lo_reader ).
                ENDCASE.
              ENDIF.

            WHEN if_sxml_node=>co_nt_element_close.
              IF lv_in_p3 = abap_true AND lv_name = 'Podmiot3'.
                IF ls_p3-nip IS NOT INITIAL
                   OR ls_p3-nazwa IS NOT INITIAL
                   OR ls_p3-idnabywcy IS NOT INITIAL.
                  APPEND ls_p3 TO rt_podmiot.
                ENDIF.

                CLEAR ls_p3.
                lv_in_p3 = abap_false.
                CONTINUE.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD read_items.
    CLEAR rt_items.

    DATA(lo_reader) = me->create_reader( iv_xml ).
    IF lo_reader IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_in_row TYPE abap_bool VALUE abap_false.
    DATA lv_name   TYPE string.

    DATA ls_item TYPE zif_ksef_xml_types=>ty_invoice_item.

    TRY.
        DO.
          lo_reader->next_node( ).
          lv_name = lo_reader->name.

          CASE lo_reader->node_type.
            WHEN if_sxml_node=>co_nt_element_open.
              IF lv_name = 'FaWiersz'.
                CLEAR ls_item.
                lv_in_row = abap_true.
                CONTINUE.
              ENDIF.

              IF lv_in_row = abap_true.
                CASE lv_name.
                  WHEN 'NrWierszaFa'.
                    ls_item-nrwierszafa = me->read_text_value( lo_reader ).
                  WHEN 'UU_ID'.
                    ls_item-uu_id = me->read_text_value( lo_reader ).
                  WHEN 'P_6A'.
                    ls_item-p_6a = me->read_text_value( lo_reader ).
                  WHEN 'P_7'.
                    ls_item-p_7 = me->read_text_value( lo_reader ).
                  WHEN 'Indeks'.
                    ls_item-indeks = me->read_text_value( lo_reader ).
                  WHEN 'GTIN'.
                    ls_item-gtin = me->read_text_value( lo_reader ).
                  WHEN 'PKWIU'.
                    ls_item-pkwiu = me->read_text_value( lo_reader ).
                  WHEN 'CN'.
                    ls_item-cn = me->read_text_value( lo_reader ).
                  WHEN 'PKOB'.
                    ls_item-pkob = me->read_text_value( lo_reader ).
                  WHEN 'P_8A'.
                    ls_item-p_8a = me->read_amount_value( lo_reader ).
                  WHEN 'P_8B'.
                    ls_item-p_8b = me->read_amount_value( lo_reader ).
                  WHEN 'P_9A'.
                    ls_item-p_9a = me->read_amount_value( lo_reader ).
                  WHEN 'P_9B'.
                    ls_item-p_9b = me->read_amount_value( lo_reader ).
                  WHEN 'P_10'.
                    ls_item-p_10 = me->read_amount_value( lo_reader ).
                  WHEN 'P_11'.
                    ls_item-p_11 = me->read_amount_value( lo_reader ).
                  WHEN 'P_11A'.
                    ls_item-p_11a = me->read_amount_value( lo_reader ).
                  WHEN 'P_11Vat'.
                    ls_item-p_11vat = me->read_amount_value( lo_reader ).
                  WHEN 'P_12'.
                    ls_item-p_12 = me->read_amount_value( lo_reader ).
                  WHEN 'P_12_XII'.
                    ls_item-p_12_xii = me->read_amount_value( lo_reader ).
                  WHEN 'P_12_Zal_15'.
                    ls_item-p_12_zal_15 = me->read_amount_value( lo_reader ).
                  WHEN 'KwotaAkcyzy'.
                    ls_item-kwotaakcyzy = me->read_amount_value( lo_reader ).
                  WHEN 'GTU'.
                    ls_item-gtu = me->read_text_value( lo_reader ).
                  WHEN 'Procedura'.
                    ls_item-procedura = me->read_text_value( lo_reader ).
                  WHEN 'KursWaluty'.
                    ls_item-kurswaluty = me->read_amount_value( lo_reader ).
                ENDCASE.
              ENDIF.

            WHEN if_sxml_node=>co_nt_element_close.
              IF lv_in_row = abap_true
                 AND ( lv_name = 'FaWiersz' ).
                lv_in_row = abap_false.
                ls_item-stanprzed = ''.

                IF ls_item-uu_id IS NOT INITIAL.
                  APPEND ls_item TO rt_items.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD read_zal_items.
    CLEAR rt_items.

    DATA(lo_reader) = me->create_reader( iv_xml ).
    IF lo_reader IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_in_row TYPE abap_bool VALUE abap_false.
    DATA lv_name   TYPE string.

    DATA ls_zitem TYPE zif_ksef_xml_types=>ty_zal_item.

    TRY.
        DO.
          lo_reader->next_node( ).
          lv_name = lo_reader->name.

          CASE lo_reader->node_type.
            WHEN if_sxml_node=>co_nt_element_open.
              IF lv_name = 'ZamowienieWiersz'.
                CLEAR ls_zitem.
                lv_in_row = abap_true.
                CONTINUE.
              ENDIF.

              IF lv_in_row = abap_true.
                CASE lv_name.
                  WHEN 'NrWierszaZam'.
                    ls_zitem-nrwierszazam = me->read_text_value( lo_reader ).
                  WHEN 'UU_IDZ'.
                    ls_zitem-uu_idz = me->read_text_value( lo_reader ).
                  WHEN 'P_7Z'.
                    ls_zitem-p_7z = me->read_text_value( lo_reader ).
                  WHEN 'IndeksZ'.
                    ls_zitem-indeksz = me->read_text_value( lo_reader ).
                  WHEN 'GTINZ'.
                    ls_zitem-gtinz = me->read_text_value( lo_reader ).
                  WHEN 'PKWiUZ'.
                    ls_zitem-pkwiuz = me->read_text_value( lo_reader ).
                  WHEN 'CNZ'.
                    ls_zitem-cnz = me->read_text_value( lo_reader ).
                  WHEN 'PKOBZ'.
                    ls_zitem-pkobz = me->read_text_value( lo_reader ).
                  WHEN 'P_8AZ'.
                    ls_zitem-p_8az = me->read_amount_value( lo_reader ).
                  WHEN 'P_8BZ'.
                    ls_zitem-p_8bz = me->read_amount_value( lo_reader ).
                  WHEN 'P_9AZ'.
                    ls_zitem-p_9az = me->read_amount_value( lo_reader ).
                  WHEN 'P_11NettoZ'.
                    ls_zitem-p_11nettoz = me->read_amount_value( lo_reader ).
                  WHEN 'P_11VatZ'.
                    ls_zitem-p_11vatz = me->read_amount_value( lo_reader ).
                  WHEN 'P_12Z'.
                    ls_zitem-p_12z = me->read_amount_value( lo_reader ).
                  WHEN 'P_12Z_XII'.
                    ls_zitem-p_12z_xii = me->read_amount_value( lo_reader ).
                  WHEN 'P_12Z_Zal_15'.
                    ls_zitem-p_12z_zal_15 = me->read_amount_value( lo_reader ).
                  WHEN 'KwotaAkcyzyZ'.
                    ls_zitem-kwotaakcyzyz = me->read_amount_value( lo_reader ).
                  WHEN 'GTUZ'.
                    ls_zitem-gtuz = me->read_text_value( lo_reader ).
                  WHEN 'ProceduraZ'.
                    ls_zitem-proceduraz = me->read_text_value( lo_reader ).
                ENDCASE.
              ENDIF.

            WHEN if_sxml_node=>co_nt_element_close.
              IF lv_in_row = abap_true
                 AND ( lv_name = 'ZamowienieWiersz' ).
                lv_in_row = abap_false.
                ls_zitem-stanprzedz = ''.

                IF ls_zitem-uu_idz IS NOT INITIAL.
                  APPEND ls_zitem TO rt_items.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD read_simple_tag.
    CLEAR rv_value.

    DATA(lo_reader) = me->create_reader( iv_xml ).
    IF lo_reader IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_name TYPE string.

    TRY.
        DO.
          lo_reader->next_node( ).
          lv_name = lo_reader->name.

          CASE lo_reader->node_type.
            WHEN if_sxml_node=>co_nt_element_open.
              IF lv_name = iv_tagname.
                rv_value = me->read_text_value( lo_reader ).
                EXIT.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD create_reader.
    CLEAR ro_reader.

    IF iv_xml IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_xml_xstr TYPE xstring.

    TRY.
        lv_xml_xstr = cl_abap_codepage=>convert_to( source = iv_xml codepage = 'UTF-8' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    TRY.
        ro_reader = cl_sxml_string_reader=>create( input = lv_xml_xstr ).
      CATCH cx_root.
        CLEAR ro_reader.
    ENDTRY.
  ENDMETHOD.

  METHOD read_text_value.
    rv_val = ``.

    TRY.
        io_reader->next_node( ).
        IF io_reader->node_type = if_sxml_node=>co_nt_value.
          rv_val = io_reader->value.
        ELSE.
          rv_val = ``.
        ENDIF.
      CATCH cx_sxml_parse_error.
        rv_val = ``.
    ENDTRY.
  ENDMETHOD.

  METHOD read_amount_value.
    rv_val = me->read_text_value( io_reader ).
  ENDMETHOD.
ENDCLASS.
