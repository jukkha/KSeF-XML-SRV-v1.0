CLASS zcl_ksef_found_xml_cor_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS build
      IMPORTING is_old            TYPE zif_ksef_xml_types=>ty_invoice
                is_new            TYPE zif_ksef_xml_types=>ty_invoice
      RETURNING VALUE(rs_invoice) TYPE zif_ksef_xml_types=>ty_invoice.

  PRIVATE SECTION.
    METHODS build_podmiot2k
      IMPORTING is_old              TYPE zif_ksef_xml_types=>ty_invoice
                is_diff             TYPE zif_ksef_xml_types=>ty_diff_result
      RETURNING VALUE(rt_podmiot2k) TYPE zif_ksef_xml_types=>tt_podmiot.

    METHODS build_item_corrections
      IMPORTING is_old          TYPE zif_ksef_xml_types=>ty_invoice
                is_new          TYPE zif_ksef_xml_types=>ty_invoice
                it_changed_keys TYPE zif_ksef_xml_types=>tt_diff_keys
      RETURNING VALUE(rt_items) TYPE zif_ksef_xml_types=>tt_invoice_items.

    METHODS build_zal_item_corrections
      IMPORTING is_old          TYPE zif_ksef_xml_types=>ty_invoice
                is_new          TYPE zif_ksef_xml_types=>ty_invoice
                it_changed_keys TYPE zif_ksef_xml_types=>tt_diff_keys
      RETURNING VALUE(rt_items) TYPE zif_ksef_xml_types=>tt_zal_items.

    METHODS get_item_key
      IMPORTING is_item       TYPE zif_ksef_xml_types=>ty_invoice_item
      RETURNING VALUE(rv_key) TYPE string.

    METHODS get_zal_item_key
      IMPORTING is_item       TYPE zif_ksef_xml_types=>ty_zal_item
      RETURNING VALUE(rv_key) TYPE string.

    METHODS get_podmiot3_key
      IMPORTING is_podmiot    TYPE zif_ksef_xml_types=>ty_podmiot3
      RETURNING VALUE(rv_key) TYPE string.

    METHODS map_podmiot3_to_podmiot2k
      IMPORTING is_podmiot3          TYPE zif_ksef_xml_types=>ty_podmiot3
      RETURNING VALUE(rs_podmiot2k) TYPE zif_ksef_xml_types=>ty_podmiot.

    METHODS get_index_from_key
      IMPORTING iv_key          TYPE string
      RETURNING VALUE(rv_index) TYPE i.

    METHODS normalize_text
      IMPORTING iv_value        TYPE string
      RETURNING VALUE(rv_value) TYPE string.
ENDCLASS.

CLASS zcl_ksef_found_xml_cor_builder IMPLEMENTATION.
  METHOD build.
    rs_invoice = is_new.

    DATA(lo_diff) = NEW zcl_ksef_found_xml_diff( ).
    DATA(ls_diff) = lo_diff->diff_invoice(
      is_old = is_old
      is_new = is_new ).

    CLEAR rs_invoice-podmiot1k.
    CLEAR rs_invoice-podmiot2k.

    IF ls_diff-changed_podmiot1 = abap_true.
      rs_invoice-podmiot1k = is_old-podmiot1.
    ENDIF.

    rs_invoice-podmiot2k = me->build_podmiot2k(
      is_old  = is_old
      is_diff = ls_diff ).

    IF ls_diff-changed_items = abap_true.
      rs_invoice-items = me->build_item_corrections(
        is_old          = is_old
        is_new          = is_new
        it_changed_keys = ls_diff-changed_item_keys ).
    ELSE.
      CLEAR rs_invoice-items.
    ENDIF.

    IF ls_diff-changed_zal_items = abap_true.
      rs_invoice-zal_items = me->build_zal_item_corrections(
        is_old          = is_old
        is_new          = is_new
        it_changed_keys = ls_diff-changed_zal_item_keys ).
    ELSE.
      CLEAR rs_invoice-zal_items.
    ENDIF.
  ENDMETHOD.

  METHOD build_podmiot2k.
    CLEAR rt_podmiot2k.

    IF is_diff-changed_podmiot2 = abap_true.
      APPEND is_old-podmiot2 TO rt_podmiot2k.
    ENDIF.

    IF is_diff-changed_podmiot3 = abap_true AND is_diff-changed_podmiot3_keys IS NOT INITIAL.
      TYPES: BEGIN OF ty_podmiot_map,
               key     TYPE string,
               podmiot TYPE zif_ksef_xml_types=>ty_podmiot3,
             END OF ty_podmiot_map.

      DATA lt_old_map TYPE HASHED TABLE OF ty_podmiot_map WITH UNIQUE KEY key.

      LOOP AT is_old-podmiot3 ASSIGNING FIELD-SYMBOL(<ls_old>).
        DATA(lv_key) = me->get_podmiot3_key( <ls_old> ).
        IF lv_key IS INITIAL OR line_exists( lt_old_map[ key = lv_key ] ).
          CONTINUE.
        ENDIF.
        INSERT VALUE #( key = lv_key podmiot = <ls_old> ) INTO TABLE lt_old_map.
      ENDLOOP.

      DATA lt_seen_keys TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

      LOOP AT is_diff-changed_podmiot3_keys INTO DATA(lv_changed_key).
        IF line_exists( lt_seen_keys[ table_line = lv_changed_key ] ).
          CONTINUE.
        ENDIF.
        INSERT lv_changed_key INTO TABLE lt_seen_keys.

        DATA(lv_index) = me->get_index_from_key( lv_changed_key ).
        IF lv_index > 0.
          " Index-based fallback: rely on diff key format INDEX:<n>.
          READ TABLE is_old-podmiot3 INDEX lv_index INTO DATA(ls_old_index).
          IF sy-subrc = 0.
            APPEND me->map_podmiot3_to_podmiot2k( ls_old_index ) TO rt_podmiot2k.
          ENDIF.
          CONTINUE.
        ENDIF.

        READ TABLE lt_old_map INTO DATA(ls_old_map) WITH TABLE KEY key = lv_changed_key.
        IF sy-subrc = 0.
          APPEND me->map_podmiot3_to_podmiot2k( ls_old_map-podmiot ) TO rt_podmiot2k.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD build_item_corrections.
    CLEAR rt_items.

    TYPES: BEGIN OF ty_item_map,
             key  TYPE string,
             item TYPE zif_ksef_xml_types=>ty_invoice_item,
           END OF ty_item_map.

    DATA lt_old_map TYPE HASHED TABLE OF ty_item_map WITH UNIQUE KEY key.
    DATA lt_new_map TYPE HASHED TABLE OF ty_item_map WITH UNIQUE KEY key.

    LOOP AT is_old-items ASSIGNING FIELD-SYMBOL(<ls_old>).
      DATA(lv_key) = me->get_item_key( <ls_old> ).
      IF lv_key IS INITIAL OR line_exists( lt_old_map[ key = lv_key ] ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( key = lv_key item = <ls_old> ) INTO TABLE lt_old_map.
    ENDLOOP.

    LOOP AT is_new-items ASSIGNING FIELD-SYMBOL(<ls_new>).
      lv_key = me->get_item_key( <ls_new> ).
      IF lv_key IS INITIAL OR line_exists( lt_new_map[ key = lv_key ] ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( key = lv_key item = <ls_new> ) INTO TABLE lt_new_map.
    ENDLOOP.

    DATA lt_seen_keys TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    LOOP AT it_changed_keys INTO DATA(lv_changed_key).
      IF line_exists( lt_seen_keys[ table_line = lv_changed_key ] ).
        CONTINUE.
      ENDIF.
      INSERT lv_changed_key INTO TABLE lt_seen_keys.

      DATA(ls_old_item) = VALUE zif_ksef_xml_types=>ty_invoice_item( ).
      DATA(ls_new_item) = VALUE zif_ksef_xml_types=>ty_invoice_item( ).
      DATA(lv_has_old) = abap_false.
      DATA(lv_has_new) = abap_false.

      DATA(lv_index) = me->get_index_from_key( lv_changed_key ).
      IF lv_index > 0.
        " Index-based fallback: rely on diff key format INDEX:<n>.
        READ TABLE is_old-items INDEX lv_index INTO ls_old_item.
        IF sy-subrc = 0.
          lv_has_old = abap_true.
        ENDIF.
        READ TABLE is_new-items INDEX lv_index INTO ls_new_item.
        IF sy-subrc = 0.
          lv_has_new = abap_true.
        ENDIF.
      ELSE.
        READ TABLE lt_old_map INTO DATA(ls_old_map) WITH TABLE KEY key = lv_changed_key.
        IF sy-subrc = 0.
          ls_old_item = ls_old_map-item.
          lv_has_old = abap_true.
        ENDIF.

        READ TABLE lt_new_map INTO DATA(ls_new_map) WITH TABLE KEY key = lv_changed_key.
        IF sy-subrc = 0.
          ls_new_item = ls_new_map-item.
          lv_has_new = abap_true.
        ENDIF.
      ENDIF.

      IF lv_has_old = abap_true.
        ls_old_item-stanprzed = '1'.
        IF lv_has_new = abap_true AND ls_new_item-nrwierszafa IS NOT INITIAL.
          ls_old_item-nrwierszafa = ls_new_item-nrwierszafa.
        ENDIF.
        APPEND ls_old_item TO rt_items.
      ENDIF.

      IF lv_has_new = abap_true.
        ls_new_item-stanprzed = ''.
        APPEND ls_new_item TO rt_items.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_zal_item_corrections.
    CLEAR rt_items.

    TYPES: BEGIN OF ty_item_map,
             key  TYPE string,
             item TYPE zif_ksef_xml_types=>ty_zal_item,
           END OF ty_item_map.

    DATA lt_old_map TYPE HASHED TABLE OF ty_item_map WITH UNIQUE KEY key.
    DATA lt_new_map TYPE HASHED TABLE OF ty_item_map WITH UNIQUE KEY key.

    LOOP AT is_old-zal_items ASSIGNING FIELD-SYMBOL(<ls_old>).
      DATA(lv_key) = me->get_zal_item_key( <ls_old> ).
      IF lv_key IS INITIAL OR line_exists( lt_old_map[ key = lv_key ] ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( key = lv_key item = <ls_old> ) INTO TABLE lt_old_map.
    ENDLOOP.

    LOOP AT is_new-zal_items ASSIGNING FIELD-SYMBOL(<ls_new>).
      lv_key = me->get_zal_item_key( <ls_new> ).
      IF lv_key IS INITIAL OR line_exists( lt_new_map[ key = lv_key ] ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( key = lv_key item = <ls_new> ) INTO TABLE lt_new_map.
    ENDLOOP.

    DATA lt_seen_keys TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.

    LOOP AT it_changed_keys INTO DATA(lv_changed_key).
      IF line_exists( lt_seen_keys[ table_line = lv_changed_key ] ).
        CONTINUE.
      ENDIF.
      INSERT lv_changed_key INTO TABLE lt_seen_keys.

      DATA(ls_old_item) = VALUE zif_ksef_xml_types=>ty_zal_item( ).
      DATA(ls_new_item) = VALUE zif_ksef_xml_types=>ty_zal_item( ).
      DATA(lv_has_old) = abap_false.
      DATA(lv_has_new) = abap_false.

      DATA(lv_index) = me->get_index_from_key( lv_changed_key ).
      IF lv_index > 0.
        " Index-based fallback: rely on diff key format INDEX:<n>.
        READ TABLE is_old-zal_items INDEX lv_index INTO ls_old_item.
        IF sy-subrc = 0.
          lv_has_old = abap_true.
        ENDIF.
        READ TABLE is_new-zal_items INDEX lv_index INTO ls_new_item.
        IF sy-subrc = 0.
          lv_has_new = abap_true.
        ENDIF.
      ELSE.
        READ TABLE lt_old_map INTO DATA(ls_old_map) WITH TABLE KEY key = lv_changed_key.
        IF sy-subrc = 0.
          ls_old_item = ls_old_map-item.
          lv_has_old = abap_true.
        ENDIF.

        READ TABLE lt_new_map INTO DATA(ls_new_map) WITH TABLE KEY key = lv_changed_key.
        IF sy-subrc = 0.
          ls_new_item = ls_new_map-item.
          lv_has_new = abap_true.
        ENDIF.
      ENDIF.

      IF lv_has_old = abap_true.
        ls_old_item-stanprzedz = '1'.
        IF lv_has_new = abap_true AND ls_new_item-nrwierszazam IS NOT INITIAL.
          ls_old_item-nrwierszazam = ls_new_item-nrwierszazam.
        ENDIF.
        APPEND ls_old_item TO rt_items.
      ENDIF.

      IF lv_has_new = abap_true.
        ls_new_item-stanprzedz = ''.
        APPEND ls_new_item TO rt_items.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_item_key.
    rv_key = ``.

    IF is_item-uu_id IS NOT INITIAL.
      rv_key = me->normalize_text( |{ is_item-uu_id }| ).
    ELSEIF is_item-nrwierszafa IS NOT INITIAL.
      rv_key = |NR:{ me->normalize_text( |{ is_item-nrwierszafa }| ) }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_zal_item_key.
    rv_key = ``.

    IF is_item-uu_idz IS NOT INITIAL.
      rv_key = me->normalize_text( |{ is_item-uu_idz }| ).
    ELSEIF is_item-nrwierszazam IS NOT INITIAL.
      rv_key = |NR:{ me->normalize_text( |{ is_item-nrwierszazam }| ) }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_podmiot3_key.
    rv_key = ``.

    IF is_podmiot-rola IS NOT INITIAL.
      rv_key = |{ me->normalize_text( |{ is_podmiot-rola }| ) }|.
    ENDIF.

    IF is_podmiot-nip IS NOT INITIAL.
      rv_key = |{ rv_key }| && `|NIP:` && me->normalize_text( |{ is_podmiot-nip }| ).
    ELSEIF is_podmiot-nrid IS NOT INITIAL.
      rv_key = |{ rv_key }| && `|ID:` && me->normalize_text( |{ is_podmiot-nrid }| ).
    ELSEIF is_podmiot-idnabywcy IS NOT INITIAL.
      rv_key = |{ rv_key }| && `|IDN:` && me->normalize_text( |{ is_podmiot-idnabywcy }| ).
    ENDIF.

    CONDENSE rv_key NO-GAPS.
  ENDMETHOD.

  METHOD map_podmiot3_to_podmiot2k.
    CLEAR rs_podmiot2k.

    rs_podmiot2k-idnabywcy = is_podmiot3-idnabywcy.
    rs_podmiot2k-nip = is_podmiot3-nip.
    rs_podmiot2k-kodue = is_podmiot3-kodue.
    rs_podmiot2k-nrvatue = is_podmiot3-nrvatue.
    rs_podmiot2k-nrid = is_podmiot3-nrid.
    rs_podmiot2k-brakid = is_podmiot3-brakid.
    rs_podmiot2k-nazwa = is_podmiot3-nazwa.
    rs_podmiot2k-kodkraju = is_podmiot3-kodkraju.
    rs_podmiot2k-adresl1 = is_podmiot3-adr_adresl1.
    rs_podmiot2k-adresl2 = is_podmiot3-adr_adresl2.
    rs_podmiot2k-gln = is_podmiot3-adr_gln.
  ENDMETHOD.

  METHOD get_index_from_key.
    rv_index = 0.

    IF iv_key CP 'INDEX:*'.
      DATA(lv_raw) = iv_key+6.
      CONDENSE lv_raw.
      TRY.
          rv_index = lv_raw.
        CATCH cx_root.
          rv_index = 0.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD normalize_text.
    rv_value = iv_value.
    CONDENSE rv_value.
    SHIFT rv_value LEFT DELETING LEADING space.
    SHIFT rv_value RIGHT DELETING TRAILING space.

    IF rv_value IS NOT INITIAL AND rv_value CO '0123456789'.
      SHIFT rv_value LEFT DELETING LEADING '0'.
      IF rv_value IS INITIAL.
        rv_value = '0'.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
