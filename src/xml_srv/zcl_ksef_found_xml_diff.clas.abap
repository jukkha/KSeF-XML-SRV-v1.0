CLASS zcl_ksef_found_xml_diff DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS gc_amount_tolerance_default TYPE decfloat34 VALUE '0.01'.

    METHODS diff_invoice
      IMPORTING is_old              TYPE zif_ksef_xml_types=>ty_invoice
                is_new              TYPE zif_ksef_xml_types=>ty_invoice
                iv_compare_mode     TYPE zif_ksef_xml_types=>ty_diff_mode DEFAULT zif_ksef_xml_types=>gc_diff_mode_full
                iv_amount_tolerance TYPE decfloat34 DEFAULT gc_amount_tolerance_default
      RETURNING VALUE(rs_diff)      TYPE zif_ksef_xml_types=>ty_diff_result.

    METHODS diff_podmiot
      IMPORTING is_old         TYPE zif_ksef_xml_types=>ty_podmiot
                is_new         TYPE zif_ksef_xml_types=>ty_podmiot
      RETURNING VALUE(rv_diff) TYPE abap_bool.

    METHODS diff_items
      IMPORTING it_old              TYPE zif_ksef_xml_types=>tt_invoice_items
                it_new              TYPE zif_ksef_xml_types=>tt_invoice_items
                iv_compare_mode     TYPE zif_ksef_xml_types=>ty_diff_mode DEFAULT zif_ksef_xml_types=>gc_diff_mode_full
                iv_amount_tolerance TYPE decfloat34 DEFAULT gc_amount_tolerance_default
      RETURNING VALUE(rv_diff)      TYPE abap_bool.

    METHODS diff_zal_items
      IMPORTING it_old              TYPE zif_ksef_xml_types=>tt_zal_items
                it_new              TYPE zif_ksef_xml_types=>tt_zal_items
                iv_compare_mode     TYPE zif_ksef_xml_types=>ty_diff_mode DEFAULT zif_ksef_xml_types=>gc_diff_mode_full
                iv_amount_tolerance TYPE decfloat34 DEFAULT gc_amount_tolerance_default
      RETURNING VALUE(rv_diff)      TYPE abap_bool.

  PRIVATE SECTION.
    METHODS normalize_text
      IMPORTING iv_value        TYPE string
      RETURNING VALUE(rv_value) TYPE string.

    METHODS normalize_amount
      IMPORTING iv_value        TYPE string
      RETURNING VALUE(rv_value) TYPE decfloat34.

    METHODS compare_podmiot_struct
      IMPORTING is_old         TYPE zif_ksef_xml_types=>ty_podmiot
                is_new         TYPE zif_ksef_xml_types=>ty_podmiot
      RETURNING VALUE(rv_diff) TYPE abap_bool.

    METHODS compare_item_struct
      IMPORTING is_old             TYPE zif_ksef_xml_types=>ty_invoice_item
                is_new             TYPE zif_ksef_xml_types=>ty_invoice_item
                iv_compare_mode    TYPE zif_ksef_xml_types=>ty_diff_mode
                iv_tolerance       TYPE decfloat34
      CHANGING  cv_changed_amounts TYPE abap_bool
      RETURNING VALUE(rv_diff)     TYPE abap_bool.

    METHODS compare_zal_item_struct
      IMPORTING is_old             TYPE zif_ksef_xml_types=>ty_zal_item
                is_new             TYPE zif_ksef_xml_types=>ty_zal_item
                iv_compare_mode    TYPE zif_ksef_xml_types=>ty_diff_mode
                iv_tolerance       TYPE decfloat34
      CHANGING  cv_changed_amounts TYPE abap_bool
      RETURNING VALUE(rv_diff)     TYPE abap_bool.

    METHODS compare_items_detail
      IMPORTING it_old             TYPE zif_ksef_xml_types=>tt_invoice_items
                it_new             TYPE zif_ksef_xml_types=>tt_invoice_items
                iv_compare_mode    TYPE zif_ksef_xml_types=>ty_diff_mode
                iv_tolerance       TYPE decfloat34
      CHANGING  ct_changed_keys    TYPE zif_ksef_xml_types=>tt_diff_keys
                cv_changed_amounts TYPE abap_bool
      RETURNING VALUE(rv_diff)     TYPE abap_bool.

    METHODS compare_zal_items_detail
      IMPORTING it_old             TYPE zif_ksef_xml_types=>tt_zal_items
                it_new             TYPE zif_ksef_xml_types=>tt_zal_items
                iv_compare_mode    TYPE zif_ksef_xml_types=>ty_diff_mode
                iv_tolerance       TYPE decfloat34
      CHANGING  ct_changed_keys    TYPE zif_ksef_xml_types=>tt_diff_keys
                cv_changed_amounts TYPE abap_bool
      RETURNING VALUE(rv_diff)     TYPE abap_bool.

    METHODS compare_podmiot3_detail
      IMPORTING it_old          TYPE zif_ksef_xml_types=>tt_podmiot
                it_new          TYPE zif_ksef_xml_types=>tt_podmiot
      CHANGING  ct_changed_keys TYPE zif_ksef_xml_types=>tt_diff_keys
      RETURNING VALUE(rv_diff)  TYPE abap_bool.

    METHODS get_item_key
      IMPORTING is_item       TYPE zif_ksef_xml_types=>ty_invoice_item
      RETURNING VALUE(rv_key) TYPE string.

    METHODS get_zal_item_key
      IMPORTING is_item       TYPE zif_ksef_xml_types=>ty_zal_item
      RETURNING VALUE(rv_key) TYPE string.

    METHODS get_podmiot3_key
      IMPORTING is_podmiot    TYPE zif_ksef_xml_types=>ty_podmiot
      RETURNING VALUE(rv_key) TYPE string.
ENDCLASS.

CLASS zcl_ksef_found_xml_diff IMPLEMENTATION.
  METHOD diff_invoice.
    CLEAR rs_diff.

    rs_diff-changed_podmiot1 = me->diff_podmiot( is_old = is_old-podmiot1
                                                 is_new = is_new-podmiot1 ).
    rs_diff-changed_podmiot2 = me->diff_podmiot( is_old = is_old-podmiot2
                                                 is_new = is_new-podmiot2 ).
    rs_diff-changed_podmiot3 = me->compare_podmiot3_detail( EXPORTING it_old          = is_old-podmiot3
                                                                      it_new          = is_new-podmiot3
                                                            CHANGING  ct_changed_keys = rs_diff-changed_podmiot3_keys ).

    rs_diff-changed_parties = xsdbool( rs_diff-changed_podmiot1 = abap_true
                                    OR rs_diff-changed_podmiot2 = abap_true
                                    OR rs_diff-changed_podmiot3 = abap_true ).

    rs_diff-changed_items = me->compare_items_detail( EXPORTING it_old             = is_old-items
                                                                it_new             = is_new-items
                                                                iv_compare_mode    = iv_compare_mode
                                                                iv_tolerance       = iv_amount_tolerance
                                                      CHANGING  ct_changed_keys    = rs_diff-changed_item_keys
                                                                cv_changed_amounts = rs_diff-changed_amounts ).

    rs_diff-changed_zal_items = me->compare_zal_items_detail( EXPORTING it_old             = is_old-zal_items
                                                                        it_new             = is_new-zal_items
                                                                        iv_compare_mode    = iv_compare_mode
                                                                        iv_tolerance       = iv_amount_tolerance
                                                              CHANGING  ct_changed_keys    = rs_diff-changed_zal_item_keys
                                                                        cv_changed_amounts = rs_diff-changed_amounts ).

    rs_diff-changed_totals = rs_diff-changed_amounts.
  ENDMETHOD.

  METHOD diff_podmiot.
    rv_diff = me->compare_podmiot_struct( is_old = is_old
                                          is_new = is_new ).
  ENDMETHOD.

  METHOD diff_items.
    DATA lt_changed_keys TYPE zif_ksef_xml_types=>tt_diff_keys.
    DATA lv_changed_amounts TYPE abap_bool.

    rv_diff = me->compare_items_detail( EXPORTING it_old             = it_old
                                                  it_new             = it_new
                                                  iv_compare_mode    = iv_compare_mode
                                                  iv_tolerance       = iv_amount_tolerance
                                        CHANGING  ct_changed_keys    = lt_changed_keys
                                                  cv_changed_amounts = lv_changed_amounts ).
  ENDMETHOD.

  METHOD diff_zal_items.
    DATA lt_changed_keys TYPE zif_ksef_xml_types=>tt_diff_keys.
    DATA lv_changed_amounts TYPE abap_bool.

    rv_diff = me->compare_zal_items_detail( EXPORTING it_old             = it_old
                                                      it_new             = it_new
                                                      iv_compare_mode    = iv_compare_mode
                                                      iv_tolerance       = iv_amount_tolerance
                                            CHANGING  ct_changed_keys    = lt_changed_keys
                                                      cv_changed_amounts = lv_changed_amounts ).
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

  METHOD normalize_amount.
    DATA lv_value TYPE string.
    lv_value = iv_value.

    CONDENSE lv_value NO-GAPS.
    IF lv_value CS ',' AND lv_value NS '.'.
      REPLACE ALL OCCURRENCES OF ',' IN lv_value WITH '.'.
    ENDIF.

    TRY.
        rv_value = lv_value.
      CATCH cx_root.
        rv_value = 0.
    ENDTRY.
  ENDMETHOD.

  METHOD compare_podmiot_struct.
    rv_diff = abap_false.

    DATA(lo_desc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( is_old ) ).

    LOOP AT lo_desc->components INTO DATA(ls_comp).
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_old TO FIELD-SYMBOL(<lv_old>).
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_new TO FIELD-SYMBOL(<lv_new>).
      IF <lv_old> IS NOT ASSIGNED OR <lv_new> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      DATA(lv_old_norm) = me->normalize_text( |{ <lv_old> }| ).
      DATA(lv_new_norm) = me->normalize_text( |{ <lv_new> }| ).

      IF lv_old_norm <> lv_new_norm.
        rv_diff = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD compare_item_struct.
    rv_diff = abap_false.

    DATA lt_amount_fields TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    lt_amount_fields = VALUE #(
      ( |P_8A| )
      ( |P_8B| )
      ( |P_9A| )
      ( |P_9B| )
      ( |P_10| )
      ( |P_11| )
      ( |P_11A| )
      ( |P_11VAT| )
      ( |P_12| )
      ( |P_12_XII| )
      ( |P_12_ZAL_15| )
      ( |KWOTAAKCYZY| )
      ( |KURSWALUTY| ) ).

    DATA(lo_desc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( is_old ) ).

    LOOP AT lo_desc->components INTO DATA(ls_comp).
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_old TO FIELD-SYMBOL(<lv_old>).
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_new TO FIELD-SYMBOL(<lv_new>).
      IF <lv_old> IS NOT ASSIGNED OR <lv_new> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      DATA(lv_is_amount) = xsdbool( line_exists( lt_amount_fields[ table_line = ls_comp-name ] ) ).
      IF iv_compare_mode = zif_ksef_xml_types=>gc_diff_mode_totals_only
         AND lv_is_amount = abap_false.
        CONTINUE.
      ENDIF.

      IF lv_is_amount = abap_true.
        DATA(lv_old_amt) = me->normalize_amount( |{ <lv_old> }| ).
        DATA(lv_new_amt) = me->normalize_amount( |{ <lv_new> }| ).
        DATA(lv_delta) = abs( lv_old_amt - lv_new_amt ).
        IF lv_delta > iv_tolerance.
          rv_diff = abap_true.
          cv_changed_amounts = abap_true.
          RETURN.
        ENDIF.
      ELSE.
        DATA(lv_old_norm) = me->normalize_text( |{ <lv_old> }| ).
        DATA(lv_new_norm) = me->normalize_text( |{ <lv_new> }| ).
        IF lv_old_norm <> lv_new_norm.
          rv_diff = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD compare_zal_item_struct.
    rv_diff = abap_false.

    DATA lt_amount_fields TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    lt_amount_fields = VALUE #(
      ( |P_8AZ| )
      ( |P_8BZ| )
      ( |P_9AZ| )
      ( |P_11NETTOZ| )
      ( |P_11VATZ| )
      ( |P_12Z| )
      ( |P_12Z_XII| )
      ( |P_12Z_ZAL_15| )
      ( |KWOTAAKCYZYZ| ) ).

    DATA(lo_desc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( is_old ) ).

    LOOP AT lo_desc->components INTO DATA(ls_comp).
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_old TO FIELD-SYMBOL(<lv_old>).
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_new TO FIELD-SYMBOL(<lv_new>).
      IF <lv_old> IS NOT ASSIGNED OR <lv_new> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      DATA(lv_is_amount) = xsdbool( line_exists( lt_amount_fields[ table_line = ls_comp-name ] ) ).
      IF iv_compare_mode = zif_ksef_xml_types=>gc_diff_mode_totals_only
         AND lv_is_amount = abap_false.
        CONTINUE.
      ENDIF.

      IF lv_is_amount = abap_true.
        DATA(lv_old_amt) = me->normalize_amount( |{ <lv_old> }| ).
        DATA(lv_new_amt) = me->normalize_amount( |{ <lv_new> }| ).
        DATA(lv_delta) = abs( lv_old_amt - lv_new_amt ).
        IF lv_delta > iv_tolerance.
          rv_diff = abap_true.
          cv_changed_amounts = abap_true.
          RETURN.
        ENDIF.
      ELSE.
        DATA(lv_old_norm) = me->normalize_text( |{ <lv_old> }| ).
        DATA(lv_new_norm) = me->normalize_text( |{ <lv_new> }| ).
        IF lv_old_norm <> lv_new_norm.
          rv_diff = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD compare_items_detail.
    rv_diff = abap_false.
    CLEAR ct_changed_keys.

    TYPES: BEGIN OF ty_item_map,
             key  TYPE string,
             item TYPE zif_ksef_xml_types=>ty_invoice_item,
           END OF ty_item_map.

    DATA lt_old_map TYPE HASHED TABLE OF ty_item_map WITH UNIQUE KEY key.
    DATA lt_new_map TYPE HASHED TABLE OF ty_item_map WITH UNIQUE KEY key.
    DATA lv_use_key TYPE abap_bool VALUE abap_true.

    LOOP AT it_old ASSIGNING FIELD-SYMBOL(<ls_old>).
      DATA(lv_key) = me->get_item_key( <ls_old> ).
      IF lv_key IS INITIAL OR line_exists( lt_old_map[ key = lv_key ] ).
        lv_use_key = abap_false.
        EXIT.
      ENDIF.
      INSERT VALUE #( key = lv_key item = <ls_old> ) INTO TABLE lt_old_map.
    ENDLOOP.

    IF lv_use_key = abap_true.
      LOOP AT it_new ASSIGNING FIELD-SYMBOL(<ls_new>).
        lv_key = me->get_item_key( <ls_new> ).
        IF lv_key IS INITIAL OR line_exists( lt_new_map[ key = lv_key ] ).
          lv_use_key = abap_false.
          EXIT.
        ENDIF.
        INSERT VALUE #( key = lv_key item = <ls_new> ) INTO TABLE lt_new_map.
      ENDLOOP.
    ENDIF.

    IF lv_use_key = abap_false.
      " Fallback to index-based comparison if no stable key exists.
      DATA(lv_max) = lines( it_old ).
      IF lines( it_new ) > lv_max.
        lv_max = lines( it_new ).
      ENDIF.

      DO lv_max TIMES.
        READ TABLE it_old INDEX sy-index INTO DATA(ls_old).
        DATA(lv_old_missing) = xsdbool( sy-subrc <> 0 ).
        READ TABLE it_new INDEX sy-index INTO DATA(ls_new).
        DATA(lv_new_missing) = xsdbool( sy-subrc <> 0 ).

        IF lv_old_missing = abap_true AND lv_new_missing = abap_true.
          CONTINUE.
        ENDIF.

        IF lv_old_missing = abap_true OR lv_new_missing = abap_true.
          rv_diff = abap_true.
          APPEND |INDEX:{ sy-index }| TO ct_changed_keys.
          RETURN.
        ENDIF.

        IF me->compare_item_struct( EXPORTING is_old             = ls_old
                                              is_new             = ls_new
                                              iv_compare_mode    = iv_compare_mode
                                              iv_tolerance       = iv_tolerance
                                    CHANGING  cv_changed_amounts = cv_changed_amounts ) = abap_true.
          rv_diff = abap_true.
          APPEND |INDEX:{ sy-index }| TO ct_changed_keys.
          RETURN.
        ENDIF.
      ENDDO.

      RETURN.
    ENDIF.

    LOOP AT lt_new_map ASSIGNING FIELD-SYMBOL(<ls_new_map>).
      READ TABLE lt_old_map ASSIGNING FIELD-SYMBOL(<ls_old_map>) WITH TABLE KEY key = <ls_new_map>-key.
      IF sy-subrc <> 0.
        rv_diff = abap_true.
        APPEND <ls_new_map>-key TO ct_changed_keys.
        RETURN.
      ENDIF.

      IF me->compare_item_struct( EXPORTING is_old             = <ls_old_map>-item
                                            is_new             = <ls_new_map>-item
                                            iv_compare_mode    = iv_compare_mode
                                            iv_tolerance       = iv_tolerance
                                  CHANGING  cv_changed_amounts = cv_changed_amounts ) = abap_true.
        rv_diff = abap_true.
        APPEND <ls_new_map>-key TO ct_changed_keys.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_old_map ASSIGNING FIELD-SYMBOL(<ls_old_remaining>).
      IF NOT line_exists( lt_new_map[ key = <ls_old_remaining>-key ] ).
        rv_diff = abap_true.
        APPEND <ls_old_remaining>-key TO ct_changed_keys.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD compare_zal_items_detail.
    rv_diff = abap_false.
    CLEAR ct_changed_keys.

    TYPES: BEGIN OF ty_item_map,
             key  TYPE string,
             item TYPE zif_ksef_xml_types=>ty_zal_item,
           END OF ty_item_map.

    DATA lt_old_map TYPE HASHED TABLE OF ty_item_map WITH UNIQUE KEY key.
    DATA lt_new_map TYPE HASHED TABLE OF ty_item_map WITH UNIQUE KEY key.
    DATA lv_use_key TYPE abap_bool VALUE abap_true.

    LOOP AT it_old ASSIGNING FIELD-SYMBOL(<ls_old>).
      DATA(lv_key) = me->get_zal_item_key( <ls_old> ).
      IF lv_key IS INITIAL OR line_exists( lt_old_map[ key = lv_key ] ).
        lv_use_key = abap_false.
        EXIT.
      ENDIF.
      INSERT VALUE #( key = lv_key item = <ls_old> ) INTO TABLE lt_old_map.
    ENDLOOP.

    IF lv_use_key = abap_true.
      LOOP AT it_new ASSIGNING FIELD-SYMBOL(<ls_new>).
        lv_key = me->get_zal_item_key( <ls_new> ).
        IF lv_key IS INITIAL OR line_exists( lt_new_map[ key = lv_key ] ).
          lv_use_key = abap_false.
          EXIT.
        ENDIF.
        INSERT VALUE #( key = lv_key item = <ls_new> ) INTO TABLE lt_new_map.
      ENDLOOP.
    ENDIF.

    IF lv_use_key = abap_false.
      " Fallback to index-based comparison if no stable key exists.
      DATA(lv_max) = lines( it_old ).
      IF lines( it_new ) > lv_max.
        lv_max = lines( it_new ).
      ENDIF.

      DO lv_max TIMES.
        READ TABLE it_old INDEX sy-index INTO DATA(ls_old).
        DATA(lv_old_missing) = xsdbool( sy-subrc <> 0 ).
        READ TABLE it_new INDEX sy-index INTO DATA(ls_new).
        DATA(lv_new_missing) = xsdbool( sy-subrc <> 0 ).

        IF lv_old_missing = abap_true AND lv_new_missing = abap_true.
          CONTINUE.
        ENDIF.

        IF lv_old_missing = abap_true OR lv_new_missing = abap_true.
          rv_diff = abap_true.
          APPEND |INDEX:{ sy-index }| TO ct_changed_keys.
          RETURN.
        ENDIF.

        IF me->compare_zal_item_struct( EXPORTING is_old             = ls_old
                                                  is_new             = ls_new
                                                  iv_compare_mode    = iv_compare_mode
                                                  iv_tolerance       = iv_tolerance
                                       CHANGING   cv_changed_amounts = cv_changed_amounts ) = abap_true.
          rv_diff = abap_true.
          APPEND |INDEX:{ sy-index }| TO ct_changed_keys.
          RETURN.
        ENDIF.
      ENDDO.

      RETURN.
    ENDIF.

    LOOP AT lt_new_map ASSIGNING FIELD-SYMBOL(<ls_new_map>).
      READ TABLE lt_old_map ASSIGNING FIELD-SYMBOL(<ls_old_map>) WITH TABLE KEY key = <ls_new_map>-key.
      IF sy-subrc <> 0.
        rv_diff = abap_true.
        APPEND <ls_new_map>-key TO ct_changed_keys.
        RETURN.
      ENDIF.

      IF me->compare_zal_item_struct( EXPORTING is_old             = <ls_old_map>-item
                                                is_new             = <ls_new_map>-item
                                                iv_compare_mode    = iv_compare_mode
                                                iv_tolerance       = iv_tolerance
                                      CHANGING  cv_changed_amounts = cv_changed_amounts ) = abap_true.
        rv_diff = abap_true.
        APPEND <ls_new_map>-key TO ct_changed_keys.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_old_map ASSIGNING FIELD-SYMBOL(<ls_old_remaining>).
      IF NOT line_exists( lt_new_map[ key = <ls_old_remaining>-key ] ).
        rv_diff = abap_true.
        APPEND <ls_old_remaining>-key TO ct_changed_keys.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD compare_podmiot3_detail.
    rv_diff = abap_false.
    CLEAR ct_changed_keys.

    TYPES: BEGIN OF ty_podmiot_map,
             key     TYPE string,
             podmiot TYPE zif_ksef_xml_types=>ty_podmiot,
           END OF ty_podmiot_map.

    DATA lt_old_map TYPE HASHED TABLE OF ty_podmiot_map WITH UNIQUE KEY key.
    DATA lt_new_map TYPE HASHED TABLE OF ty_podmiot_map WITH UNIQUE KEY key.
    DATA lv_use_key TYPE abap_bool VALUE abap_true.

    LOOP AT it_old ASSIGNING FIELD-SYMBOL(<ls_old>).
      DATA(lv_key) = me->get_podmiot3_key( <ls_old> ).
      IF lv_key IS INITIAL OR line_exists( lt_old_map[ key = lv_key ] ).
        lv_use_key = abap_false.
        EXIT.
      ENDIF.
      INSERT VALUE #( key = lv_key podmiot = <ls_old> ) INTO TABLE lt_old_map.
    ENDLOOP.

    IF lv_use_key = abap_true.
      LOOP AT it_new ASSIGNING FIELD-SYMBOL(<ls_new>).
        lv_key = me->get_podmiot3_key( <ls_new> ).
        IF lv_key IS INITIAL OR line_exists( lt_new_map[ key = lv_key ] ).
          lv_use_key = abap_false.
          EXIT.
        ENDIF.
        INSERT VALUE #( key = lv_key podmiot = <ls_new> ) INTO TABLE lt_new_map.
      ENDLOOP.
    ENDIF.

    IF lv_use_key = abap_false.
      " Fallback to index-based comparison when no stable key is available.
      DATA(lv_max) = lines( it_old ).
      IF lines( it_new ) > lv_max.
        lv_max = lines( it_new ).
      ENDIF.

      DO lv_max TIMES.
        READ TABLE it_old INDEX sy-index INTO DATA(ls_old).
        DATA(lv_old_missing) = xsdbool( sy-subrc <> 0 ).
        READ TABLE it_new INDEX sy-index INTO DATA(ls_new).
        DATA(lv_new_missing) = xsdbool( sy-subrc <> 0 ).

        IF lv_old_missing = abap_true AND lv_new_missing = abap_true.
          CONTINUE.
        ENDIF.

        IF lv_old_missing = abap_true OR lv_new_missing = abap_true.
          rv_diff = abap_true.
          APPEND |INDEX:{ sy-index }| TO ct_changed_keys.
          RETURN.
        ENDIF.

        IF me->compare_podmiot_struct( is_old = ls_old
                                       is_new = ls_new ) = abap_true.
          rv_diff = abap_true.
          APPEND |INDEX:{ sy-index }| TO ct_changed_keys.
          RETURN.
        ENDIF.
      ENDDO.

      RETURN.
    ENDIF.

    LOOP AT lt_new_map ASSIGNING FIELD-SYMBOL(<ls_new_map>).
      READ TABLE lt_old_map ASSIGNING FIELD-SYMBOL(<ls_old_map>) WITH TABLE KEY key = <ls_new_map>-key.
      IF sy-subrc <> 0.
        rv_diff = abap_true.
        APPEND <ls_new_map>-key TO ct_changed_keys.
        RETURN.
      ENDIF.

      IF me->compare_podmiot_struct( is_old = <ls_old_map>-podmiot
                                     is_new = <ls_new_map>-podmiot ) = abap_true.
        rv_diff = abap_true.
        APPEND <ls_new_map>-key TO ct_changed_keys.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_old_map ASSIGNING FIELD-SYMBOL(<ls_old_remaining>).
      IF NOT line_exists( lt_new_map[ key = <ls_old_remaining>-key ] ).
        rv_diff = abap_true.
        APPEND <ls_old_remaining>-key TO ct_changed_keys.
        RETURN.
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

    ASSIGN COMPONENT 'ROLA' OF STRUCTURE is_podmiot TO FIELD-SYMBOL(<lv_role>).
    IF <lv_role> IS ASSIGNED AND <lv_role> IS NOT INITIAL.
      rv_key = |{ me->normalize_text( |{ <lv_role> }| ) }|.
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
ENDCLASS.
