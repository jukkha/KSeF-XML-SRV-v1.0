CLASS zcl_ksef_xml_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*    TYPES: BEGIN OF ty_msg,
*             type TYPE c LENGTH 1,   "E/W/I
*             text TYPE string,
*           END OF ty_msg,
*           tt_msg TYPE STANDARD TABLE OF ty_msg WITH EMPTY KEY.

    CLASS-METHODS validate_string IMPORTING iv_xml_str TYPE string
                                  EXPORTING ev_ok      TYPE abap_bool
                                            et_msg     TYPE zkstg_t_msg.

    CLASS-METHODS validate
      IMPORTING
        iv_xml_xstr TYPE xstring
      EXPORTING
        ev_ok       TYPE abap_bool
        et_msg      TYPE zkstg_t_msg.

  PRIVATE SECTION.
    CONSTANTS:
      c_ns       TYPE string VALUE 'http://crd.gov.pl/wzor/2025/06/25/13775/',
      c_root     TYPE string VALUE 'Faktura',
      c_kod_text TYPE string VALUE 'FA',
      c_kod_sys  TYPE string VALUE 'FA (3)',
      c_ver      TYPE string VALUE '1-0E',
      c_variant  TYPE string VALUE '3',
      c_dt_min   TYPE string VALUE '2025-09-01T00:00:00Z',
      c_dt_max   TYPE string VALUE '2050-01-01T23:59:59Z'.

    CLASS-METHODS add
      IMPORTING iv_type TYPE c
                iv_text TYPE string
      CHANGING  ct_msg  TYPE zkstg_t_msg.

    CLASS-METHODS first_child
      IMPORTING io_parent      TYPE REF TO if_ixml_node
                iv_name        TYPE string
                iv_ns          TYPE string
      RETURNING VALUE(ro_elem) TYPE REF TO if_ixml_element.

    CLASS-METHODS next_elem_sibling
      IMPORTING io_node        TYPE REF TO if_ixml_node
      RETURNING VALUE(ro_elem) TYPE REF TO if_ixml_element.

    CLASS-METHODS count_children
      IMPORTING io_parent     TYPE REF TO if_ixml_node
                iv_name       TYPE string
                iv_ns         TYPE string
      RETURNING VALUE(rv_cnt) TYPE i.

    CLASS-METHODS get_attr
      IMPORTING io_elem       TYPE REF TO if_ixml_element
                iv_attr       TYPE string
      RETURNING VALUE(rv_val) TYPE string.

    CLASS-METHODS get_text
      IMPORTING io_elem        TYPE REF TO if_ixml_element
      RETURNING VALUE(rv_text) TYPE string.

    CLASS-METHODS parse_ts_utc
      IMPORTING iv_iso       TYPE string
      RETURNING VALUE(rv_ts) TYPE timestampl.

ENDCLASS.

CLASS zcl_ksef_xml_validator IMPLEMENTATION.

  METHOD add.
    APPEND VALUE #( type = iv_type text = iv_text ) TO ct_msg.
  ENDMETHOD.

  METHOD next_elem_sibling.
    CLEAR ro_elem.
    IF io_node IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_n) = io_node->get_next( ).
    WHILE lo_n IS NOT INITIAL.
      IF lo_n->get_type( ) = if_ixml_node=>co_node_element.
        ro_elem = CAST if_ixml_element( lo_n ).
        RETURN.
      ENDIF.
      lo_n = lo_n->get_next( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD first_child.
    CLEAR ro_elem.
    IF io_parent IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_n) = io_parent->get_first_child( ).
    WHILE lo_n IS NOT INITIAL.
      IF lo_n->get_type( ) = if_ixml_node=>co_node_element.
        DATA(lo_e) = CAST if_ixml_element( lo_n ).
        IF lo_e->get_name( ) = iv_name AND lo_e->get_namespace_uri( ) = iv_ns.
          ro_elem = lo_e.
          RETURN.
        ENDIF.
      ENDIF.
      lo_n = lo_n->get_next( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD count_children.
    rv_cnt = 0.
    IF io_parent IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_n) = io_parent->get_first_child( ).
    WHILE lo_n IS NOT INITIAL.
      IF lo_n->get_type( ) = if_ixml_node=>co_node_element.
        DATA(lo_e) = CAST if_ixml_element( lo_n ).
        IF lo_e->get_name( ) = iv_name AND lo_e->get_namespace_uri( ) = iv_ns.
          rv_cnt += 1.
        ENDIF.
      ENDIF.
      lo_n = lo_n->get_next( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD get_attr.
    rv_val = ``.
    IF io_elem IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_attrs) = io_elem->get_attributes( ).
    IF lo_attrs IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_a) = lo_attrs->get_named_item( iv_attr ).
    IF lo_a IS BOUND.
      rv_val = lo_a->get_value( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_text.
    rv_text = ``.
    IF io_elem IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_n) = io_elem->get_first_child( ).
    WHILE lo_n IS NOT INITIAL.
      IF lo_n->get_type( ) = if_ixml_node=>co_node_text.
        rv_text = lo_n->get_value( ).
        CONDENSE rv_text.
        RETURN.
      ENDIF.
      lo_n = lo_n->get_next( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_ts_utc.
    "format view 2025-12-23T20:01:17Z
    rv_ts = 0.
    DATA(lv) = iv_iso.
    CONDENSE lv NO-GAPS.
    IF strlen( lv ) < 19.
      RETURN.
    ENDIF.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.

    TRY.
        lv_date = |{ lv(4) }{ lv+5(2) }{ lv+8(2) }|.
        lv_time = |{ lv+11(2) }{ lv+14(2) }{ lv+17(2) }|.
      CATCH cx_root.
        RETURN.
    ENDTRY.

    " UTC timestamp
    CONVERT DATE lv_date TIME lv_time INTO TIME STAMP rv_ts TIME ZONE 'UTC'.
  ENDMETHOD.

  METHOD validate.

    ev_ok = abap_false.
    CLEAR et_msg.

    " 1) well-formed
    DATA lo_ixml   TYPE REF TO if_ixml.
    DATA lo_doc    TYPE REF TO if_ixml_document.
    DATA lo_sf     TYPE REF TO if_ixml_stream_factory.
    DATA lo_stream TYPE REF TO if_ixml_istream.
    DATA lo_parser TYPE REF TO if_ixml_parser.

    lo_ixml = cl_ixml=>create( ).
    lo_sf   = lo_ixml->create_stream_factory( ).
    lo_doc  = lo_ixml->create_document( ).

    lo_stream = lo_sf->create_istream_xstring( iv_xml_xstr ).
    lo_parser = lo_ixml->create_parser(
                  stream_factory = lo_sf
                  istream        = lo_stream
                  document       = lo_doc ).

    IF lo_parser->parse( ) <> 0.
      add( EXPORTING iv_type = 'E'
                     iv_text = 'XML is not well-formed (parse error).'
           CHANGING ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lo_root) = lo_doc->get_root_element( ).
    IF lo_root IS INITIAL.
      add( EXPORTING iv_type = 'E'
                  iv_text = 'Missing root element.'
        CHANGING ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF lo_root->get_name( ) <> c_root.
      add( EXPORTING iv_type = 'E'
                     iv_text = |Root element must be "{ c_root }", got "{ lo_root->get_name( ) }".|
           CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF lo_root->get_namespace_uri( ) <> c_ns.
      add( EXPORTING iv_type = 'E'
                     iv_text = |Wrong namespace. Expected "{ c_ns }", got "{ lo_root->get_namespace_uri( ) }".|
           CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lo_e) = next_elem_sibling( lo_root->get_first_child( ) ).
    CLEAR lo_e.
    DATA(lo_n) = lo_root->get_first_child( ).
    WHILE lo_n IS NOT INITIAL AND lo_e IS INITIAL.
      IF lo_n->get_type( ) = if_ixml_node=>co_node_element.
        lo_e = CAST if_ixml_element( lo_n ).
      ELSE.
        lo_n = lo_n->get_next( ).
      ENDIF.
    ENDWHILE.

    IF lo_e IS INITIAL.
      add( EXPORTING iv_type = 'E'
                     iv_text = 'Faktura has no child elements.'
           CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DEFINE expect.
      IF &1 IS INITIAL OR &1->get_name( ) <> &2 OR &1->get_namespace_uri( ) <> c_ns.
            add( EXPORTING iv_type = 'E'
                     iv_text = |Expected element "{ &2 }" in sequence.|
           CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
      &1 = next_elem_sibling( &1 ).
    END-OF-DEFINITION.

    expect lo_e 'Naglowek'.
    expect lo_e 'Podmiot1'.
    expect lo_e 'Podmiot2'.

    " Podmiot3 0..100
    DATA(lv_p3) = 0.
    WHILE lo_e IS NOT INITIAL AND lo_e->get_name( ) = 'Podmiot3' AND lo_e->get_namespace_uri( ) = c_ns.
      lv_p3 += 1.
      IF lv_p3 > 100.
        add( EXPORTING iv_type = 'E'
                 iv_text = 'Podmiot3 occurs more than 100 times (XSD maxOccurs=100).'
       CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
      lo_e = next_elem_sibling( lo_e ).
    ENDWHILE.

    IF lo_e IS NOT INITIAL AND lo_e->get_name( ) = 'PodmiotUpowazniony' AND lo_e->get_namespace_uri( ) = c_ns.
      lo_e = next_elem_sibling( lo_e ).
    ENDIF.

    " Fa required
    IF lo_e IS INITIAL OR lo_e->get_name( ) <> 'Fa' OR lo_e->get_namespace_uri( ) <> c_ns.
      add( EXPORTING iv_type = 'E'
                     iv_text = 'Missing required element "Fa" in sequence.'
           CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.
    lo_e = next_elem_sibling( lo_e ).

    IF lo_e IS NOT INITIAL AND lo_e->get_name( ) = 'Stopka' AND lo_e->get_namespace_uri( ) = c_ns.
      lo_e = next_elem_sibling( lo_e ).
    ENDIF.

    IF lo_e IS NOT INITIAL AND lo_e->get_name( ) = 'Zalacznik' AND lo_e->get_namespace_uri( ) = c_ns.
      lo_e = next_elem_sibling( lo_e ).
    ENDIF.

    IF lo_e IS NOT INITIAL.
      add( EXPORTING iv_type = 'E'
                     iv_text = |Unexpected element after expected sequence: "{ lo_e->get_name( ) }".|
           CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " =========================================================
    " Naglowek validation (FA(3)
    " =========================================================
    DATA(lo_nag) = first_child( io_parent = lo_root iv_name = 'Naglowek' iv_ns = c_ns ).

    DATA(lo_kod) = first_child( io_parent = lo_nag iv_name = 'KodFormularza' iv_ns = c_ns ).
    IF lo_kod IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing required element: Naglowek_KodFormularza.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF get_text( lo_kod ) <> c_kod_text.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Naglowek_KodFormularza.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lv_kodsys) = get_attr( io_elem = lo_kod iv_attr = 'kodSystemowy' ).
    DATA(lv_ver)    = get_attr( io_elem = lo_kod iv_attr = 'wersjaSchemy' ).

    IF lv_kodsys IS INITIAL OR lv_kodsys <> c_kod_sys.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Naglowek_KodFormularza_kodSystemowy.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF lv_ver IS INITIAL OR lv_ver <> c_ver.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Naglowek_KodFormularza_wersjaSchemy.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lo_var) = first_child( io_parent = lo_nag iv_name = 'WariantFormularza' iv_ns = c_ns ).
    IF lo_var IS INITIAL OR get_text( lo_var ) <> c_variant.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Naglowek_KodFormularza_WariantFormularza.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lo_dt) = first_child( io_parent = lo_nag iv_name = 'DataWytworzeniaFa' iv_ns = c_ns ).
    IF lo_dt IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Naglowek_KodFormularza_DataWytworzeniaFa.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lv_dt_str) = get_text( lo_dt ).
    DATA(lv_ts)     = parse_ts_utc( lv_dt_str ).
    IF lv_ts = 0.
      add( EXPORTING iv_type = 'E' iv_text = 'DataWytworzeniaFa is not a valid ISO timestamp.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lv_ts_min) = parse_ts_utc( c_dt_min ).
    DATA(lv_ts_max) = parse_ts_utc( c_dt_max ).

    IF lv_ts < lv_ts_min OR lv_ts > lv_ts_max.
      add( EXPORTING iv_type = 'E' iv_text = |DataWytworzeniaFa out of allowed range: { c_dt_min } .. { c_dt_max }.| CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " =========================================================
    " Podmiot1 validation (FA(3)
    " =========================================================
    DATA(lo_pod1) = first_child( io_parent = lo_root iv_name   = 'Podmiot1' iv_ns     = c_ns ).

    IF lo_pod1 IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing required element: Podmiot1.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " ---- DaneIdentyfikacyjne (required)
    DATA(lo_p1_id) = first_child( io_parent = lo_pod1 iv_name   = 'DaneIdentyfikacyjne' iv_ns     = c_ns ).

    IF lo_p1_id IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1/DaneIdentyfikacyjne is required.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " NIP (required)
    DATA(lo_p1_nip) = first_child( io_parent = lo_p1_id iv_name = 'NIP' iv_ns  = c_ns ).

    IF lo_p1_nip IS INITIAL OR get_text( lo_p1_nip ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot1_DaneIdentyfikacyjne_NIP.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " Minimal safe check: length = 10
    IF strlen( get_text( lo_p1_nip ) ) <> 10.
      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1/NIP must contain exactly 10 digits.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " Nazwa (required)
    DATA(lo_p1_name) = first_child( io_parent = lo_p1_id iv_name   = 'Nazwa' iv_ns = c_ns ).

    IF lo_p1_name IS INITIAL OR get_text( lo_p1_name ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot1_DaneIdentyfikacyjne_Nazwa.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF strlen( get_text( lo_p1_name ) ) > 512.
      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1_DaneIdentyfikacyjne_Nazwa max 512 signs.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " ---- Adres (required)
    DATA(lo_p1_addr) = first_child( io_parent = lo_pod1 iv_name   = 'Adres' iv_ns  = c_ns ).

    IF lo_p1_addr IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing required element: Podmiot1_Adres.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " KodKraju (required, enumeration in XSD)
    DATA(lo_p1_country) = first_child( io_parent = lo_p1_addr iv_name   = 'KodKraju' iv_ns = c_ns ).

    IF lo_p1_country IS INITIAL OR get_text( lo_p1_country ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot1_KodKraju.' CHANGING  ct_msg = et_msg ).
      RETURN.
    ENDIF.

*    IF get_text( lo_p1_country ) <> 'PL'.
*      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1/Adres/KodKraju must be "PL".' CHANGING  ct_msg = et_msg ).
*      RETURN.
*    ENDIF.

    " AdresL1 (required)
    DATA(lo_p1_addr1) = first_child( io_parent = lo_p1_addr  iv_name   = 'AdresL1'  iv_ns = c_ns ).

    IF lo_p1_addr1 IS INITIAL OR get_text( lo_p1_addr1 ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missisng Podmiot1_Adres_AdresL1.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF strlen( get_text( lo_p1_addr1 ) ) > 512.
      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1_Adres_AdresL1 max 512 signs.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " AdresL2 (optional)
    DATA(lo_p1_addr2) = first_child( io_parent = lo_p1_addr  iv_name   = 'AdresL2'  iv_ns = c_ns ).

    IF strlen( get_text( lo_p1_addr2 ) ) > 512.
      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1_Adres_AdresL2 max 512 signs.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    "AdresKoresp (optional)
    DATA(lo_p1_adrKoresp) = first_child( io_parent = lo_pod1  iv_name   = 'AdresKoresp'  iv_ns = c_ns ).

    IF lo_p1_adrKoresp IS BOUND.
      DATA(lo_p1_adrkor_kodkraju) = first_child( io_parent = lo_p1_adrKoresp  iv_name   = 'KodKraju'  iv_ns = c_ns ).
      IF lo_p1_adrkor_kodkraju IS INITIAL OR get_text( lo_p1_adrkor_kodkraju ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot1_AdresKoresp_KodKraj.' CHANGING  ct_msg = et_msg ).
        RETURN.
      ENDIF.

      DATA(lo_p1_adrkorl1) = first_child( io_parent = lo_p1_adrKoresp  iv_name   = 'AdresL1'  iv_ns = c_ns ).
      IF lo_p1_adrkorl1 IS INITIAL OR get_text( lo_p1_adrkorl1 ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot1_AdresKoresp_AdresL1.' CHANGING  ct_msg = et_msg ).
        RETURN.
      ELSE.
        IF strlen( get_text( lo_p1_adrkorl1 ) ) > 512.
          add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1_AdresKoresp_AdresL1 max 512 signs.' CHANGING  ct_msg  = et_msg ).
          RETURN.
        ENDIF.
      ENDIF.

      DATA(lo_p1_adrkorl2) = first_child( io_parent = lo_p1_adrKoresp  iv_name   = 'AdresL2'  iv_ns = c_ns ).
      IF strlen( get_text( lo_p1_adrkorl1 ) ) > 512.
        add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1_AdresKoresp_AdresL2 max 512 signs.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
    ENDIF.

    " =========================================================
    " Podmiot2 validation (FA(3))
    " =========================================================
    DATA(lo_pod2) = first_child( io_parent = lo_root iv_name   = 'Podmiot2' iv_ns = c_ns ).

    IF lo_pod2 IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing required element: Podmiot2.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " ---- DaneIdentyfikacyjne (required)
    DATA(lo_p2_id) = first_child( io_parent = lo_pod2 iv_name   = 'DaneIdentyfikacyjne' iv_ns = c_ns ).

    IF lo_p2_id IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot2/DaneIdentyfikacyjne is required.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " NIP (required)
    DATA(lo_p2_nip) = first_child( io_parent = lo_p2_id iv_name   = 'NIP' iv_ns = c_ns ).

*    IF lo_p2_nip IS INITIAL OR get_text( lo_p2_nip ) IS INITIAL.
*      add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot2_DaneIdentyfikacyjne_NIP' CHANGING  ct_msg  = et_msg ).
*      RETURN.
*    ENDIF.

    IF strlen( get_text( lo_p2_nip ) ) <> 10.
      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot2_NIP must contain exactly 10 digits.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " Nazwa (required)
    DATA(lo_p2_name) = first_child( io_parent = lo_p2_id  iv_name   = 'Nazwa'  iv_ns = c_ns ).

    IF lo_p2_name IS INITIAL OR get_text( lo_p2_name ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot2_DaneIdentyfikacyjne_Nazwa.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.
    IF strlen( get_text( lo_p2_name ) ) > 512.
      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot2_DaneIdentyfikacyjne_Nazwa max 512 signs.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " ---- Adres (optional)
    DATA(lo_p2_addr) = first_child( io_parent = lo_pod2 iv_name   = 'Adres'  iv_ns = c_ns ).

    IF lo_p2_addr IS BOUND.

      " KodKraju (required)
      DATA(lo_p2_country) = first_child( io_parent = lo_p2_addr iv_name   = 'KodKraju'  iv_ns = c_ns ).
      IF lo_p2_country IS INITIAL OR get_text( lo_p2_country ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot2_Adres_KodKraju.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

*    IF get_text( lo_p2_country ) <> 'PL'.
*      add( EXPORTING iv_type = 'E' iv_text = 'Podmiot2/Adres/KodKraju must be "PL" (FA(3) XSD).'  CHANGING  ct_msg  = et_msg ).
*      RETURN.
*    ENDIF.

      " AdresL1 (required)
      DATA(lo_p2_addr1) = first_child( io_parent = lo_p2_addr  iv_name   = 'AdresL1'  iv_ns     = c_ns ).

      IF lo_p2_addr1 IS INITIAL OR get_text( lo_p2_addr1 ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot2_Adres_AdresL1.'  CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

      IF strlen( get_text( lo_p2_addr1 ) ) > 512.
        add( EXPORTING iv_type = 'E' iv_text = 'Podmiot2_Adres_AdresL1 max 512 signs.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

      DATA(lo_p2_addr2) = first_child( io_parent = lo_p2_addr  iv_name   = 'AdresL2'  iv_ns = c_ns ).
      IF strlen( get_text( lo_p2_addr2 ) ) > 512.
        add( EXPORTING iv_type = 'E' iv_text = 'Podmiot2_Adres_AdresL2 max 512 signs.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
    ENDIF.

    "AdresKoresp (optional)
    DATA(lo_p2_adrKoresp) = first_child( io_parent = lo_pod2  iv_name   = 'AdresKoresp'  iv_ns = c_ns ).

    IF lo_p1_adrKoresp IS BOUND.
      DATA(lo_p2_adrkor_kodkraju) = first_child( io_parent = lo_p2_adrKoresp  iv_name   = 'KodKraju'  iv_ns = c_ns ).
      IF lo_p1_adrkor_kodkraju IS INITIAL OR get_text( lo_p1_adrkor_kodkraju ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot1_AdresKoresp_KodKraj.' CHANGING  ct_msg = et_msg ).
        RETURN.
      ENDIF.

      DATA(lo_p2_adrkorl1) = first_child( io_parent = lo_p2_adrKoresp  iv_name   = 'AdresL1'  iv_ns = c_ns ).
      IF lo_p2_adrkorl1 IS INITIAL OR get_text( lo_p2_adrkorl1 ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Missing Podmiot1_AdresKoresp_AdresL1.' CHANGING  ct_msg = et_msg ).
        RETURN.
      ELSE.
        IF strlen( get_text( lo_p2_adrkorl1 ) ) > 512.
          add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1_AdresKoresp_AdresL1 max 512 signs.' CHANGING  ct_msg  = et_msg ).
          RETURN.
        ENDIF.
      ENDIF.

      DATA(lo_p2_adrkorl2) = first_child( io_parent = lo_p2_adrKoresp  iv_name   = 'AdresL2'  iv_ns = c_ns ).
      IF strlen( get_text( lo_p2_adrkorl2 ) ) > 512.
        add( EXPORTING iv_type = 'E' iv_text = 'Podmiot1_AdresKoresp_AdresL2 max 512 signs.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
    ENDIF.

    " =========================================================
    " Fa validation
    " =========================================================

    DATA(lo_fa) = first_child( io_parent = lo_root iv_name = 'Fa' iv_ns = c_ns ).
    IF lo_fa IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing required element: Fa.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " --- KodWaluty (required)
    DATA(lo_kw) = first_child( io_parent = lo_fa iv_name = 'KodWaluty' iv_ns = c_ns ).
    IF lo_kw IS INITIAL OR get_text( lo_kw ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_KodWaluty.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lv_kw) = get_text( lo_kw ).
    IF strlen( lv_kw ) <> 3.
      add( EXPORTING iv_type = 'E' iv_text = 'Fa/KodWaluty must have length 3 (ISO 4217).' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " --- P_1 (required, date)
    DATA(lo_p1) = first_child( io_parent = lo_fa iv_name = 'P_1' iv_ns = c_ns ).
    IF lo_p1 IS INITIAL OR get_text( lo_p1 ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_P_1.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lv_p1) = get_text( lo_p1 ).
    IF strlen( lv_p1 ) <> 10 OR lv_p1+4(1) <> '-' OR lv_p1+7(1) <> '-'.
      add( EXPORTING iv_type = 'E' iv_text = 'Fa/P_1 must be in format YYYY-MM-DD.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " --- P_2 (required, invoice number)
    DATA(lo_p2) = first_child( io_parent = lo_fa iv_name = 'P_2' iv_ns = c_ns ).
    IF lo_p2 IS INITIAL OR get_text( lo_p2 ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_P_2.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " --- OkresFa (optional)
    DATA(lo_okresfa) = first_child( io_parent = lo_fa iv_name = 'OkresFa' iv_ns = c_ns ).
    IF lo_okresfa IS BOUND.
      DATA(lo_p_6_od) = first_child( io_parent = lo_okresfa iv_name = 'P_6_Od' iv_ns = c_ns ).
      IF lo_p_6_od IS INITIAL OR get_text( lo_p_6_od ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_OkresFa_P_6_Od.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
      DATA(lo_p_6_do) = first_child( io_parent = lo_okresfa iv_name = 'P_6_Do' iv_ns = c_ns ).
      IF lo_p_6_od IS INITIAL OR get_text( lo_p_6_od ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_OkresFa_P_6_Do.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
    ENDIF.

    " --- Prices / VAT summary checks: P_13_x with paired P_14_x (+ P_14_xW for foreign currency)
    DATA(lv_cur) = get_text( lo_kw ).

    TYPES: BEGIN OF ty_pair,
             p13  TYPE string,
             p14  TYPE string,
             p14w TYPE string,
           END OF ty_pair.

    DATA lt_pairs TYPE STANDARD TABLE OF ty_pair WITH EMPTY KEY.
    DATA ls_pair  TYPE ty_pair.

    CLEAR lt_pairs.

    ls_pair-p13  = 'P_13_1'.  ls_pair-p14  = 'P_14_1'.  ls_pair-p14w = 'P_14_1W'.  APPEND ls_pair TO lt_pairs.
    ls_pair-p13  = 'P_13_2'.  ls_pair-p14  = 'P_14_2'.  ls_pair-p14w = 'P_14_2W'.  APPEND ls_pair TO lt_pairs.
    ls_pair-p13  = 'P_13_3'.  ls_pair-p14  = 'P_14_3'.  ls_pair-p14w = 'P_14_3W'.  APPEND ls_pair TO lt_pairs.
    ls_pair-p13  = 'P_13_4'.  ls_pair-p14  = 'P_14_4'.  ls_pair-p14w = 'P_14_4W'.  APPEND ls_pair TO lt_pairs.
    ls_pair-p13  = 'P_13_5'.  ls_pair-p14  = 'P_14_5'.  CLEAR ls_pair-p14w.        APPEND ls_pair TO lt_pairs.

    DATA(lv_any_p13) = abap_false.

    LOOP AT lt_pairs ASSIGNING FIELD-SYMBOL(<p>).

      DATA(lo_p13)  = first_child( io_parent = lo_fa iv_name = <p>-p13  iv_ns = c_ns ).
      DATA(lo_p14)  = first_child( io_parent = lo_fa iv_name = <p>-p14  iv_ns = c_ns ).
      DATA lo_p14w TYPE REF TO if_ixml_element.
      CLEAR lo_p14w.
      IF <p>-p14w IS NOT INITIAL.
        lo_p14w = first_child( io_parent = lo_fa iv_name = <p>-p14w iv_ns = c_ns ).
      ENDIF.

      DATA lv_p13_txt  TYPE string.
      DATA lv_p14_txt  TYPE string.
      DATA lv_p14w_txt TYPE string.
      CLEAR: lv_p13_txt, lv_p14_txt, lv_p14w_txt.

      IF lo_p13 IS BOUND.
        lv_p13_txt = get_text( lo_p13 ).
      ENDIF.
      IF lo_p14 IS BOUND.
        lv_p14_txt = get_text( lo_p14 ).
      ENDIF.
      IF <p>-p14w IS NOT INITIAL AND lo_p14w IS BOUND.
        lv_p14w_txt = get_text( lo_p14w ).
      ENDIF.

      " --- Rule A: at least one P_13_* must be filled
      IF lv_p13_txt IS NOT INITIAL.
        lv_any_p13 = abap_true.
      ENDIF.

      " --- Rule B: if P_13_* filled => paired P_14_* must be filled
      IF lv_p13_txt IS NOT INITIAL AND lv_p14_txt IS INITIAL.
        add( EXPORTING iv_type = 'E'
                     iv_text = |Fa/{ <p>-p13 } is filled, therefore Fa/{ <p>-p14 } is required.|
             CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

      " --- Rule C: if P_13_* empty => paired P_14_* (and W) should not appear
      IF lv_p13_txt IS INITIAL AND lv_p14_txt IS NOT INITIAL.
        add( EXPORTING iv_type = 'E'
                     iv_text = |Fa/{ <p>-p14 } must not be provided when Fa/{ <p>-p13 } is empty.|
             CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
      IF <p>-p14w IS NOT INITIAL AND lv_p13_txt IS INITIAL AND lv_p14w_txt IS NOT INITIAL.
        add( EXPORTING iv_type = 'E'
                     iv_text = |Fa/{ <p>-p14w } must not be provided when Fa/{ <p>-p13 } is empty.|
             CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

      " --- Rule D: if currency <> PLN and pair has W-field => W must be filled when P_13 is filled
      IF lv_p13_txt IS NOT INITIAL AND <p>-p14w IS NOT INITIAL AND lv_cur <> 'PLN' AND lv_p14w_txt IS INITIAL.
        add( EXPORTING iv_type = 'E'
                     iv_text = |Fa/{ <p>-p14w } is required when KodWaluty <> PLN and Fa/{ <p>-p13 } is filled.|
             CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

      " Optional strictness: if currency = PLN then W must be empty
      IF lv_p13_txt IS NOT INITIAL AND <p>-p14w IS NOT INITIAL AND lv_cur = 'PLN' AND lv_p14w_txt IS NOT INITIAL.
        add( EXPORTING iv_type = 'E'
                     iv_text = |Fa/{ <p>-p14w } must be empty when KodWaluty = PLN.|
             CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

      " --- Numeric format checks (dot decimal separator)
      IF lv_p13_txt IS NOT INITIAL.
        TRY.
            DATA lv_num TYPE decfloat34.
            lv_num = lv_p13_txt.
          CATCH cx_sy_conversion_no_number.
            add( EXPORTING iv_type = 'E'
                         iv_text = |Fa/{ <p>-p13 } must be a number (dot as decimal separator).|
                 CHANGING  ct_msg  = et_msg ).
            RETURN.
        ENDTRY.
      ENDIF.

      IF lv_p14_txt IS NOT INITIAL.
        TRY.
            DATA lv_num2 TYPE decfloat34.
            lv_num2 = lv_p14_txt.
          CATCH cx_sy_conversion_no_number.
            add( EXPORTING iv_type = 'E'
                         iv_text = |Fa/{ <p>-p14 } must be a number (dot as decimal separator).|
                 CHANGING  ct_msg  = et_msg ).
            RETURN.
        ENDTRY.
      ENDIF.

      IF <p>-p14w IS NOT INITIAL AND lv_p14w_txt IS NOT INITIAL.
        TRY.
            DATA lv_num3 TYPE decfloat34.
            lv_num3 = lv_p14w_txt.
          CATCH cx_sy_conversion_no_number.
            add( EXPORTING iv_type = 'E'
                         iv_text = |Fa/{ <p>-p14w } must be a number (dot as decimal separator).|
                 CHANGING  ct_msg  = et_msg ).
            RETURN.
        ENDTRY.
      ENDIF.

    ENDLOOP.

    IF lv_any_p13 = abap_false.
      add( EXPORTING iv_type = 'E'
                   iv_text = 'At least one of Fa/P_13_* fields must be filled.'
           CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " --- P_15 (required)
    DATA(lo_p15) = first_child( io_parent = lo_fa iv_name = 'P_15' iv_ns = c_ns ).
    IF lo_p15 IS INITIAL OR get_text( lo_p15 ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_P_15.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " numeric check (basic)
    DATA(lv_p15_txt) = get_text( lo_p15 ).
    TRY.
        DATA lv_p15_num TYPE decfloat34.
        lv_p15_num = lv_p15_txt.
      CATCH cx_sy_conversion_no_number.
        add( EXPORTING iv_type = 'E' iv_text = 'Fa/P_15 must be a number (dot as decimal separator).' CHANGING  ct_msg  = et_msg ).
        RETURN.
    ENDTRY.

    " --- Adnotacje (required)
    DATA(lo_adn) = first_child( io_parent = lo_fa iv_name = 'Adnotacje' iv_ns = c_ns ).
    IF lo_adn IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing required element: Fa/Adnotacje.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " helper to check TWybor1_2 fields: value must be '1' or '2'
    DATA lv_choice_val TYPE string.
    DEFINE check_1_2.
      DATA(&1) = first_child( io_parent = lo_adn iv_name = &2 iv_ns = c_ns ).
      IF &1 IS INITIAL OR get_text( &1 ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = |Missing Fa_Adnotacje/{ &2 }.| CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

      lv_choice_val = get_text( &1 ).
      IF lv_choice_val <> '1' AND lv_choice_val <> '2'.
        add( EXPORTING iv_type = 'E' iv_text = |Fa/Adnotacje/{ &2 } must be "1" or "2".| CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
    END-OF-DEFINITION.

    check_1_2 lo_p16 'P_16'.
    check_1_2 lo_p17 'P_17'.
    check_1_2 lo_p18 'P_18'.
    check_1_2 lo_p18a 'P_18A'.

    " Zwolnienie (required) : choice (P_19N) OR (P_19 + one of P_19A/B/C)
    DATA(lo_zw) = first_child( io_parent = lo_adn iv_name = 'Zwolnienie' iv_ns = c_ns ).
    IF lo_zw IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_Adnotacje_Zwolnienie.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lo_p19n) = first_child( io_parent = lo_zw iv_name = 'P_19N' iv_ns = c_ns ).
    DATA(lo_p19)  = first_child( io_parent = lo_zw iv_name = 'P_19'  iv_ns = c_ns ).

    IF lo_p19n IS INITIAL AND lo_p19 IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Fa/Adnotacje/Zwolnienie must contain either P_19N or P_19.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF lo_p19 IS NOT INITIAL.
      " if P_19 present -> one of P_19A/B/C must exist
      DATA(lo_p19a) = first_child( io_parent = lo_zw iv_name = 'P_19A' iv_ns = c_ns ).
      DATA(lo_p19b) = first_child( io_parent = lo_zw iv_name = 'P_19B' iv_ns = c_ns ).
      DATA(lo_p19c) = first_child( io_parent = lo_zw iv_name = 'P_19C' iv_ns = c_ns ).
      IF lo_p19a IS INITIAL AND lo_p19b IS INITIAL AND lo_p19c IS INITIAL.
        add( EXPORTING iv_type = 'E'  iv_text = 'Fa/Adnotacje/Zwolnienie: when P_19 is present, one of P_19A/P_19B/P_19C is required.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
    ENDIF.

    " NoweSrodkiTransportu (required): choice (P_22N) OR (P_22 + P_42_5 + NowySrodekTransportu>=1)
    DATA(lo_nst) = first_child( io_parent = lo_adn iv_name = 'NoweSrodkiTransportu' iv_ns = c_ns ).
    IF lo_nst IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing required element: Fa_Adnotacje_NoweSrodkiTransportu.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lo_22n) = first_child( io_parent = lo_nst iv_name = 'P_22N' iv_ns = c_ns ).
    DATA(lo_22)  = first_child( io_parent = lo_nst iv_name = 'P_22'  iv_ns = c_ns ).

    IF lo_22n IS INITIAL AND lo_22 IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Fa/Adnotacje/NoweSrodkiTransportu must contain either P_22N or P_22.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF lo_22 IS NOT INITIAL.
      DATA(lo_425) = first_child( io_parent = lo_nst iv_name = 'P_42_5' iv_ns = c_ns ).
      IF lo_425 IS INITIAL OR get_text( lo_425 ) IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Fa/Adnotacje/NoweSrodkiTransportu/P_42_5 is required when P_22 is present.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.

      DATA(lv_nst_cnt) = count_children( io_parent = lo_nst iv_name = 'NowySrodekTransportu' iv_ns = c_ns ).
      IF lv_nst_cnt < 1.
        add( EXPORTING iv_type = 'E' iv_text = 'Fa/Adnotacje/NoweSrodkiTransportu: NowySrodekTransportu must occur at least once when P_22 is present.' CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
    ENDIF.

    " P_23 (required, 1/2)
    DATA(lo_p23) = first_child( io_parent = lo_adn iv_name = 'P_23' iv_ns = c_ns ).
    IF lo_p23 IS INITIAL OR get_text( lo_p23 ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Fa/Adnotacje/P_23 is required.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.
    IF get_text( lo_p23 ) <> '1' AND get_text( lo_p23 ) <> '2'.
      add( EXPORTING iv_type = 'E' iv_text = 'Fa/Adnotacje/P_23 must be "1" or "2".' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    " PMarzy (required): choice P_PMarzyN OR (P_PMarzy + one of P_PMarzy_2/_3_1/_3_2/_3_3)
    DATA(lo_pm) = first_child( io_parent = lo_adn iv_name = 'PMarzy' iv_ns = c_ns ).
    IF lo_pm IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Fa/Adnotacje/PMarzy is required.'  CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    DATA(lo_pmn) = first_child( io_parent = lo_pm iv_name = 'P_PMarzyN' iv_ns = c_ns ).
    DATA(lo_pm1) = first_child( io_parent = lo_pm iv_name = 'P_PMarzy'  iv_ns = c_ns ).

    IF lo_pmn IS INITIAL AND lo_pm1 IS INITIAL.
      add( EXPORTING iv_type = 'E'  iv_text = 'Fa_Adnotacje_PMarzy must contain either P_PMarzyN or P_PMarzy.'
           CHANGING  ct_msg  = et_msg ).
      RETURN.
    ENDIF.

    IF lo_pm1 IS NOT INITIAL.
      DATA(lo_pm2)  = first_child( io_parent = lo_pm iv_name = 'P_PMarzy_2'   iv_ns = c_ns ).
      DATA(lo_pm31) = first_child( io_parent = lo_pm iv_name = 'P_PMarzy_3_1' iv_ns = c_ns ).
      DATA(lo_pm32) = first_child( io_parent = lo_pm iv_name = 'P_PMarzy_3_2' iv_ns = c_ns ).
      DATA(lo_pm33) = first_child( io_parent = lo_pm iv_name = 'P_PMarzy_3_3' iv_ns = c_ns ).
      IF lo_pm2 IS INITIAL AND lo_pm31 IS INITIAL AND lo_pm32 IS INITIAL AND lo_pm33 IS INITIAL.
        add( EXPORTING iv_type = 'E' iv_text = 'Fa_Adnotacje_PMarzy: when P_PMarzy is present, one of P_PMarzy_2/P_PMarzy_3_1/P_PMarzy_3_2/P_PMarzy_3_3 is required.'
             CHANGING  ct_msg  = et_msg ).
        RETURN.
      ENDIF.
    ENDIF.

    " --- RodzajFaktury (required)
    DATA(lo_RodzajFaktury) = first_child( io_parent = lo_fa iv_name = 'RodzajFaktury' iv_ns = c_ns ).
    IF lo_RodzajFaktury IS INITIAL OR get_text( lo_RodzajFaktury ) IS INITIAL.
      add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_RodzajFaktury.' CHANGING  ct_msg  = et_msg ).
      RETURN.
    ELSE.
      "---------Corrective invoice
      IF get_text( lo_RodzajFaktury ) = 'KOR'.
        DATA(lo_DaneFaKorygowanej) = first_child( io_parent = lo_fa iv_name = 'DaneFaKorygowanej' iv_ns = c_ns ).
        IF lo_DaneFaKorygowanej IS INITIAL.
          add( EXPORTING iv_type = 'E' iv_text = 'Missing required element: Fa_DaneFaKorygowanej.' CHANGING  ct_msg  = et_msg ).
          RETURN.
        ELSE.
          DATA(lo_DataWystFaKorygowanej) = first_child( io_parent = lo_DaneFaKorygowanej iv_name = 'DataWystFaKorygowanej' iv_ns = c_ns ).
          IF lo_DataWystFaKorygowanej IS INITIAL OR get_text( lo_DataWystFaKorygowanej ) IS INITIAL.
            add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_DaneFaKorygowanej_DataWystFaKorygowanej.' CHANGING  ct_msg  = et_msg ).
            RETURN.
          ENDIF.
          DATA(lo_NrFaKorygowanej) = first_child( io_parent = lo_DaneFaKorygowanej iv_name = 'NrFaKorygowanej' iv_ns = c_ns ).
          IF lo_NrFaKorygowanej IS INITIAL OR get_text( lo_NrFaKorygowanej ) IS INITIAL.
            add( EXPORTING iv_type = 'E' iv_text = 'Missing Fa_DaneFaKorygowanej_NrFaKorygowanej.' CHANGING  ct_msg  = et_msg ).
            RETURN.
          ENDIF.
          DATA(lo_Podmiot1K) = first_child( io_parent = lo_fa iv_name = 'Podmiot1K' iv_ns = c_ns ).
          DATA(lo_Podmiot2K) = first_child( io_parent = lo_fa iv_name = 'Podmiot2K' iv_ns = c_ns ).
          IF lo_Podmiot1K IS INITIAL AND lo_Podmiot2K IS INITIAL.
            DATA(lv_wiersz_cnt) = count_children( io_parent = lo_fa iv_name = 'FaWiersz' iv_ns = c_ns ).
            IF lv_wiersz_cnt = 0.
              add( EXPORTING iv_type = 'E' iv_text = 'The corrective invoice does not include any changes.'
                   CHANGING  ct_msg  = et_msg ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " --- FaWiersz is optional in XSD: add warning if none
*    DATA(lv_wiersz_cnt) = count_children( io_parent = lo_fa iv_name = 'FaWiersz' iv_ns = c_ns ).
*    IF lv_wiersz_cnt = 0.
*      add( EXPORTING iv_type = 'W' iv_text = 'Missing Fa_FaWiersz.'
*           CHANGING  ct_msg  = et_msg ).
*    ENDIF.

    ev_ok = abap_true.

  ENDMETHOD.

  METHOD validate_string.
    DATA lv_xml_xstr TYPE xstring.
    TRY.
        lv_xml_xstr = cl_abap_codepage=>convert_to(
                        source    = iv_xml_str
                        codepage  = 'UTF-8' ).
      CATCH cx_root INTO DATA(lx).
        ev_ok = abap_false.
        CLEAR et_msg.
        add(
          EXPORTING
            iv_type = 'E'
            iv_text = |Cannot convert XML string to UTF-8: { lx->get_text( ) }|
          CHANGING
            ct_msg  = et_msg ).
        RETURN.
    ENDTRY.

    validate(
    EXPORTING
      iv_xml_xstr = lv_xml_xstr
    IMPORTING
      ev_ok       = ev_ok
      et_msg      = et_msg ).
  ENDMETHOD.

ENDCLASS.
