CLASS zcl_ksef_xml_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ksef_parser .


  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS read_xml_into_dom
      IMPORTING iv_xml_raw    TYPE string
      RETURNING VALUE(ro_doc) TYPE REF TO if_ixml_document
      RAISING   cx_static_check.

ENDCLASS.

CLASS zcl_ksef_xml_parser IMPLEMENTATION.

  METHOD zif_ksef_parser~get_value_by_name.

    TRY.
        rv_value = iv_elem->get_elements_by_tag_name( name = iv_filter_name )->get_item( 0 )->get_value(  ).
      CATCH cx_root INTO DATA(ex).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_ksef_parser~parse_header.

    DATA(lo_doc)  = read_xml_into_dom( iv_xml_raw ).
    DATA(lo_root) = lo_doc->get_root_element( ).
    CHECK lo_root IS NOT INITIAL.

    DATA(lv_data) = VALUE string( ).
    DATA(lo_iter) = lo_root->create_iterator( ).
    WHILE abap_true = abap_true.
      DATA(lo_node) = lo_iter->get_next( ).

      IF lo_node IS INITIAL.
        EXIT.
      ENDIF.

      IF lo_node->get_type( ) = if_ixml_node=>co_node_element.
        DATA(lo_elem) = CAST if_ixml_element( lo_node ).
        DATA(lv_name) = lo_elem->get_name( ).

        CASE lv_name.
          WHEN 'P_1'.
*            es_header-doc_date = lo_elem->get_value( ).
*            es_header-post_date = lo_elem->get_value( ).
            es_header-doc_date = replace(
                          val  = lo_elem->get_value( )
                          sub  = '-'
                          with = ''
                          occ  = 0 ).
            es_header-post_date = replace(
                           val  = lo_elem->get_value( )
                           sub  = '-'
                           with = ''
                           occ  = 0 ).

          WHEN 'Podmiot1'.
            es_header-seller_nip = me->zif_ksef_parser~get_value_by_name( iv_elem = lo_elem iv_filter_name = 'NIP').
            es_header-company = me->zif_ksef_parser~get_value_by_name( iv_elem = lo_elem iv_filter_name = 'Nazwa').
          WHEN 'Podmiot2'.
            es_header-buyer_nip = me->zif_ksef_parser~get_value_by_name( iv_elem = lo_elem iv_filter_name = 'NIP').
          WHEN 'KodWaluty'.
            es_header-currency = lo_elem->get_value( ).
          WHEN 'NrZamowienia'.
            es_header-po_number = lo_elem->get_value( ).
          WHEN 'RodzajFaktury'.
            es_header-doc_type = lo_elem->get_value( ).
          WHEN 'P_2'.
            es_header-docnumber = lo_elem->get_value( ).
*          WHEN 'KSeFReferenceNumber'.
*            es_header-ksef_ref = lo_elem->get_value( ).
          WHEN 'P_13_1'.
            es_header-p_13_1 = lo_elem->get_value( ).
          WHEN 'P_13_2'.
            es_header-p_13_2 = lo_elem->get_value( ).
          WHEN 'P_13_3'.
            es_header-p_13_3 = lo_elem->get_value( ).
          WHEN 'P_13_4'.
            es_header-p_13_4 = lo_elem->get_value( ).
          WHEN 'P_13_5'.
            es_header-p_13_5 = lo_elem->get_value( ).
          WHEN 'P_13_6_1'.
            es_header-p_13_6_1 = lo_elem->get_value( ).
          WHEN 'P_13_6_2'.
            es_header-p_13_6_2 = lo_elem->get_value( ).
          WHEN 'P_13_6_3'.
            es_header-p_13_6_3 = lo_elem->get_value( ).
          WHEN 'P_13_7'.
            es_header-p_13_7 = lo_elem->get_value( ).
          WHEN 'P_13_8'.
            es_header-p_13_8 = lo_elem->get_value( ).
          WHEN 'P_13_9'.
            es_header-p_13_9 = lo_elem->get_value( ).
          WHEN 'P_13_10'.
            es_header-p_13_10 = lo_elem->get_value( ).
          WHEN 'P_13_11'.
            es_header-p_13_11 = lo_elem->get_value( ).
          WHEN 'P_14_1'.
            es_header-p_14_1 = lo_elem->get_value( ).
          WHEN 'P_14_2'.
            es_header-p_14_2 = lo_elem->get_value( ).
          WHEN 'P_14_3'.
            es_header-p_14_3 = lo_elem->get_value( ).
          WHEN 'P_14_4'.
            es_header-p_14_4 = lo_elem->get_value( ).
          WHEN 'P_14_5'.
            es_header-p_14_5 = lo_elem->get_value( ).
          WHEN 'P_14_1W'.
            es_header-p_14_1w = lo_elem->get_value( ).
          WHEN 'P_14_2W'.
            es_header-p_14_2w = lo_elem->get_value( ).
          WHEN 'P_14_3W'.
            es_header-p_14_3w = lo_elem->get_value( ).
          WHEN 'P_14_4W'.
            es_header-p_14_4w = lo_elem->get_value( ).
          WHEN 'P_15'.
            es_header-p_15 = lo_elem->get_value( ).
*            MOVE lo_elem->get_value( ) TO es_header-p_15.


        ENDCASE.
      ENDIF.
    ENDWHILE.


  ENDMETHOD.


  METHOD zif_ksef_parser~parse_items.
    CLEAR et_items.
    DATA(lo_doc)  = read_xml_into_dom( iv_xml_raw ).

    DATA(lo_fawiersz_collection) = lo_doc->get_elements_by_tag_name( name = 'FaWiersz' ).
*    data(len) = lo_fawiersz_collection->get_length( ).
    DO lo_fawiersz_collection->get_length( ) TIMES.
      FINAL(lo_item_element) = lo_fawiersz_collection->get_item( sy-index - 1 ).
*      data(nam) = node->get_name( ).
*      data(val) = node->get_value( ).
    ENDDO.

*    DATA(lo_root) = lo_doc->get_root_element( ).
*    CHECK lo_root IS NOT INITIAL.

    DATA(lo_iter) = lo_item_element->create_iterator( ).
    WHILE abap_true = abap_true.

      DATA(lo_node) = lo_iter->get_next( ).
      IF lo_node IS INITIAL.
        EXIT.
      ENDIF.

*      IF lo_node->get_type( ) = if_ixml_node=>co_node_element.
*        DATA(lo_elem) = CAST if_ixml_element( lo_node ).
      DATA(lv_name) = lo_node->get_name( ).

      CASE lv_name.
        WHEN 'NrWierszaFa'.
          APPEND INITIAL LINE TO et_items ASSIGNING FIELD-SYMBOL(<ls_item>).
          <ls_item>-invoice_doc_item = lo_node->get_value( ).

        WHEN 'UU_ID'.
          <ls_item>-po_item = lo_node->get_value( ).

      ENDCASE.
*      ENDIF.
    ENDWHILE.

  ENDMETHOD.

  METHOD zif_ksef_parser~validate.
    CLEAR et_msg.
    " Example checks (extend later)
    IF is_header-currency IS INITIAL.
      APPEND VALUE #( type = 'E' code = 'CURR' text = 'Currency is empty' ) TO et_msg.
    ENDIF.
  ENDMETHOD.

  METHOD read_xml_into_dom.

    DATA: lo_ixml    TYPE REF TO if_ixml,
          lo_stream  TYPE REF TO if_ixml_stream_factory,
          lo_istream TYPE REF TO if_ixml_istream,
          lo_parser  TYPE REF TO if_ixml_parser.

    lo_ixml = cl_ixml=>create( ).
    lo_stream = lo_ixml->create_stream_factory( ).
*    lo_istream = lo_stream->create_istream_xstring( lv_xml ).
    lo_istream = lo_stream->create_istream_string( iv_xml_raw ).
    ro_doc = lo_ixml->create_document( ).
    lo_parser = lo_ixml->create_parser( stream_factory = lo_stream
                                        istream        = lo_istream
                                        document       = ro_doc ).
    IF lo_parser->parse( ) IS NOT INITIAL.
*      RAISE EXCEPTION TYPE cx_static_check
*        EXPORTING textid = cx_static_check=>default_textid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
