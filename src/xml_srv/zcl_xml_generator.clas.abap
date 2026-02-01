CLASS zcl_xml_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_ksef_xml_conf,
             node_id    TYPE z_nod_id,
             parent_id  TYPE z_parent_id,
             field_name TYPE zlx_field_name,
             has_attr   TYPE zlx_ksef_has_attr,
             is_leaf    TYPE zlx_field,
           END OF ty_ksef_xml_conf.

    TYPES tt_ksef_xml_conf TYPE STANDARD TABLE OF ty_ksef_xml_conf WITH DEFAULT KEY.
    TYPES tt_item TYPE STANDARD TABLE OF zksef_s_item WITH DEFAULT KEY.
    TYPES tt_podmiot3 TYPE STANDARD TABLE OF zlx_ksef_podmiot3 WITH DEFAULT KEY.
    TYPES tt_podmiot2k TYPE STANDARD TABLE OF zlx_ksef_podmiot WITH DEFAULT KEY.
    TYPES tt_zal_items TYPE STANDARD TABLE OF zlx_ksef_zal_items WITH DEFAULT KEY.

    METHODS:
      constructor
        IMPORTING iv_xml_type TYPE zlx_xml_type,

      create_xml
        IMPORTING iv_docnum         TYPE zlx_docnum
                  is_header         TYPE zksef_s_head
                  it_podmiot3       TYPE tt_podmiot3
                  it_podmiot2k      TYPE tt_podmiot2k
                  it_zal_items      TYPE tt_zal_items
                  it_items          TYPE tt_item
        RETURNING VALUE(rv_xml_str) TYPE string.

    TYPES: BEGIN OF ty_elements,
             node_id TYPE z_nod_id,
             element TYPE REF TO if_ixml_element,
           END OF ty_elements.

    TYPES: BEGIN OF ty_node_to_delete,
             node TYPE REF TO if_ixml_node,
           END OF ty_node_to_delete.

    TYPES tt_elements TYPE STANDARD TABLE OF ty_elements WITH DEFAULT KEY.
    TYPES tt_node_to_delete TYPE STANDARD TABLE OF ty_node_to_delete WITH DEFAULT KEY.

    METHODS get_name IMPORTING iv_nodid       TYPE z_nod_id
                     RETURNING VALUE(rv_name) TYPE zlx_field_name.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_ksef_xml_conf TYPE tt_ksef_xml_conf.
    DATA mt_attr TYPE TABLE OF zlx_ksef_attr.
    DATA mt_elements TYPE tt_elements.
    DATA mt_node_to_delete TYPE tt_node_to_delete.
    DATA mr_ixml TYPE REF TO if_ixml.
    DATA lr_document TYPE REF TO if_ixml_document.
    TYPES tt_nod_id TYPE STANDARD TABLE OF z_nod_id.

    METHODS build_subtree
      IMPORTING
        iv_parent_id TYPE z_nod_id
        ir_parent_el TYPE REF TO if_ixml_node
        ir_data      TYPE REF TO data
        it_conf      TYPE tt_ksef_xml_conf.

    METHODS collect_subtree_conf
      IMPORTING
        iv_root_id  TYPE z_nod_id
      EXPORTING
        et_conf     TYPE tt_ksef_xml_conf
        et_node_ids TYPE tt_nod_id.

ENDCLASS.



CLASS zcl_xml_generator IMPLEMENTATION.


  METHOD constructor.
    SELECT node_id, parent_id, field_name, has_attr, is_leaf
          FROM zlx_xml_conf
         WHERE xml_type = @iv_xml_type
          INTO TABLE @mt_ksef_xml_conf.

    SELECT *
    FROM zlx_ksef_attr
    INTO TABLE @mt_attr.
  ENDMETHOD.

  METHOD create_xml.

    DATA: lr_ixml TYPE REF TO if_ixml.
    DATA lr_parent TYPE REF TO if_ixml_node.
    DATA lt_item_conf TYPE tt_ksef_xml_conf.
    DATA lt_pod3_conf TYPE tt_ksef_xml_conf.

    "Create Document
    mr_ixml = cl_ixml=>create(  ).
    lr_document = mr_ixml->create_document( ).

    SORT mt_ksef_xml_conf BY node_id.

*****Save configure for Podmiot3 into lt.
    DATA lt_pod3_conf_all TYPE tt_ksef_xml_conf.
    DATA lt_pod3_ids      TYPE tt_nod_id.

    me->collect_subtree_conf(
      EXPORTING
        iv_root_id  = '54'          "node_id Podmiot3
      IMPORTING
        et_conf     = lt_pod3_conf_all
        et_node_ids = lt_pod3_ids
    ).

    LOOP AT lt_pod3_ids INTO DATA(lv_del_id).
      DELETE mt_ksef_xml_conf WHERE node_id = lv_del_id.
    ENDLOOP.

*****Save configure for Podmiot2K into lt.
    DATA lt_pod2k_conf_all TYPE tt_ksef_xml_conf.
    DATA lt_pod2k_ids      TYPE tt_nod_id.

    me->collect_subtree_conf(
      EXPORTING
        iv_root_id  = '199'
      IMPORTING
        et_conf     = lt_pod2k_conf_all
        et_node_ids = lt_pod2k_ids
    ).

    LOOP AT lt_pod2k_ids INTO DATA(lv_del_id2).
      DELETE mt_ksef_xml_conf WHERE node_id = lv_del_id2.
    ENDLOOP.
*******************************************
*******************************************
    "Save configure for items into lt.
    LOOP AT mt_ksef_xml_conf ASSIGNING FIELD-SYMBOL(<ls_conf>) WHERE parent_id = '231'.
      APPEND <ls_conf> TO lt_item_conf.
    ENDLOOP.
    DELETE mt_ksef_xml_conf WHERE parent_id = '231'.
*******************************************
*******************************************
    "Save configure for down payment items into lt.
    LOOP AT mt_ksef_xml_conf ASSIGNING <ls_conf> WHERE parent_id = '358'.
      APPEND <ls_conf> TO lt_item_conf.
    ENDLOOP.
    DELETE mt_ksef_xml_conf WHERE parent_id = '358'.
********************************************

    "Add nodes into document
    LOOP AT mt_ksef_xml_conf ASSIGNING <ls_conf>.

      "Get parent node
      READ TABLE mt_elements WITH KEY node_id = <ls_conf>-parent_id ASSIGNING FIELD-SYMBOL(<ls_element>).
      IF sy-subrc = 0.
        lr_parent = <ls_element>-element.
      ELSE.
        lr_parent = lr_document.
      ENDIF.

      FIELD-SYMBOLS <lv_value> TYPE any.
      DATA lr_element TYPE REF TO if_ixml_element.
      CLEAR lr_element.

      IF <ls_conf>-field_name = 'FaWiersz'.

        DATA(lv_index) = 0.
        DO lines( it_items ) TIMES.

          DATA(lr_fawiersz) = lr_document->create_simple_element(
            EXPORTING
              name   = CONV #( <ls_conf>-field_name )
              parent = lr_parent
          ).

          lv_index += 1.
          DATA(ls_item) = it_items[ lv_index ].

          LOOP AT lt_item_conf ASSIGNING FIELD-SYMBOL(<ls_item_conf>).
            ASSIGN COMPONENT <ls_item_conf>-field_name OF STRUCTURE ls_item TO <lv_value>.
            IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
              lr_document->create_simple_element(
                EXPORTING
                  name   = CONV #( <ls_item_conf>-field_name )
                  parent = lr_fawiersz
                  value  = CONV #( <lv_value> )
              ).
            ENDIF.
          ENDLOOP.
        ENDDO.

        lr_element = lr_fawiersz.

      ELSEIF <ls_conf>-field_name = 'ZamowienieWiersz'.

        lv_index = 0.
        DO lines( it_zal_items ) TIMES.

          lr_fawiersz = lr_document->create_simple_element(
            EXPORTING
              name   = CONV #( <ls_conf>-field_name )
              parent = lr_parent
          ).

          lv_index += 1.
          DATA(ls_zal_item) = it_zal_items[ lv_index ].

          LOOP AT lt_item_conf ASSIGNING <ls_item_conf>.
            ASSIGN COMPONENT <ls_item_conf>-field_name OF STRUCTURE ls_zal_item TO <lv_value>.
            IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
              lr_document->create_simple_element(
                EXPORTING
                  name   = CONV #( <ls_item_conf>-field_name )
                  parent = lr_fawiersz
                  value  = CONV #( <lv_value> )
              ).
            ENDIF.
          ENDLOOP.
        ENDDO.

        lr_element = lr_fawiersz.

      ELSEIF <ls_conf>-field_name = 'Podmiot3'.

        LOOP AT it_podmiot3 INTO DATA(ls_pod3).

          DATA(lr_pod3) = lr_document->create_simple_element(
            EXPORTING
              name   = CONV #( <ls_conf>-field_name )
              parent = lr_parent
          ).

          me->build_subtree(
            EXPORTING
              iv_parent_id = '54'
              ir_parent_el = lr_pod3
              ir_data      = REF #( ls_pod3 )
              it_conf      = lt_pod3_conf_all
          ).

        ENDLOOP.
        CLEAR lr_element.

      ELSEIF <ls_conf>-field_name = 'Podmiot2K'.
        LOOP AT it_podmiot2k INTO DATA(ls_pod2k).

          DATA(lr_pod2k) = lr_document->create_simple_element(
            EXPORTING
              name   = CONV #( <ls_conf>-field_name )
              parent = lr_parent
          ).

          me->build_subtree(
            EXPORTING
              iv_parent_id = '199'
              ir_parent_el = lr_pod2k
              ir_data      = REF #( ls_pod2k )
              it_conf      = lt_pod2k_conf_all
          ).

        ENDLOOP.
        CLEAR lr_element.
      ELSE.

        "Create current node
        IF <ls_conf>-is_leaf = abap_true.
          DATA(lv_name) = me->get_name( <ls_conf>-node_id ).
          ASSIGN COMPONENT lv_name OF STRUCTURE is_header TO <lv_value>.
          IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
            lr_element = lr_document->create_simple_element(
              EXPORTING
                name   = CONV #( <ls_conf>-field_name )
                parent = lr_parent
                value  = CONV #( <lv_value> )
            ).
          ENDIF.
        ELSE.
          lr_element = lr_document->create_simple_element(
            EXPORTING
              name   = CONV #( <ls_conf>-field_name )
              parent = lr_parent
          ).
        ENDIF.

        "Add attributes
        IF lr_element IS BOUND AND <ls_conf>-has_attr = abap_true.
          LOOP AT mt_attr ASSIGNING FIELD-SYMBOL(<ls_attr>) WHERE field_name = <ls_conf>-field_name.
            lr_element->set_attribute(
              EXPORTING
                name  = CONV #( <ls_attr>-attr_name )
                value = CONV #( <ls_attr>-attr_value )
            ).
          ENDLOOP.
        ENDIF.

      ENDIF.

      "Save node into temporary table
      IF lr_element IS BOUND.
        APPEND INITIAL LINE TO mt_elements ASSIGNING <ls_element>.
        <ls_element>-node_id = <ls_conf>-node_id.
        <ls_element>-element = lr_element.
      ENDIF.

    ENDLOOP.

    "--- delete empty tags
    FINAL(iterator) = lr_document->create_iterator(  ).
    DO.
      FINAL(node) = iterator->get_next(  ).
      IF node IS INITIAL.
        EXIT.
      ENDIF.
      IF node->get_type(  ) = if_ixml_node=>co_node_element.
        DATA(lv_v) = node->get_value(  ).
        DATA(lv_n) = node->get_name(  ).
        DATA(lv_t) = node->get_type(  ).
        IF node->get_value(  ) IS INITIAL.
          APPEND INITIAL LINE TO mt_node_to_delete ASSIGNING FIELD-SYMBOL(<ls_node>).
          <ls_node>-node = node.
        ENDIF.
      ENDIF.
    ENDDO.
    LOOP AT mt_node_to_delete ASSIGNING <ls_node>.
*    lo_xml_node = lo_xml_ref2->find_node( name = lv_ele_path ). "2641568
*              lo_xml_node->remove_node( ).
      <ls_node>-node->remove_node(  ).
    ENDLOOP.

    "--- Render XML string
    DATA lv_xml_xstring TYPE xstring.
    DATA lv_xml_string TYPE string.
    DATA(lr_stream_factory) = mr_ixml->create_stream_factory(  ).


*    data(lr_istream_string) = lr_stream_factory->create_istream_string( string = lv_xml_string ) .

    "---for xml srting
    DATA(lr_renderer) = mr_ixml->create_renderer( document = lr_document
                               ostream = lr_stream_factory->create_ostream_cstring( rv_xml_str )
                             )->render(  ).

    "---for xml xsrting
    DATA lv_xstr TYPE xstring.
    DATA(lr_renderer_xstr) = mr_ixml->create_renderer(
                       document = lr_document
                       ostream  = lr_stream_factory->create_ostream_xstring( string = lv_xstr )
                       )->render(  ).

    rv_xml_str = cl_abap_conv_codepage=>create_in( codepage = 'UTF-8' )->convert( lv_xstr ).

  ENDMETHOD.

  METHOD get_name.

    CASE iv_nodid.
      WHEN '3'.
        rv_name = 'NAGL_KODFORMULARZA'.
      WHEN '4'.
        rv_name = 'NAGL_WARIANTFORMULARZA'.
      WHEN '5'.
        rv_name = 'NAGL_DATAWYTWORZENIAFA'.
      WHEN '6'.
        rv_name = 'NAGL_SYSTEMINFO'.
      WHEN '8'.
        rv_name = 'P1_PREFIKSPODATNIKA'.
      WHEN '9'.
        rv_name = 'P1_NREORI'.
      WHEN '11'.
        rv_name = 'P1_NIP'.
      WHEN '12'.
        rv_name = 'P1_NAZWA'.
      WHEN '14'.
        rv_name = 'P1_KODKRAJU'.
      WHEN '15'.
        rv_name = 'P1_ADRESL1'.
      WHEN '16'.
        rv_name = 'P1_ADRESL2'.
      WHEN '17'.
        rv_name = 'P1_GLN'.
      WHEN '19'.
        rv_name = 'P1_ADRKOR_KODKRAJU'.
      WHEN '20'.
        rv_name = 'P1_ADRKOR_ADRESL1'.
      WHEN '21'.
        rv_name = 'P1_ADRKOR_ADRESL2'.
      WHEN '22'.
        rv_name = 'P1_ADRKOR_GLN'.
      WHEN '24'.
        rv_name = 'P1_EMAIL'.
      WHEN '25'.
        rv_name = 'P1_TELEFON'.
      WHEN '26'.
        rv_name = 'P1_STATUSINFOPODATNIKA'.
      WHEN '28'.
        rv_name = 'P2_NREORI'.
      WHEN '30'.
        rv_name = 'P2_NIP'.
      WHEN '31'.
        rv_name = 'P2_KODUE'.
      WHEN '32'.
        rv_name = 'P2_NRVATUE'.
      WHEN '33'.
        rv_name = 'P2_KODKRAJU'.
      WHEN '34'.
        rv_name = 'P2_NRID'.
      WHEN '35'.
        rv_name = 'P2_BRAKID'.
      WHEN '36'.
        rv_name = 'P2_NAZWA'.
      WHEN '38'.
        rv_name = 'P2_ADR_KODKRAJU'.
      WHEN '39'.
        rv_name = 'P2_ADR_ADRESL1'.
      WHEN '40'.
        rv_name = 'P2_ADR_ADRESL2'.
      WHEN '41'.
        rv_name = 'P2_ADR_GLN'.
      WHEN '43'.
        rv_name = 'P2_ADRKOR_KODKRAJU'.
      WHEN '44'.
        rv_name = 'P2_ADRKOR_ADRESL1'.
      WHEN '45'.
        rv_name = 'P2_ADRKOR_ADRESL2'.
      WHEN '46'.
        rv_name = 'P2_ADRKOR_GLN'.
      WHEN '48'.
        rv_name = 'P2_EMAIL'.
      WHEN '49'.
        rv_name = 'P2_TELEFON'.
      WHEN '50'.
        rv_name = 'P2_NRKLIENTA'.
      WHEN '51'.
        rv_name = 'P2_IDNABYWCY'.
      WHEN '52'.
        rv_name = 'P2_JST'.
      WHEN '53'.
        rv_name = 'P2_GV'.
      WHEN '55'.
        rv_name = 'IDNABYWCY'.
      WHEN '56'.
        rv_name = 'NREORI'.
      WHEN '58'.
        rv_name = 'NIP'.
      WHEN '59'.
        rv_name = 'IDWEW'.
      WHEN '60'.
        rv_name = 'KODUE'.
      WHEN '61'.
        rv_name = 'NRVATUE'.
      WHEN '62'.
        rv_name = 'KODKRAJU'.
      WHEN '63'.
        rv_name = 'NRID'.
      WHEN '64'.
        rv_name = 'BRAKID'.
      WHEN '65'.
        rv_name = 'NAZWA'.
      WHEN '67'.
        rv_name = 'ADR_KODKRAJU'.
      WHEN '68'.
        rv_name = 'ADR_ADRESL1'.
      WHEN '69'.
        rv_name = 'ADR_ADRESL2'.
      WHEN '70'.
        rv_name = 'ADR_GLN'.
      WHEN '72'.
        rv_name = 'ADRKOR_KODKRAJU'.
      WHEN '73'.
        rv_name = 'ADRKOR_ADRESL1'.
      WHEN '74'.
        rv_name = 'ADRKOR_ADRESL2'.
      WHEN '75'.
        rv_name = 'ADRKOR_GLN'.
      WHEN '77'.
        rv_name = 'EMAIL'.
      WHEN '78'.
        rv_name = 'TELEFON'.
      WHEN '79'.
        rv_name = 'ROLA'.
      WHEN '80'.
        rv_name = 'ROLAINNA'.
      WHEN '81'.
        rv_name = 'OPISROLI'.
      WHEN '82'.
        rv_name = 'UDZIAL'.
      WHEN '83'.
        rv_name = 'NRKLIENTA'.
      WHEN '85'.
        rv_name = 'PU_NREORI'.
      WHEN '87'.
        rv_name = 'PU_NIP'.
      WHEN '88'.
        rv_name = 'PU_NAZWA'.
      WHEN '90'.
        rv_name = 'PU_KODKRAJU'.
      WHEN '91'.
        rv_name = 'PU_ADRESL1'.
      WHEN '92'.
        rv_name = 'PU_ADRESL2'.
      WHEN '93'.
        rv_name = 'PU_GLN'.
      WHEN '95'.
        rv_name = 'PU_ADRKOR_KODKRAJU'.
      WHEN '96'.
        rv_name = 'PU_ADRKOR_ADRESL1'.
      WHEN '97'.
        rv_name = 'PU_ADRKOR_ADRESL2'.
      WHEN '98'.
        rv_name = 'PU_ADRKOR_GLN'.
      WHEN '100'.
        rv_name = 'PU_EMAILPU'.
      WHEN '101'.
        rv_name = 'PU_TELEFONPU'.
      WHEN '102'.
        rv_name = 'PU_ROLAPU'.
      WHEN '104'.
        rv_name = 'FA_KODWALUTY'.
      WHEN '105'.
        rv_name = 'FA_P_1'.
      WHEN '106'.
        rv_name = 'FA_P_1M'.
      WHEN '107'.
        rv_name = 'FA_P_2'.
      WHEN '108'.
        rv_name = 'FA_WZ'.
      WHEN '109'.
        rv_name = 'FA_P_6'.
      WHEN '111'.
        rv_name = 'FA_P_6_OD'.
      WHEN '112'.
        rv_name = 'FA_P_6_DO'.
      WHEN '113'.
        rv_name = 'FA_P_13_1'.
      WHEN '114'.
        rv_name = 'FA_P_14_1'.
      WHEN '115'.
        rv_name = 'FA_P_14_1W'.
      WHEN '116'.
        rv_name = 'FA_P_13_2'.
      WHEN '117'.
        rv_name = 'FA_P_14_2'.
      WHEN '118'.
        rv_name = 'FA_P_14_2W'.
      WHEN '119'.
        rv_name = 'FA_P_13_3'.
      WHEN '120'.
        rv_name = 'FA_P_14_3'.
      WHEN '121'.
        rv_name = 'FA_P_14_3W'.
      WHEN '122'.
        rv_name = 'FA_P_13_4'.
      WHEN '123'.
        rv_name = 'FA_P_14_4'.
      WHEN '124'.
        rv_name = 'FA_P_14_4W'.
      WHEN '125'.
        rv_name = 'FA_P_13_5'.
      WHEN '126'.
        rv_name = 'FA_P_14_5'.
      WHEN '127'.
        rv_name = 'FA_P_13_6_1'.
      WHEN '128'.
        rv_name = 'FA_P_13_6_2'.
      WHEN '129'.
        rv_name = 'FA_P_13_6_3'.
      WHEN '130'.
        rv_name = 'FA_P_13_7'.
      WHEN '131'.
        rv_name = 'FA_P_13_8'.
      WHEN '132'.
        rv_name = 'FA_P_13_9'.
      WHEN '133'.
        rv_name = 'FA_P_13_10'.
      WHEN '134'.
        rv_name = 'FA_P_13_11'.
      WHEN '135'.
        rv_name = 'FA_P_15'.
      WHEN '136'.
        rv_name = 'FA_KURSWALUTYZ'.
      WHEN '138'.
        rv_name = 'FA_P_16'.
      WHEN '139'.
        rv_name = 'FA_P_17'.
      WHEN '140'.
        rv_name = 'FA_P_18'.
      WHEN '141'.
        rv_name = 'FA_P_18A'.
      WHEN '143'.
        rv_name = 'FA_P_19'.
      WHEN '144'.
        rv_name = 'FA_P_19A'.
      WHEN '145'.
        rv_name = 'FA_P_19B'.
      WHEN '146'.
        rv_name = 'FA_P_19C'.
      WHEN '147'.
        rv_name = 'FA_P_19N'.
      WHEN '149'.
        rv_name = 'FA_P_22'.
      WHEN '150'.
        rv_name = 'FA_P_42_5'.
      WHEN '152'.
        rv_name = 'FA_P_22A'.
      WHEN '153'.
        rv_name = 'FA_P_NRWIERSZANST'.
      WHEN '154'.
        rv_name = 'FA_P_22BMK'.
      WHEN '155'.
        rv_name = 'FA_P_22BMD'.
      WHEN '156'.
        rv_name = 'FA_P_22BK'.
      WHEN '157'.
        rv_name = 'FA_P_22BNR'.
      WHEN '158'.
        rv_name = 'FA_P_22BRP'.
      WHEN '159'.
        rv_name = 'FA_P_22B'.
      WHEN '160'.
        rv_name = 'FA_P_22B1'.
      WHEN '161'.
        rv_name = 'FA_P_22B2'.
      WHEN '162'.
        rv_name = 'FA_P_22B3'.
      WHEN '163'.
        rv_name = 'FA_P_22B4'.
      WHEN '164'.
        rv_name = 'FA_P_22BT'.
      WHEN '165'.
        rv_name = 'FA_P_22C'.
      WHEN '166'.
        rv_name = 'FA_P_22C1'.
      WHEN '167'.
        rv_name = 'FA_P_22D'.
      WHEN '168'.
        rv_name = 'FA_P_22D1'.
      WHEN '169'.
        rv_name = 'FA_P_22N'.
      WHEN '170'.
        rv_name = 'FA_P_23'.
      WHEN '172'.
        rv_name = 'FA_P_PMARZY'.
      WHEN '173'.
        rv_name = 'FA_P_PMARZY_2'.
      WHEN '174'.
        rv_name = 'FA_P_PMARZY_3_1'.
      WHEN '175'.
        rv_name = 'fa_p_pmarzy_3_2'.
      WHEN '176'.
        rv_name = 'FA_P_PMARZY_3_3'.
      WHEN '177'.
        rv_name = 'FA_P_PMARZYN'.
      WHEN '178'.
        rv_name = 'FA_RODZAJFAKTURY'.
      WHEN '179'.
        rv_name = 'FA_PRZYCZYNAKOREKTY'.
      WHEN '180'.
        rv_name = 'FA_TYPKOREKTY'.
      WHEN '182'.
        rv_name = 'FA_DATAWYSTFAKORYGOWANEJ'.
      WHEN '183'.
        rv_name = 'FA_NRFAKORYGOWANEJ'.
      WHEN '184'.
        rv_name = 'FA_NRKSEF'.
      WHEN '185'.
        rv_name = 'FA_NRKSEFFAKORYGOWANEJ'.
      WHEN '186'.
        rv_name = 'FA_NRKSEFN'.
      WHEN '187'.
        rv_name = 'FA_OKRESFAKORYGOWANEJ'.
      WHEN '188'.
        rv_name = 'FA_NRFAKORYGOWANY'.
      WHEN '190'.
        rv_name = 'FA_P1K_PREFIKSPODATNIKA'.
      WHEN '192'.
        rv_name = 'FA_P1K_NIP'.
      WHEN '193'.
        rv_name = 'FA_P1K_NAZWA'.
      WHEN '195'.
        rv_name = 'FA_P1K_KODKRAJU'.
      WHEN '196'.
        rv_name = 'FA_P1K_ADRESL1'.
      WHEN '197'.
        rv_name = 'FA_P1K_ADRESL2'.
      WHEN '198'.
        rv_name = 'FA_P1K_GLN'.
*      WHEN '201'.
*        rv_name = 'FA_P2K_NIP'.
*      WHEN '202'.
*        rv_name = 'FA_P2K_KODUE'.
*      WHEN '203'.
*        rv_name = 'FA_P2K_NRVATUE'.
*      WHEN '204'.
*        rv_name = 'FA_P2K_KODKRAJU'.
*      WHEN '205'.
*        rv_name = 'FA_P2K_NRID'.
*      WHEN '206'.
*        rv_name = 'FA_P2K_BRAKID'.
*      WHEN '207'.
*        rv_name = 'FA_P2K_NAZWA'.
*      WHEN '209'.
*        rv_name = 'FA_P2K_ADR_KODKRAJU'.
*      WHEN '210'.
*        rv_name = 'FA_P2K_ADR_ADRESL1'.
*      WHEN '211'.
*        rv_name = 'FA_P2K_ADR_ADRESL2'.
*      WHEN '212'.
*        rv_name = 'FA_P2K_ADR_GLN'.
*      WHEN '213'.
*        rv_name = 'FA_P2K_IDNABYWCY'.
      WHEN '201'.
        rv_name = 'NIP'.
      WHEN '202'.
        rv_name = 'KODUE'.
      WHEN '203'.
        rv_name = 'NRVATUE'.
      WHEN '204'.
        rv_name = 'KODKRAJU'.
      WHEN '205'.
        rv_name = 'NRID'.
      WHEN '206'.
        rv_name = 'BRAKID'.
      WHEN '207'.
        rv_name = 'NAZWA'.
      WHEN '209'.
        rv_name = 'ADR_KODKRAJU'.
      WHEN '210'.
        rv_name = 'ADRESL1'.
      WHEN '211'.
        rv_name = 'ADRESL2'.
      WHEN '212'.
        rv_name = 'GLN'.
      WHEN '213'.
        rv_name = 'IDNABYWCY'.

      WHEN '214'.
        rv_name = 'FA_P_15ZK'.
      WHEN '215'.
        rv_name = 'FA_KURSWALUTYZK'.
      WHEN '217'.
        rv_name = 'FA_P_6Z'.
      WHEN '218'.
        rv_name = 'FA_P_15Z'.
      WHEN '219'.
        rv_name = 'FA_KURSWALUTYZW'.
      WHEN '220'.
        rv_name = 'FA_FP'.
      WHEN '221'.
        rv_name = 'FA_TP'.
      WHEN '223'.
        rv_name = 'FA_NRWIERSZA'.
      WHEN '224'.
        rv_name = 'FA_KLUCZ'.
      WHEN '225'.
        rv_name = 'FA_WARTOSC'.
      WHEN '227'.
        rv_name = 'FA_NRKSEFZN'.
      WHEN '228'.
        rv_name = 'FA_NRFAZALICZKOWEJ'.
      WHEN '229'.
        rv_name = 'FA_NRKSEFFAZALICZKOWEJ'.
      WHEN '230'.
        rv_name = 'FA_ZWROTAKCYZY'.
      WHEN '259'.
        rv_name = 'FA_OBC_KWOTA'.
      WHEN '260'.
        rv_name = 'FA_OBC_POWOD'.
      WHEN '261'.
        rv_name = 'FA_SUMAOBCIAZEN'.
      WHEN '263'.
        rv_name = 'FA_ODL_KWOTA'.
      WHEN '264'.
        rv_name = 'FA_ODL_POWOD'.
      WHEN '265'.
        rv_name = 'FA_SUMAODLICZEN'.
      WHEN '266'.
        rv_name = 'FA_DOZAPLATY'.
      WHEN '267'.
        rv_name = 'FA_DOROZLICZENIA'.
      WHEN '269'.
        rv_name = 'FA_ZAPLACONO'.
      WHEN '270'.
        rv_name = 'FA_DATAZAPLATY'.
      WHEN '271'.
        rv_name = 'FA_ZNACZNIKZAPLATYCZESCIOWEJ'.
      WHEN '273'.
        rv_name = 'FA_ZC_KWOTAZAPLATYCZESCIOWEJ'.
      WHEN '274'.
        rv_name = 'FA_ZC_DATAZAPLATYCZESCIOWEJ'.
      WHEN '275'.
        rv_name = 'FA_ZC_FORMAPLATNOSCI'.
      WHEN '276'.
        rv_name = 'FA_ZC_PLATNOSCINNA'.
      WHEN '277'.
        rv_name = 'FA_ZC_OPISPLATNOSCI'.
      WHEN '279'.
        rv_name = 'FA_TERMIN'.
      WHEN '281'.
        rv_name = 'FA_ILOSC'.
      WHEN '282'.
        rv_name = 'FA_JEDNOSTKA'.
      WHEN '283'.
        rv_name = 'FA_ZDARZENIEPOCZATKOWE'.
      WHEN '284'.
        rv_name = 'FA_FORMAPLATNOSCI'.
      WHEN '285'.
        rv_name = 'FA_PLATNOSCINNA'.
      WHEN '286'.
        rv_name = 'FA_OPISPLATNOSCI'.
      WHEN '288'.
        rv_name = 'FA_NRRB'.
      WHEN '289'.
        rv_name = 'FA_SWIFT'.
      WHEN '290'.
        rv_name = 'FA_RACHUNEKWLASNYBANKU'.
      WHEN '291'.
        rv_name = 'FA_NAZWABANKU'.
      WHEN '292'.
        rv_name = 'FA_OPISRACHUNKU'.
      WHEN '294'.
        rv_name = 'FA_RBF_NRRB_2'.
      WHEN '295'.
        rv_name = 'FA_RBF_SWIFT_2'.
      WHEN '296'.
        rv_name = 'FA_RBF_RACHUNEKWLASNYBANKU_2'.
      WHEN '297'.
        rv_name = 'FA_RBF_NAZWABANKU_2'.
      WHEN '298'.
        rv_name = 'FA_RBF_OPISRACHUNKU_2'.
      WHEN '300'.
        rv_name = 'FA_WARUNKISKONTA'.
      WHEN '301'.
        rv_name = 'FA_WYSOKOSCSKONTA'.
      WHEN '302'.
        rv_name = 'FA_LINKDOPLATNOSCI'.
      WHEN '303'.
        rv_name = 'FA_IPKSEF'.
      WHEN '306'.
        rv_name = 'FA_DATAUMOWY'.
      WHEN '307'.
        rv_name = 'FA_NRUMOWY'.
      WHEN '309'.
        rv_name = 'FA_DATAZAMOWIENIA'.
      WHEN '310'.
        rv_name = 'FA_NRZAMOWIENIA'.
      WHEN '311'.
        rv_name = 'FA_NRPARTIITOWARU'.
      WHEN '312'.
        rv_name = 'FA_WARUNKIDOSTAWY'.
      WHEN '313'.
        rv_name = 'FA_KURSUMOWNY'.
      WHEN '314'.
        rv_name = 'FA_WALUTAUMOWNA'.
      WHEN '316'.
        rv_name = 'FA_RODZAJTRANSPORTU'.
      WHEN '317'.
        rv_name = 'FA_TRANSPORTINNY'.
      WHEN '318'.
        rv_name = 'FA_OPISINNEGOTRANSPORTU'.
      WHEN '321'.
        rv_name = 'FA_NIP'.
      WHEN '322'.
        rv_name = 'FA_KODUE'.
      WHEN '323'.
        rv_name = 'FA_NRVATUE'.
      WHEN '324'.
        rv_name = 'FA_KODKRAJU'.
      WHEN '325'.
        rv_name = 'FA_NRID'.
      WHEN '326'.
        rv_name = 'FA_BRAKID'.
      WHEN '327'.
        rv_name = 'FA_NAZWA'.
      WHEN '329'.
        rv_name = 'FA_ADR_KODKRAJU'.
      WHEN '330'.
        rv_name = 'FA_ADR_ADRESL1'.
      WHEN '331'.
        rv_name = 'FA_ADR_ADRESL2'.
      WHEN '332'.
        rv_name = 'FA_ADR_GLN'.
      WHEN '333'.
        rv_name = 'FA_NRZLECENIATRANSPORTU'.
      WHEN '334'.
        rv_name = 'FA_OPISLADUNKU'.
      WHEN '335'.
        rv_name = 'FA_LADUNEKINNY'.
      WHEN '336'.
        rv_name = 'FA_OPISINNEGOLADUNKU'.
      WHEN '337'.
        rv_name = 'FA_JEDNOSTKAOPAKOWANIA'.
      WHEN '338'.
        rv_name = 'FA_DATAGODZROZPTRANSPORTU'.
      WHEN '339'.
        rv_name = 'FA_DATAGODZZAKTRANSPORTU'.
      WHEN '341'.
        rv_name = 'FA_WYSYLKAZ_KODKRAJU'.
      WHEN '342'.
        rv_name = 'FA_WYSYLKAZ_ADRESL1'.
      WHEN '343'.
        rv_name = 'FA_WYSYLKAZ_ADRESL2'.
      WHEN '344'.
        rv_name = 'FA_WYSYLKAZ_GLN'.
      WHEN '346'.
        rv_name = 'FA_WYSYLKAP_KODKRAJU'.
      WHEN '347'.
        rv_name = 'FA_WYSYLKAP_ADRESL1'.
      WHEN '348'.
        rv_name = 'FA_WYSYLKAP_ADRESL2'.
      WHEN '349'.
        rv_name = 'FA_WYSYLKAP_GLN'.
      WHEN '351'.
        rv_name = 'FA_WYSYLKAD_KODKRAJU'.
      WHEN '352'.
        rv_name = 'FA_WYSYLKAD_ADRESL1'.
      WHEN '353'.
        rv_name = 'FA_WYSYLKAD_ADRESL2'.
      WHEN '354'.
        rv_name = 'FA_WYSYLKAD_GLN'.
      WHEN '355'.
        rv_name = 'FA_PODMIOTPOSREDNICZACY'.
      WHEN '357'.
        rv_name = 'FA_WARTOSCZAMOWIENIA'.
      WHEN '359'.
        rv_name = 'FA_NRWIERSZAZAM'.
      WHEN '360'.
        rv_name = 'FA_UU_IDZ'.
      WHEN '361'.
        rv_name = 'FA_P_7Z'.
      WHEN '362'.
        rv_name = 'FA_INDEKSZ'.
      WHEN '363'.
        rv_name = 'FA_GTINZ'.
      WHEN '364'.
        rv_name = 'FA_PKWIUZ'.
      WHEN '365'.
        rv_name = 'FA_CNZ'.
      WHEN '366'.
        rv_name = 'FA_PKOBZ'.
      WHEN '367'.
        rv_name = 'FA_P_8AZ'.
      WHEN '368'.
        rv_name = 'FA_P_8BZ'.
      WHEN '369'.
        rv_name = 'FA_P_9AZ'.
      WHEN '370'.
        rv_name = 'FA_P_11NETTOZ'.
      WHEN '371'.
        rv_name = 'FA_P_11VATZ'.
      WHEN '372'.
        rv_name = 'FA_P_12Z'.
      WHEN '373'.
        rv_name = 'FA_P_12Z_XII'.
      WHEN '374'.
        rv_name = 'FA_P_12Z_ZAL_15'.
      WHEN '375'.
        rv_name = 'FA_GTUZ'.
      WHEN '376'.
        rv_name = 'FA_PROCEDURAZ'.
      WHEN '377'.
        rv_name = 'FA_KWOTAAKCYZYZ'.
      WHEN '378'.
        rv_name = 'FA_STANPRZEDZ'.
      WHEN '381'.
        rv_name = 'ST_STOPKAFAKTURY'.
      WHEN '383'.
        rv_name = 'ST_PELNANAZWA'.
      WHEN '384'.
        rv_name = 'ST_KRS'.
      WHEN '385'.
        rv_name = 'ST_REGON'.
      WHEN '386'.
        rv_name = 'ST_BDO'.

    ENDCASE.

  ENDMETHOD.


  METHOD build_subtree.
    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_value> TYPE any.

    ASSIGN ir_data->* TO <ls_data>.

    LOOP AT it_conf ASSIGNING FIELD-SYMBOL(<ls_conf>) WHERE parent_id = iv_parent_id.

      DATA lr_el TYPE REF TO if_ixml_element.

      IF <ls_conf>-is_leaf = abap_true.

        DATA(lv_comp) = me->get_name( <ls_conf>-node_id ).
        ASSIGN COMPONENT lv_comp OF STRUCTURE <ls_data> TO <lv_value>.
        IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.

          lr_el = lr_document->create_simple_element(
            EXPORTING
              name   = CONV #( <ls_conf>-field_name )
              parent = ir_parent_el
              value  = CONV #( <lv_value> )
          ).
        ENDIF.

      ELSE.

        "container node
        lr_el = lr_document->create_simple_element(
          EXPORTING
            name   = CONV #( <ls_conf>-field_name )
            parent = ir_parent_el
        ).

        me->build_subtree(
          EXPORTING
            iv_parent_id = <ls_conf>-node_id
            ir_parent_el = lr_el
            ir_data      = ir_data
            it_conf      = it_conf
        ).

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD collect_subtree_conf.
    DATA lt_queue TYPE STANDARD TABLE OF z_nod_id WITH DEFAULT KEY.

    CLEAR: et_conf, et_node_ids.

    APPEND iv_root_id TO lt_queue.

    WHILE lt_queue IS NOT INITIAL.
      READ TABLE lt_queue INDEX 1 INTO DATA(lv_curr).
      DELETE lt_queue INDEX 1.

      LOOP AT mt_ksef_xml_conf ASSIGNING FIELD-SYMBOL(<ls_conf>) WHERE parent_id = lv_curr.
        APPEND <ls_conf> TO et_conf.
        APPEND <ls_conf>-node_id TO lt_queue.
        APPEND <ls_conf>-node_id TO et_node_ids.
      ENDLOOP.
    ENDWHILE.

    SORT et_conf BY parent_id node_id.
    SORT et_node_ids.
    DELETE ADJACENT DUPLICATES FROM et_node_ids.

  ENDMETHOD.

ENDCLASS.
