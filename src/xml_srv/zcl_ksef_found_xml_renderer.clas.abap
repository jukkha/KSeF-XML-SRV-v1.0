CLASS zcl_ksef_found_xml_renderer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS render
      IMPORTING is_invoice    TYPE zif_ksef_xml_types=>ty_invoice
                is_options    TYPE zif_ksef_xml_types=>ty_render_options OPTIONAL
      RETURNING VALUE(rv_xml) TYPE string.
  PRIVATE SECTION.
    CONSTANTS:
      gc_root_name          TYPE string VALUE 'Faktura',
      gc_ns_default         TYPE string VALUE 'http://crd.gov.pl/wzor/2025/06/25/13775/',
      gc_ns_etd             TYPE string VALUE 'http://crd.gov.pl/xml/schematy/dziedzinowe/mf/2022/01/05/eD/DefinicjeTypy/',
      gc_ns_xsi             TYPE string VALUE 'http://www.w3.org/2001/XMLSchema-instance',
      gc_kod_formularza     TYPE string VALUE 'FA',
      gc_kod_systemowy      TYPE string VALUE 'FA (3)',
      gc_wersja_schemy      TYPE string VALUE '1-0E',
      gc_wariant_formularza TYPE string VALUE '3'.

    TYPES:
      BEGIN OF ty_tag_map,
        tag_name       TYPE string,
        component_name TYPE string,
      END OF ty_tag_map,
      tt_tag_map TYPE STANDARD TABLE OF ty_tag_map WITH EMPTY KEY.

    METHODS append_header
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        is_header   TYPE zif_ksef_xml_types=>ty_invoice_header.

    METHODS append_podmiot
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        iv_tagname  TYPE string
        is_podmiot  TYPE zif_ksef_xml_types=>ty_podmiot.

    METHODS append_podmiot_list
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        iv_tagname  TYPE string
        it_podmiot  TYPE zif_ksef_xml_types=>tt_podmiot.

    METHODS append_podmiot3_list
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        it_podmiot3 TYPE zif_ksef_xml_types=>tt_podmiot3.

    METHODS append_fa_section
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        is_invoice  TYPE zif_ksef_xml_types=>ty_invoice.

    METHODS append_items
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        it_items    TYPE zif_ksef_xml_types=>tt_invoice_items.

    METHODS append_zal_items
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        it_items    TYPE zif_ksef_xml_types=>tt_zal_items.

    METHODS append_simple_tag
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        iv_tagname  TYPE string
        iv_value    TYPE string.

    METHODS append_tag_map
      IMPORTING
        ir_document TYPE REF TO if_ixml_document
        ir_parent   TYPE REF TO if_ixml_node
        is_data     TYPE any
        it_tag_map  TYPE tt_tag_map.

    METHODS get_component_value
      IMPORTING
                is_data         TYPE any
                iv_component    TYPE string
      RETURNING VALUE(rv_value) TYPE string.

    METHODS has_struct_data
      IMPORTING
                is_data            TYPE any
      RETURNING VALUE(rv_has_data) TYPE abap_bool.

    METHODS build_struct_key
      IMPORTING
                is_data       TYPE any
      RETURNING VALUE(rv_key) TYPE string.

    METHODS normalize_text
      IMPORTING
                iv_value        TYPE string
      RETURNING VALUE(rv_value) TYPE string.

    METHODS get_item_key
      IMPORTING
                is_item       TYPE zif_ksef_xml_types=>ty_invoice_item
      RETURNING VALUE(rv_key) TYPE string.

    METHODS get_zal_item_key
      IMPORTING
                is_item       TYPE zif_ksef_xml_types=>ty_zal_item
      RETURNING VALUE(rv_key) TYPE string.

    METHODS get_podmiot_key
      IMPORTING
                is_podmiot    TYPE zif_ksef_xml_types=>ty_podmiot
      RETURNING VALUE(rv_key) TYPE string.

    METHODS sort_items
      IMPORTING
                it_items        TYPE zif_ksef_xml_types=>tt_invoice_items
      RETURNING VALUE(rt_items) TYPE zif_ksef_xml_types=>tt_invoice_items.

    METHODS sort_zal_items
      IMPORTING
                it_items        TYPE zif_ksef_xml_types=>tt_zal_items
      RETURNING VALUE(rt_items) TYPE zif_ksef_xml_types=>tt_zal_items.

    METHODS sort_podmiot
      IMPORTING
                it_podmiot        TYPE zif_ksef_xml_types=>tt_podmiot
      RETURNING VALUE(rt_podmiot) TYPE zif_ksef_xml_types=>tt_podmiot.

    METHODS get_podmiot3_key
      IMPORTING
                is_podmiot3    TYPE zif_ksef_xml_types=>ty_podmiot3
      RETURNING VALUE(rv_key) TYPE string.

    METHODS sort_podmiot3
      IMPORTING
                it_podmiot3        TYPE zif_ksef_xml_types=>tt_podmiot3
      RETURNING VALUE(rt_podmiot3) TYPE zif_ksef_xml_types=>tt_podmiot3.

    METHODS pretty_print_xml
      IMPORTING
                iv_xml        TYPE string
      RETURNING VALUE(rv_xml) TYPE string.
ENDCLASS.

CLASS zcl_ksef_found_xml_renderer IMPLEMENTATION.
  METHOD render.
    DATA ls_options TYPE zif_ksef_xml_types=>ty_render_options.
    ls_options-pretty_print = abap_false.
    IF is_options IS NOT INITIAL.
      ls_options = is_options.
    ENDIF.

    DATA(lo_ixml) = cl_ixml=>create( ).
    DATA(lo_document) = lo_ixml->create_document( ).

    DATA(lo_root) = lo_document->create_simple_element(
      EXPORTING
        name   = gc_root_name
        parent = lo_document ).

    lo_root->set_attribute( name = 'xmlns' value = gc_ns_default ).
    lo_root->set_attribute( name = 'xmlns:etd' value = gc_ns_etd ).
    lo_root->set_attribute( name = 'xmlns:xsi' value = gc_ns_xsi ).

    me->append_header(
      EXPORTING
        ir_document = lo_document
        ir_parent   = lo_root
        is_header   = is_invoice-header ).

    me->append_podmiot(
      EXPORTING
        ir_document = lo_document
        ir_parent   = lo_root
        iv_tagname  = 'Podmiot1'
        is_podmiot  = is_invoice-podmiot1 ).

    me->append_podmiot(
      EXPORTING
        ir_document = lo_document
        ir_parent   = lo_root
        iv_tagname  = 'Podmiot2'
        is_podmiot  = is_invoice-podmiot2 ).

    me->append_podmiot3_list(
      EXPORTING
        ir_document = lo_document
        ir_parent   = lo_root
        it_podmiot3 = is_invoice-podmiot3 ).

    me->append_fa_section(
      EXPORTING
        ir_document = lo_document
        ir_parent   = lo_root
        is_invoice  = is_invoice ).

    DATA(lv_xml_xstring) = VALUE xstring( ).
    DATA(lo_stream_factory) = lo_ixml->create_stream_factory( ).
    DATA(lo_ostream) = lo_stream_factory->create_ostream_xstring( string = lv_xml_xstring ).

    DATA(lo_renderer) = lo_ixml->create_renderer(
      document = lo_document
      ostream  = lo_ostream ).
    lo_renderer->render( ).

    rv_xml = cl_abap_conv_codepage=>create_in( codepage = 'UTF-8' )->convert( lv_xml_xstring ).

    IF ls_options-pretty_print = abap_true.
      rv_xml = me->pretty_print_xml( rv_xml ).
    ENDIF.
  ENDMETHOD.

  METHOD append_header.
    DATA(lo_header) = ir_document->create_simple_element(
      EXPORTING
        name   = 'Naglowek'
        parent = ir_parent ).

    DATA(lv_kod_formularza) = me->get_component_value( is_data = is_header iv_component = 'NAGL_KODFORMULARZA' ).
    IF lv_kod_formularza IS INITIAL.
      lv_kod_formularza = gc_kod_formularza.
    ENDIF.

    DATA(lo_kod_form) = ir_document->create_simple_element(
      EXPORTING
        name   = 'KodFormularza'
        parent = lo_header
        value  = lv_kod_formularza ).
    lo_kod_form->set_attribute( name = 'kodSystemowy' value = gc_kod_systemowy ).
    lo_kod_form->set_attribute( name = 'wersjaSchemy' value = gc_wersja_schemy ).

    DATA(lv_variant) = me->get_component_value( is_data = is_header iv_component = 'NAGL_WARIANTFORMULARZA' ).
    IF lv_variant IS INITIAL.
      lv_variant = gc_wariant_formularza.
    ENDIF.
    me->append_simple_tag(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_header
        iv_tagname  = 'WariantFormularza'
        iv_value    = lv_variant ).

    me->append_simple_tag(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_header
        iv_tagname  = 'DataWytworzeniaFa'
        iv_value    = me->get_component_value( is_data = is_header iv_component = 'NAGL_DATAWYTWORZENIAFA' ) ).

    me->append_simple_tag(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_header
        iv_tagname  = 'SystemInfo'
        iv_value    = me->get_component_value( is_data = is_header iv_component = 'NAGL_SYSTEMINFO' ) ).
  ENDMETHOD.

  METHOD append_podmiot.
    IF me->has_struct_data( is_data = is_podmiot ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lo_podmiot) = ir_document->create_simple_element(
      EXPORTING
        name   = iv_tagname
        parent = ir_parent ).

    DATA(lt_id_map) = VALUE tt_tag_map(
      ( tag_name = 'PrefiksPodatnika' component_name = 'PREFIKSPODATNIKA' )
      ( tag_name = 'NIP'             component_name = 'NIP' )
      ( tag_name = 'KodUE'           component_name = 'KODUE' )
      ( tag_name = 'NrVatUE'         component_name = 'NRVATUE' )
      ( tag_name = 'NrID'            component_name = 'NRID' )
      ( tag_name = 'BrakID'          component_name = 'BRAKID' )
      ( tag_name = 'Nazwa'           component_name = 'NAZWA' )
      ( tag_name = 'IDNabywcy'       component_name = 'IDNABYWCY' )
      ( tag_name = 'NrKlienta'       component_name = 'NRKLIENTA' ) ).

    DATA(lv_has_ident) = abap_false.
    LOOP AT lt_id_map ASSIGNING FIELD-SYMBOL(<ls_id_check>).
      IF me->get_component_value( is_data = is_podmiot iv_component = <ls_id_check>-component_name ) IS NOT INITIAL.
        lv_has_ident = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_has_ident = abap_true.
      DATA(lo_ident) = ir_document->create_simple_element(
        EXPORTING
          name   = 'DaneIdentyfikacyjne'
          parent = lo_podmiot ).
      me->append_tag_map(
        EXPORTING
          ir_document = ir_document
          ir_parent   = lo_ident
          is_data     = is_podmiot
          it_tag_map  = lt_id_map ).
    ENDIF.

    DATA(lt_addr_map) = VALUE tt_tag_map(
      ( tag_name = 'KodKraju' component_name = 'KODKRAJU' )
      ( tag_name = 'AdresL1'  component_name = 'ADRESL1' )
      ( tag_name = 'AdresL2'  component_name = 'ADRESL2' )
      ( tag_name = 'GLN'      component_name = 'GLN' ) ).

    DATA(lv_has_addr) = abap_false.
    LOOP AT lt_addr_map ASSIGNING FIELD-SYMBOL(<ls_addr_check>).
      IF me->get_component_value( is_data = is_podmiot iv_component = <ls_addr_check>-component_name ) IS NOT INITIAL.
        lv_has_addr = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_has_addr = abap_true.
      DATA(lo_addr) = ir_document->create_simple_element(
        EXPORTING
          name   = 'Adres'
          parent = lo_podmiot ).
      me->append_tag_map(
        EXPORTING
          ir_document = ir_document
          ir_parent   = lo_addr
          is_data     = is_podmiot
          it_tag_map  = lt_addr_map ).
    ENDIF.

    DATA(lt_role_map) = VALUE tt_tag_map(
      ( tag_name = 'Rola'      component_name = 'ROLA' )
      ( tag_name = 'RolaInna'  component_name = 'ROLAINNA' )
      ( tag_name = 'OpisRoli'  component_name = 'OPISROLI' )
      ( tag_name = 'Udzial'    component_name = 'UDZIAL' ) ).

    me->append_tag_map(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_podmiot
        is_data     = is_podmiot
        it_tag_map  = lt_role_map ).
  ENDMETHOD.

  METHOD append_podmiot_list.
    DATA(lt_sorted) = me->sort_podmiot( it_podmiot ).
    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<ls_podmiot>).
      me->append_podmiot(
        EXPORTING
          ir_document = ir_document
          ir_parent   = ir_parent
          iv_tagname  = iv_tagname
          is_podmiot  = <ls_podmiot> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD append_podmiot3_list.
    DATA(lt_sorted) = me->sort_podmiot3( it_podmiot3 ).
    DATA(lt_id_map) = VALUE tt_tag_map(
      ( tag_name = 'NIP'       component_name = 'NIP' )
      ( tag_name = 'KodUE'     component_name = 'KODUE' )
      ( tag_name = 'NrVatUE'   component_name = 'NRVATUE' )
      ( tag_name = 'NrID'      component_name = 'NRID' )
      ( tag_name = 'BrakID'    component_name = 'BRAKID' )
      ( tag_name = 'Nazwa'     component_name = 'NAZWA' )
      ( tag_name = 'IDNabywcy' component_name = 'IDNABYWCY' )
      ( tag_name = 'NrKlienta' component_name = 'NRKLIENTA' ) ).

    DATA(lt_addr_map) = VALUE tt_tag_map(
      ( tag_name = 'KodKraju' component_name = 'KODKRAJU' )
      ( tag_name = 'AdresL1'  component_name = 'ADR_ADRESL1' )
      ( tag_name = 'AdresL2'  component_name = 'ADR_ADRESL2' )
      ( tag_name = 'GLN'      component_name = 'GLN' ) ).

    DATA(lt_role_map) = VALUE tt_tag_map(
      ( tag_name = 'Rola'      component_name = 'ROLA' )
      ( tag_name = 'RolaInna'  component_name = 'ROLAINNA' )
      ( tag_name = 'OpisRoli'  component_name = 'OPISROLI' )
      ( tag_name = 'Udzial'    component_name = 'UDZIAL' ) ).

    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<ls_podmiot3>).
      DATA(lo_podmiot3) = ir_document->create_simple_element(
        EXPORTING
          name   = 'Podmiot3'
          parent = ir_parent ).

      DATA(lv_has_ident) = abap_false.
      LOOP AT lt_id_map ASSIGNING FIELD-SYMBOL(<ls_id_check>).
        IF me->get_component_value( is_data = <ls_podmiot3> iv_component = <ls_id_check>-component_name ) IS NOT INITIAL.
          lv_has_ident = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_has_ident = abap_true.
        DATA(lo_ident) = ir_document->create_simple_element(
          EXPORTING
            name   = 'DaneIdentyfikacyjne'
            parent = lo_podmiot3 ).
        me->append_tag_map(
          EXPORTING
            ir_document = ir_document
            ir_parent   = lo_ident
            is_data     = <ls_podmiot3>
            it_tag_map  = lt_id_map ).
      ENDIF.

      DATA(lv_has_addr) = abap_false.
      LOOP AT lt_addr_map ASSIGNING FIELD-SYMBOL(<ls_addr_check>).
        IF me->get_component_value( is_data = <ls_podmiot3> iv_component = <ls_addr_check>-component_name ) IS NOT INITIAL.
          lv_has_addr = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_has_addr = abap_true.
        DATA(lo_addr) = ir_document->create_simple_element(
          EXPORTING
            name   = 'Adres'
            parent = lo_podmiot3 ).
        me->append_tag_map(
          EXPORTING
            ir_document = ir_document
            ir_parent   = lo_addr
            is_data     = <ls_podmiot3>
            it_tag_map  = lt_addr_map ).
      ENDIF.

      me->append_tag_map(
        EXPORTING
          ir_document = ir_document
          ir_parent   = lo_podmiot3
          is_data     = <ls_podmiot3>
          it_tag_map  = lt_role_map ).
    ENDLOOP.
  ENDMETHOD.

  METHOD append_fa_section.
    DATA(lv_has_section) = abap_false.

    IF me->has_struct_data( is_data = is_invoice-header ) = abap_true.
      lv_has_section = abap_true.
    ENDIF.
    IF lines( is_invoice-items ) > 0 OR lines( is_invoice-zal_items ) > 0.
      lv_has_section = abap_true.
    ENDIF.
    IF me->has_struct_data( is_data = is_invoice-podmiot1k ) = abap_true
       OR lines( is_invoice-podmiot2k ) > 0.
      lv_has_section = abap_true.
    ENDIF.

    IF lv_has_section = abap_false.
      RETURN.
    ENDIF.

    DATA(lo_fa) = ir_document->create_simple_element(
      EXPORTING
        name   = 'Fa'
        parent = ir_parent ).

    DATA(lt_fa_map) = VALUE tt_tag_map(
      ( tag_name = 'RodzajFaktury'       component_name = 'FA_RODZAJFAKTURY' )
      ( tag_name = 'KodWaluty'           component_name = 'FA_KODWALUTY' )
      ( tag_name = 'P_1'                 component_name = 'FA_P_1' )
      ( tag_name = 'P_2'                 component_name = 'FA_P_2' )
      ( tag_name = 'DataWystFaKorygowanej' component_name = 'FA_DATAWYSTFAKORYGOWANEJ' )
      ( tag_name = 'NrFaKorygowanej'     component_name = 'FA_NRFAKORYGOWANEJ' )
      ( tag_name = 'P_15'                component_name = 'FA_P_15' )
      ( tag_name = 'KursWalutyZ'         component_name = 'FA_KURSWALUTYZ' ) ).

    me->append_tag_map(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_fa
        is_data     = is_invoice-header
        it_tag_map  = lt_fa_map ).

    me->append_podmiot(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_fa
        iv_tagname  = 'Podmiot1K'
        is_podmiot  = is_invoice-podmiot1k ).

    me->append_podmiot_list(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_fa
        iv_tagname  = 'Podmiot2K'
        it_podmiot  = is_invoice-podmiot2k ).

    me->append_items(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_fa
        it_items    = is_invoice-items ).

    me->append_zal_items(
      EXPORTING
        ir_document = ir_document
        ir_parent   = lo_fa
        it_items    = is_invoice-zal_items ).
  ENDMETHOD.

  METHOD append_items.
    DATA(lt_sorted) = me->sort_items( it_items ).
    DATA(lt_item_map) = VALUE tt_tag_map(
      ( tag_name = 'NrWierszaFa' component_name = 'NRWIERSZAFA' )
      ( tag_name = 'UU_ID'        component_name = 'UU_ID' )
      ( tag_name = 'P_6A'         component_name = 'P_6A' )
      ( tag_name = 'P_7'          component_name = 'P_7' )
      ( tag_name = 'Indeks'       component_name = 'INDEKS' )
      ( tag_name = 'GTIN'         component_name = 'GTIN' )
      ( tag_name = 'PKWIU'        component_name = 'PKWIU' )
      ( tag_name = 'CN'           component_name = 'CN' )
      ( tag_name = 'PKOB'         component_name = 'PKOB' )
      ( tag_name = 'P_8A'         component_name = 'P_8A' )
      ( tag_name = 'P_8B'         component_name = 'P_8B' )
      ( tag_name = 'P_9A'         component_name = 'P_9A' )
      ( tag_name = 'P_9B'         component_name = 'P_9B' )
      ( tag_name = 'P_10'         component_name = 'P_10' )
      ( tag_name = 'P_11'         component_name = 'P_11' )
      ( tag_name = 'P_11A'        component_name = 'P_11A' )
      ( tag_name = 'P_11Vat'      component_name = 'P_11VAT' )
      ( tag_name = 'P_12'         component_name = 'P_12' )
      ( tag_name = 'P_12_XII'     component_name = 'P_12_XII' )
      ( tag_name = 'P_12_Zal_15'  component_name = 'P_12_ZAL_15' )
      ( tag_name = 'KwotaAkcyzy'  component_name = 'KWOTAAKCYZY' )
      ( tag_name = 'GTU'          component_name = 'GTU' )
      ( tag_name = 'Procedura'    component_name = 'PROCEDURA' )
      ( tag_name = 'KursWaluty'   component_name = 'KURSWALUTY' )
      ( tag_name = 'StanPrzed'    component_name = 'STANPRZED' ) ).

    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<ls_item>).
      DATA(lo_row) = ir_document->create_simple_element(
        EXPORTING
          name   = 'FaWiersz'
          parent = ir_parent ).

      me->append_tag_map(
        EXPORTING
          ir_document = ir_document
          ir_parent   = lo_row
          is_data     = <ls_item>
          it_tag_map  = lt_item_map ).
    ENDLOOP.
  ENDMETHOD.

  METHOD append_zal_items.
    DATA(lt_sorted) = me->sort_zal_items( it_items ).
    DATA(lt_item_map) = VALUE tt_tag_map(
      ( tag_name = 'NrWierszaZam' component_name = 'NRWIERSZAZAM' )
      ( tag_name = 'UU_IDZ'        component_name = 'UU_IDZ' )
      ( tag_name = 'P_7Z'          component_name = 'P_7Z' )
      ( tag_name = 'IndeksZ'       component_name = 'INDEKSZ' )
      ( tag_name = 'GTINZ'         component_name = 'GTINZ' )
      ( tag_name = 'PKWiUZ'        component_name = 'PKWIUZ' )
      ( tag_name = 'CNZ'           component_name = 'CNZ' )
      ( tag_name = 'PKOBZ'         component_name = 'PKOBZ' )
      ( tag_name = 'P_8AZ'         component_name = 'P_8AZ' )
      ( tag_name = 'P_8BZ'         component_name = 'P_8BZ' )
      ( tag_name = 'P_9AZ'         component_name = 'P_9AZ' )
      ( tag_name = 'P_11NettoZ'    component_name = 'P_11NETTOZ' )
      ( tag_name = 'P_11VatZ'      component_name = 'P_11VATZ' )
      ( tag_name = 'P_12Z'         component_name = 'P_12Z' )
      ( tag_name = 'P_12Z_XII'     component_name = 'P_12Z_XII' )
      ( tag_name = 'P_12Z_Zal_15'  component_name = 'P_12Z_ZAL_15' )
      ( tag_name = 'KwotaAkcyzyZ'  component_name = 'KWOTAAKCYZYZ' )
      ( tag_name = 'GTUZ'          component_name = 'GTUZ' )
      ( tag_name = 'ProceduraZ'    component_name = 'PROCEDURAZ' )
      ( tag_name = 'StanPrzedZ'    component_name = 'STANPRZEDZ' ) ).

    LOOP AT lt_sorted ASSIGNING FIELD-SYMBOL(<ls_item>).
      DATA(lo_row) = ir_document->create_simple_element(
        EXPORTING
          name   = 'ZamowienieWiersz'
          parent = ir_parent ).

      me->append_tag_map(
        EXPORTING
          ir_document = ir_document
          ir_parent   = lo_row
          is_data     = <ls_item>
          it_tag_map  = lt_item_map ).
    ENDLOOP.
  ENDMETHOD.

  METHOD append_simple_tag.
    IF iv_value IS INITIAL.
      RETURN.
    ENDIF.

    ir_document->create_simple_element(
      EXPORTING
        name   = iv_tagname
        parent = ir_parent
        value  = iv_value ).
  ENDMETHOD.

  METHOD append_tag_map.
    LOOP AT it_tag_map ASSIGNING FIELD-SYMBOL(<ls_map>).
      DATA(lv_value) = me->get_component_value( is_data = is_data iv_component = <ls_map>-component_name ).
      IF lv_value IS NOT INITIAL.
        me->append_simple_tag(
          EXPORTING
            ir_document = ir_document
            ir_parent   = ir_parent
            iv_tagname  = <ls_map>-tag_name
            iv_value    = lv_value ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_component_value.
    rv_value = ``.

    FIELD-SYMBOLS <lv_value> TYPE any.
    ASSIGN COMPONENT iv_component OF STRUCTURE is_data TO <lv_value>.
    IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
      rv_value = |{ <lv_value> }|.
    ENDIF.
  ENDMETHOD.

  METHOD has_struct_data.
    rv_has_data = abap_false.

    DATA(lo_desc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( is_data ) ).
    LOOP AT lo_desc->components INTO DATA(ls_comp).
      FIELD-SYMBOLS <lv_value> TYPE any.
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_data TO <lv_value>.
      IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
        rv_has_data = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_struct_key.
    rv_key = ``.

    DATA(lo_desc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( is_data ) ).
    LOOP AT lo_desc->components INTO DATA(ls_comp).
      FIELD-SYMBOLS <lv_value> TYPE any.
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE is_data TO <lv_value>.
      IF sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
        rv_key = |{ rv_key }| && `|` && ls_comp-name && `=` && me->normalize_text( |{ <lv_value> }| ).
      ENDIF.
    ENDLOOP.
    CONDENSE rv_key NO-GAPS.
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

  METHOD get_podmiot_key.
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

  METHOD sort_items.
    TYPES: BEGIN OF ty_item_sort,
             key  TYPE string,
             item TYPE zif_ksef_xml_types=>ty_invoice_item,
           END OF ty_item_sort.

    DATA lt_sorted TYPE STANDARD TABLE OF ty_item_sort WITH EMPTY KEY.
    LOOP AT it_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      DATA(lv_key) = me->get_item_key( <ls_item> ).
      IF lv_key IS INITIAL.
        lv_key = me->build_struct_key( <ls_item> ).
      ENDIF.
      APPEND VALUE #( key = lv_key item = <ls_item> ) TO lt_sorted.
    ENDLOOP.

    SORT lt_sorted BY key.
    rt_items = VALUE #( FOR ls_row IN lt_sorted ( ls_row-item ) ).
  ENDMETHOD.

  METHOD sort_zal_items.
    TYPES: BEGIN OF ty_item_sort,
             key  TYPE string,
             item TYPE zif_ksef_xml_types=>ty_zal_item,
           END OF ty_item_sort.

    DATA lt_sorted TYPE STANDARD TABLE OF ty_item_sort WITH EMPTY KEY.
    LOOP AT it_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      DATA(lv_key) = me->get_zal_item_key( <ls_item> ).
      IF lv_key IS INITIAL.
        lv_key = me->build_struct_key( <ls_item> ).
      ENDIF.
      APPEND VALUE #( key = lv_key item = <ls_item> ) TO lt_sorted.
    ENDLOOP.

    SORT lt_sorted BY key.
    rt_items = VALUE #( FOR ls_row IN lt_sorted ( ls_row-item ) ).
  ENDMETHOD.

  METHOD sort_podmiot.
    TYPES: BEGIN OF ty_pod_sort,
             key     TYPE string,
             podmiot TYPE zif_ksef_xml_types=>ty_podmiot,
           END OF ty_pod_sort.

    DATA lt_sorted TYPE STANDARD TABLE OF ty_pod_sort WITH EMPTY KEY.
    LOOP AT it_podmiot ASSIGNING FIELD-SYMBOL(<ls_podmiot>).
      DATA(lv_key) = me->get_podmiot_key( <ls_podmiot> ).
      IF lv_key IS INITIAL.
        lv_key = me->build_struct_key( <ls_podmiot> ).
      ENDIF.
      APPEND VALUE #( key = lv_key podmiot = <ls_podmiot> ) TO lt_sorted.
    ENDLOOP.

    SORT lt_sorted BY key.
    rt_podmiot = VALUE #( FOR ls_row IN lt_sorted ( ls_row-podmiot ) ).
  ENDMETHOD.


  METHOD get_podmiot3_key.
    rv_key = ``.

    IF is_podmiot3-rola IS NOT INITIAL.
      rv_key = |{ me->normalize_text( |{ is_podmiot3-rola }| ) }|.
    ENDIF.

    IF is_podmiot3-nip IS NOT INITIAL.
      rv_key = |{ rv_key }| && `|NIP:` && me->normalize_text( |{ is_podmiot3-nip }| ).
    ELSEIF is_podmiot3-nrid IS NOT INITIAL.
      rv_key = |{ rv_key }| && `|ID:` && me->normalize_text( |{ is_podmiot3-nrid }| ).
    ELSEIF is_podmiot3-idnabywcy IS NOT INITIAL.
      rv_key = |{ rv_key }| && `|IDN:` && me->normalize_text( |{ is_podmiot3-idnabywcy }| ).
    ENDIF.

    CONDENSE rv_key NO-GAPS.
  ENDMETHOD.

  METHOD sort_podmiot3.
    TYPES: BEGIN OF ty_pod_sort,
             key     TYPE string,
             podmiot TYPE zif_ksef_xml_types=>ty_podmiot3,
           END OF ty_pod_sort.

    DATA lt_sorted TYPE STANDARD TABLE OF ty_pod_sort WITH EMPTY KEY.
    LOOP AT it_podmiot3 ASSIGNING FIELD-SYMBOL(<ls_podmiot3>).
      DATA(lv_key) = me->get_podmiot3_key( <ls_podmiot3> ).
      IF lv_key IS INITIAL.
        lv_key = me->build_struct_key( <ls_podmiot3> ).
      ENDIF.
      APPEND VALUE #( key = lv_key podmiot = <ls_podmiot3> ) TO lt_sorted.
    ENDLOOP.

    SORT lt_sorted BY key.
    rt_podmiot3 = VALUE #( FOR ls_row IN lt_sorted ( ls_row-podmiot ) ).
  ENDMETHOD.

  METHOD pretty_print_xml.
    rv_xml = iv_xml.
    IF rv_xml IS INITIAL.
      RETURN.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '><' IN rv_xml WITH |>{ cl_abap_char_utilities=>newline }<|.

    DATA lt_lines TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    SPLIT rv_xml AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    DATA(lv_indent) = 0.
    DATA(lv_step) = 2.
    DATA lt_pretty TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<lv_line>).
      DATA(lv_trimmed) = <lv_line>.
      CONDENSE lv_trimmed NO-GAPS.

      IF lv_trimmed CP '</*'.
        IF lv_indent >= lv_step.
          lv_indent = lv_indent - lv_step.
        ENDIF.
      ENDIF.

      DATA(lv_prefix) = repeat( val = ` ` occ = lv_indent ).
      APPEND |{ lv_prefix }{ <lv_line> }| TO lt_pretty.

      IF lv_trimmed CP '<*'
         AND lv_trimmed NP '</*'
         AND lv_trimmed NP '<?*'
         AND lv_trimmed NP '<!*>'
         AND lv_trimmed NP '*/*>'.
        lv_indent = lv_indent + lv_step.
      ENDIF.
    ENDLOOP.

    rv_xml = concat_lines_of( table = lt_pretty sep = cl_abap_char_utilities=>newline ).
  ENDMETHOD.
ENDCLASS.
