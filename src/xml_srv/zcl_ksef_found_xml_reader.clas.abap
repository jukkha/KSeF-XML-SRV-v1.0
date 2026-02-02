CLASS zcl_ksef_found_xml_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS read_podmiot
      IMPORTING iv_xml     TYPE string
                iv_tagname TYPE string
      RETURNING VALUE(rs_podmiot) TYPE zcl_ksef_found_xml_service=>ty_podmiot.

    METHODS read_podmiot3_list
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_podmiot) TYPE zcl_ksef_found_xml_service=>tt_podmiot.

    METHODS read_items
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_items) TYPE zcl_ksef_found_xml_service=>tt_invoice_items.

    METHODS read_zal_items
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_items) TYPE zcl_ksef_found_xml_service=>tt_zal_items.

    METHODS read_simple_tag
      IMPORTING iv_xml     TYPE string
                iv_tagname TYPE string
      RETURNING VALUE(rv_value) TYPE string.
ENDCLASS.

CLASS zcl_ksef_found_xml_reader IMPLEMENTATION.
  METHOD read_podmiot.
    CLEAR rs_podmiot.
  ENDMETHOD.

  METHOD read_podmiot3_list.
    CLEAR rt_podmiot.
  ENDMETHOD.

  METHOD read_items.
    CLEAR rt_items.
  ENDMETHOD.

  METHOD read_zal_items.
    CLEAR rt_items.
  ENDMETHOD.

  METHOD read_simple_tag.
    CLEAR rv_value.
  ENDMETHOD.
ENDCLASS.
