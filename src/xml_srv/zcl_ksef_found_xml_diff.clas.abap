CLASS zcl_ksef_found_xml_diff DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS diff_podmiot
      IMPORTING is_old TYPE zif_ksef_xml_types=>ty_podmiot
                is_new TYPE zif_ksef_xml_types=>ty_podmiot
      RETURNING VALUE(rv_diff) TYPE abap_bool.

    METHODS diff_items
      IMPORTING it_old TYPE zif_ksef_xml_types=>tt_invoice_items
                it_new TYPE zif_ksef_xml_types=>tt_invoice_items
      RETURNING VALUE(rv_diff) TYPE abap_bool.

    METHODS diff_zal_items
      IMPORTING it_old TYPE zif_ksef_xml_types=>tt_zal_items
                it_new TYPE zif_ksef_xml_types=>tt_zal_items
      RETURNING VALUE(rv_diff) TYPE abap_bool.
ENDCLASS.

CLASS zcl_ksef_found_xml_diff IMPLEMENTATION.
  METHOD diff_podmiot.
    rv_diff = abap_false.
  ENDMETHOD.

  METHOD diff_items.
    rv_diff = abap_false.
  ENDMETHOD.

  METHOD diff_zal_items.
    rv_diff = abap_false.
  ENDMETHOD.
ENDCLASS.
