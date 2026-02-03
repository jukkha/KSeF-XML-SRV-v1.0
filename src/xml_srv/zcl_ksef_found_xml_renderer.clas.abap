CLASS zcl_ksef_found_xml_renderer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS render
      IMPORTING is_invoice TYPE zif_ksef_xml_types=>ty_invoice
      RETURNING VALUE(rv_xml) TYPE string.
ENDCLASS.

CLASS zcl_ksef_found_xml_renderer IMPLEMENTATION.
  METHOD render.
    CLEAR rv_xml.
  ENDMETHOD.
ENDCLASS.
