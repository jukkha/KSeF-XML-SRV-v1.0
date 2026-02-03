CLASS zcl_ksef_found_xml_cor_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS build
      IMPORTING is_old TYPE zif_ksef_xml_types=>ty_invoice
                is_new TYPE zif_ksef_xml_types=>ty_invoice
      RETURNING VALUE(rs_invoice) TYPE zif_ksef_xml_types=>ty_invoice.
ENDCLASS.

CLASS zcl_ksef_found_xml_cor_builder IMPLEMENTATION.
  METHOD build.
    rs_invoice = is_new.
  ENDMETHOD.
ENDCLASS.
