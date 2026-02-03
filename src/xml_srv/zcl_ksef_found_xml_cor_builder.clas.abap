CLASS zcl_ksef_found_xml_cor_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS build
      IMPORTING is_old TYPE zcl_ksef_found_xml_service=>ty_invoice
                is_new TYPE zcl_ksef_found_xml_service=>ty_invoice
      RETURNING VALUE(rs_invoice) TYPE zcl_ksef_found_xml_service=>ty_invoice.
ENDCLASS.

CLASS zcl_ksef_found_xml_cor_builder IMPLEMENTATION.
  METHOD build.
    rs_invoice = is_new.
  ENDMETHOD.
ENDCLASS.
