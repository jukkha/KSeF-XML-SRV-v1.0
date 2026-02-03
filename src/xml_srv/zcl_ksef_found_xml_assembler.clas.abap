CLASS zcl_ksef_found_xml_assembler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS assemble
      IMPORTING is_repo_invoice TYPE zcl_ksef_found_xml_service=>ty_repo_invoice
      RETURNING VALUE(rs_invoice) TYPE zcl_ksef_found_xml_service=>ty_invoice.
ENDCLASS.

CLASS zcl_ksef_found_xml_assembler IMPLEMENTATION.
  METHOD assemble.
    CLEAR rs_invoice.
    rs_invoice-header-ksef_id = is_repo_invoice-ksef_id.
  ENDMETHOD.
ENDCLASS.
