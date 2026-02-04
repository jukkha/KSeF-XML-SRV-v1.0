CLASS zcl_ksef_found_xml_assembler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS assemble
      IMPORTING is_repo_invoice   TYPE zif_ksef_xml_types=>ty_repo_invoice
      RETURNING VALUE(rs_invoice) TYPE zif_ksef_xml_types=>ty_invoice.
ENDCLASS.

CLASS zcl_ksef_found_xml_assembler IMPLEMENTATION.
  METHOD assemble.
    CLEAR rs_invoice.
    rs_invoice-header-ksef_id = is_repo_invoice-ksef_id.
    rs_invoice-correction_context-xml_old = is_repo_invoice-xml_old.
  ENDMETHOD.
ENDCLASS.
