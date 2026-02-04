CLASS zcl_ksef_found_xml_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS read_batch
      IMPORTING it_ksef_ids TYPE zkstg_t_inv_key
      RETURNING VALUE(rt_invoices) TYPE zif_ksef_xml_types=>tt_repo_invoices.
ENDCLASS.

CLASS zcl_ksef_found_xml_repository IMPLEMENTATION.
  METHOD read_batch.
    CLEAR rt_invoices.
    LOOP AT it_ksef_ids INTO DATA(lv_ksef_id).
      APPEND VALUE #( ksef_id = lv_ksef_id ) TO rt_invoices.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
