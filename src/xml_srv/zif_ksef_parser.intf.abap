INTERFACE zif_ksef_parser
  PUBLIC .


  METHODS get_value_by_name
    IMPORTING iv_elem         TYPE REF TO if_ixml_element
              iv_filter_name  TYPE string
    RETURNING VALUE(rv_value) TYPE string.

  METHODS parse_header
    IMPORTING iv_xml_raw TYPE string
    EXPORTING es_header  TYPE zkstg_header
              et_msg     TYPE zkstg_t_msg
    RAISING   cx_static_check.

  METHODS parse_items
    IMPORTING iv_xml_raw TYPE string
    EXPORTING
              et_items   TYPE zkstg_t_item
              et_msg     TYPE zkstg_t_msg
    RAISING   cx_static_check.

  METHODS validate
    IMPORTING is_header TYPE zkstg_header
              it_items  TYPE zkstg_t_item
    EXPORTING et_msg    TYPE zkstg_t_msg
    RAISING   cx_static_check.
ENDINTERFACE.
