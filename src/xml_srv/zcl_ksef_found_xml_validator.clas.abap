CLASS zcl_ksef_found_xml_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS validate
      IMPORTING iv_xml TYPE string
      RETURNING VALUE(rt_messages) TYPE zkstg_t_message.
ENDCLASS.

CLASS zcl_ksef_found_xml_validator IMPLEMENTATION.
  METHOD validate.
    CLEAR rt_messages.
  ENDMETHOD.
ENDCLASS.
