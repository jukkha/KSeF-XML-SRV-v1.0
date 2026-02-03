INTERFACE zif_ksef_xml_types
  PUBLIC .

  TYPES:
    BEGIN OF ty_xml_request,
      ksef_id TYPE zlx_ksef_id,
    END OF ty_xml_request,
    tt_xml_request TYPE STANDARD TABLE OF ty_xml_request WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_xml_result,
      ksef_id    TYPE zlx_ksef_id,
      xml_string TYPE string,
      status     TYPE char1,
      messages   TYPE zkstg_t_message,
    END OF ty_xml_result,
    tt_xml_result TYPE STANDARD TABLE OF ty_xml_result WITH KEY ksef_id.

  TYPES:
    BEGIN OF ty_invoice_header,
      ksef_id TYPE zlx_ksef_id,
    END OF ty_invoice_header.

  TYPES:
    ty_invoice_item TYPE zksef_s_item,
    tt_invoice_items TYPE STANDARD TABLE OF ty_invoice_item WITH EMPTY KEY.

  TYPES:
    ty_podmiot TYPE zlx_ksef_podmiot,
    tt_podmiot TYPE STANDARD TABLE OF ty_podmiot WITH EMPTY KEY.

  TYPES:
    ty_zal_item TYPE zlx_ksef_zal_items,
    tt_zal_items TYPE STANDARD TABLE OF ty_zal_item WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_correction_context,
      xml_old TYPE string,
    END OF ty_correction_context.

  TYPES:
    BEGIN OF ty_invoice,
      header             TYPE ty_invoice_header,
      podmiot1           TYPE ty_podmiot,
      podmiot2           TYPE ty_podmiot,
      podmiot3           TYPE tt_podmiot,
      items              TYPE tt_invoice_items,
      zal_items          TYPE tt_zal_items,
      correction_context TYPE ty_correction_context,
    END OF ty_invoice.

  TYPES:
    BEGIN OF ty_repo_invoice,
      ksef_id TYPE zlx_ksef_id,
    END OF ty_repo_invoice,
    tt_repo_invoices TYPE STANDARD TABLE OF ty_repo_invoice WITH EMPTY KEY.

ENDINTERFACE.
