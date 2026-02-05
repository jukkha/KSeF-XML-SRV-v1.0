INTERFACE zif_ksef_xml_types
  PUBLIC .

  TYPES:
    BEGIN OF ty_xml_request,
      ksef_id TYPE zlx_ksef_id,
    END OF ty_xml_request,
    tt_xml_request TYPE STANDARD TABLE OF ty_xml_request WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_message,
      severity   TYPE symsgty,
      code       TYPE string,
      text       TYPE string,
      ksef_id    TYPE zlx_ksef_id,
      item_no    TYPE string,
      field_name TYPE string,
    END OF ty_message,
    tt_message TYPE STANDARD TABLE OF ty_message WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_render_options,
      pretty_print TYPE abap_bool,
    END OF ty_render_options.

  TYPES:
    BEGIN OF ty_xml_result,
      ksef_id       TYPE zlx_ksef_id,
      xml_string    TYPE string,
      status        TYPE char1,
      is_correction TYPE abap_bool,
      has_diff      TYPE abap_bool,
      messages      TYPE tt_message,
    END OF ty_xml_result,
    tt_xml_result TYPE STANDARD TABLE OF ty_xml_result WITH KEY ksef_id.

  TYPES:
    ty_diff_mode TYPE c LENGTH 11.

  CONSTANTS:
    gc_diff_mode_full        TYPE ty_diff_mode VALUE 'FULL',
    gc_diff_mode_totals_only TYPE ty_diff_mode VALUE 'TOTALS_ONLY'.

  TYPES:
    ty_diff_key  TYPE string,
    tt_diff_keys TYPE STANDARD TABLE OF ty_diff_key WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_diff_result,
      changed_items         TYPE abap_bool,
      changed_zal_items     TYPE abap_bool,
      changed_parties       TYPE abap_bool,
      changed_podmiot1      TYPE abap_bool,
      changed_podmiot2      TYPE abap_bool,
      changed_podmiot3      TYPE abap_bool,
      changed_amounts       TYPE abap_bool,
      changed_totals        TYPE abap_bool,
      changed_item_keys     TYPE tt_diff_keys,
      changed_zal_item_keys TYPE tt_diff_keys,
      changed_podmiot3_keys TYPE tt_diff_keys,
    END OF ty_diff_result.

  TYPES:
    BEGIN OF ty_invoice_header,
      INCLUDE TYPE zksef_s_head.
  TYPES:
      ksef_id TYPE zlx_ksef_id,
    END OF ty_invoice_header.

  TYPES:
    ty_invoice_item  TYPE zksef_s_item,
    tt_invoice_items TYPE STANDARD TABLE OF ty_invoice_item WITH EMPTY KEY.

  TYPES:
    ty_podmiot TYPE zlx_ksef_podmiot,
    tt_podmiot TYPE STANDARD TABLE OF ty_podmiot WITH EMPTY KEY.

  TYPES:
    ty_zal_item  TYPE zlx_ksef_zal_items,
    tt_zal_items TYPE STANDARD TABLE OF ty_zal_item WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_correction_context,
      xml_old TYPE string,
    END OF ty_correction_context.

  TYPES:
    BEGIN OF ty_invoice,
      header             TYPE ty_invoice_header,
      podmiot1           TYPE ty_podmiot,
      podmiot1k          TYPE ty_podmiot,
      podmiot2           TYPE ty_podmiot,
      podmiot2k          TYPE tt_podmiot,
      podmiot3           TYPE tt_podmiot,
      items              TYPE tt_invoice_items,
      zal_items          TYPE tt_zal_items,
      correction_context TYPE ty_correction_context,
    END OF ty_invoice.

  TYPES:
    BEGIN OF ty_repo_main_info,
      v_waerk  TYPE vbrk-waerk,
      v_vbeln  TYPE vbrk-vbeln,
      v_netwr  TYPE vbrk-netwr,
      v_fkart  TYPE vbrk-fkart,
      v_sppord TYPE vbrk-sppord,
      t_mwskz  TYPE bset-mwskz,
      t_hwste  TYPE bset-hwste,
      t_hwbas  TYPE bset-hwbas,
      t_fwste  TYPE bset-fwste,
      t_h2ste  TYPE bset-h2ste,
      t_fwbas  TYPE bset-fwbas,
      b_waers  TYPE bkpf-waers,
      b_bldat  TYPE bkpf-bldat,
      b_blart  TYPE bkpf-blart,
      b_cpudt  TYPE bkpf-cpudt,
      b_cputm  TYPE bkpf-cputm,
      b_kursf  TYPE bkpf-kursf,
      g_wrbtr  TYPE bseg-wrbtr,
    END OF ty_repo_main_info.

  TYPES:
    BEGIN OF ty_repo_correction,
      ksef_num    TYPE zlx_ksef_out-ksef_num,
      xmlgen_date TYPE zlx_ksef_out-xmlgen_date,
      xml         TYPE zlx_ksef_out-xml,
      vbeln       TYPE vbrk-vbeln,
      stceg       TYPE vbrk-stceg,
      bukrs       TYPE bseg-bukrs,
      gjahr       TYPE bseg-gjahr,
      rebzg       TYPE bseg-rebzg,
      rebzj       TYPE bseg-rebzj,
      kidno       TYPE bseg-kidno,
    END OF ty_repo_correction.

  TYPES:
    BEGIN OF ty_repo_vendor_address,
      name1      TYPE adrc-name1,
      city1      TYPE adrc-city1,
      post_code1 TYPE adrc-post_code1,
      street     TYPE adrc-street,
      house_num1 TYPE adrc-house_num1,
      stceg      TYPE t001-stceg,
      country    TYPE adrc-country,
      land1      TYPE t001-land1,
      butxt      TYPE t001-butxt,
    END OF ty_repo_vendor_address.

  TYPES:
    BEGIN OF ty_repo_customer_address,
      kunnr TYPE kna1-kunnr,
      name1 TYPE kna1-name1,
      ort01 TYPE kna1-ort01,
      pstlz TYPE kna1-pstlz,
      stras TYPE kna1-stras,
      stceg TYPE kna1-stceg,
      land1 TYPE kna1-land1,
      zterm TYPE knb1-zterm,
    END OF ty_repo_customer_address.

  TYPES:
    BEGIN OF ty_repo_item,
      vbeln TYPE vbrk-vbeln,
      fkdat TYPE vbrk-fkdat,
      bukrs TYPE vbrk-bukrs,
      waerk TYPE vbrk-waerk,
      kurrf TYPE vbrk-kurrf,
      fkart TYPE vbrk-fkart,
      knumv TYPE vbrk-knumv,
      posnr TYPE vbrp-posnr,
      arktx TYPE vbrp-arktx,
      matnr TYPE vbrp-matnr,
      vrkme TYPE vbrp-vrkme,
      fkimg TYPE vbrp-fkimg,
      netwr TYPE vbrp-netwr,
      mwskz TYPE vbrp-mwskz,
      lmeng TYPE vbrp-lmeng,
      mwsbp TYPE vbrp-mwsbp,
      cmpre TYPE vbrp-cmpre,
      kzwi1 TYPE vbrp-kzwi1,
      kzwi2 TYPE vbrp-kzwi2,
    END OF ty_repo_item,
    tt_repo_items TYPE STANDARD TABLE OF ty_repo_item WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_repo_podmiot3,
      vbeln TYPE vbpa-vbeln,
      kunnr TYPE kna1-kunnr,
      name1 TYPE kna1-name1,
      ort01 TYPE kna1-ort01,
      pstlz TYPE kna1-pstlz,
      stras TYPE kna1-stras,
      stceg TYPE kna1-stceg,
      land1 TYPE kna1-land1,
      parvw TYPE vbpa-parvw,
      vtext TYPE tpart-vtext,
    END OF ty_repo_podmiot3,
    tt_repo_podmiot3 TYPE STANDARD TABLE OF ty_repo_podmiot3 WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_repo_invoice,
      ksef_id          TYPE zlx_ksef_id,
      bukrs            TYPE bukrs,
      gjahr            TYPE gjahr,
      docnum           TYPE vbrk-vbeln,
      belnr            TYPE belnr_d,
      main_info        TYPE ty_repo_main_info,
      correction       TYPE ty_repo_correction,
      vendor_address   TYPE ty_repo_vendor_address,
      customer_address TYPE ty_repo_customer_address,
      items            TYPE tt_repo_items,
      podmiot3         TYPE tt_repo_podmiot3,
      xml_old          TYPE string,
      messages         TYPE tt_message,
    END OF ty_repo_invoice,
    tt_repo_invoices TYPE STANDARD TABLE OF ty_repo_invoice WITH EMPTY KEY.

ENDINTERFACE.
