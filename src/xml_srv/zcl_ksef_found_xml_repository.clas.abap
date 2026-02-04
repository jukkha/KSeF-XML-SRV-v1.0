CLASS zcl_ksef_found_xml_repository DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS read_batch
      IMPORTING it_ksef_ids        TYPE zkstg_t_inv_key
      RETURNING VALUE(rt_invoices) TYPE zif_ksef_xml_types=>tt_repo_invoices.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_ksef_mapping,
        ksef_id TYPE zlx_ksef_id,
        docnum  TYPE vbrk-vbeln,
      END OF ty_ksef_mapping,
      tt_ksef_mapping TYPE HASHED TABLE OF ty_ksef_mapping WITH UNIQUE KEY ksef_id.

    TYPES:
      BEGIN OF ty_ksef_id_entry,
        ksef_id TYPE zlx_ksef_id,
      END OF ty_ksef_id_entry,
      tt_ksef_id_entry TYPE SORTED TABLE OF ty_ksef_id_entry WITH UNIQUE KEY ksef_id.

    TYPES:
      BEGIN OF ty_docnum_entry,
        docnum TYPE vbrk-vbeln,
      END OF ty_docnum_entry,
      tt_docnum_entry TYPE SORTED TABLE OF ty_docnum_entry WITH UNIQUE KEY docnum.

    TYPES:
      BEGIN OF ty_bukrs_entry,
        bukrs TYPE bukrs,
      END OF ty_bukrs_entry,
      tt_bukrs_entry TYPE SORTED TABLE OF ty_bukrs_entry WITH UNIQUE KEY bukrs.

    TYPES:
      BEGIN OF ty_vbrk_base,
        vbeln  TYPE vbrk-vbeln,
        bukrs  TYPE vbrk-bukrs,
        gjahr  TYPE vbrk-gjahr,
        belnr  TYPE vbrk-belnr,
        waerk  TYPE vbrk-waerk,
        netwr  TYPE vbrk-netwr,
        fkart  TYPE vbrk-fkart,
        sppord TYPE vbrk-sppord,
        fkdat  TYPE vbrk-fkdat,
        kurrf  TYPE vbrk-kurrf,
        knumv  TYPE vbrk-knumv,
        kunag  TYPE vbrk-kunag,
        stceg  TYPE vbrk-stceg,
      END OF ty_vbrk_base,
      tt_vbrk_base TYPE SORTED TABLE OF ty_vbrk_base WITH UNIQUE KEY vbeln.

    TYPES:
      BEGIN OF ty_bkpf_data,
        bukrs TYPE bkpf-bukrs,
        gjahr TYPE bkpf-gjahr,
        belnr TYPE bkpf-belnr,
        waers TYPE bkpf-waers,
        bldat TYPE bkpf-bldat,
        blart TYPE bkpf-blart,
        cpudt TYPE bkpf-cpudt,
        cputm TYPE bkpf-cputm,
        kursf TYPE bkpf-kursf,
      END OF ty_bkpf_data,
      tt_bkpf_data TYPE HASHED TABLE OF ty_bkpf_data WITH UNIQUE KEY bukrs gjahr belnr.

    TYPES:
      BEGIN OF ty_bset_data,
        bukrs TYPE bset-bukrs,
        gjahr TYPE bset-gjahr,
        belnr TYPE bset-belnr,
        mwskz TYPE bset-mwskz,
        hwste TYPE bset-hwste,
        hwbas TYPE bset-hwbas,
        fwste TYPE bset-fwste,
        h2ste TYPE bset-h2ste,
        fwbas TYPE bset-fwbas,
      END OF ty_bset_data,
      tt_bset_data TYPE SORTED TABLE OF ty_bset_data WITH NON-UNIQUE KEY bukrs gjahr belnr.

    TYPES:
      BEGIN OF ty_bseg_data,
        bukrs TYPE bseg-bukrs,
        gjahr TYPE bseg-gjahr,
        belnr TYPE bseg-belnr,
        wrbtr TYPE bseg-wrbtr,
      END OF ty_bseg_data,
      tt_bseg_data TYPE SORTED TABLE OF ty_bseg_data WITH NON-UNIQUE KEY bukrs gjahr belnr.

    TYPES:
      BEGIN OF ty_correction_keyed,
        bukrs       TYPE bseg-bukrs,
        gjahr       TYPE bseg-gjahr,
        belnr       TYPE bseg-belnr,
        ksef_num    TYPE zlx_ksef_out-ksef_num,
        xmlgen_date TYPE zlx_ksef_out-xmlgen_date,
        xml         TYPE zlx_ksef_out-xml,
        vbeln       TYPE vbrk-vbeln,
        stceg       TYPE vbrk-stceg,
        rebzg       TYPE bseg-rebzg,
        rebzj       TYPE bseg-rebzj,
        kidno       TYPE bseg-kidno,
      END OF ty_correction_keyed,
      tt_correction_keyed TYPE SORTED TABLE OF ty_correction_keyed WITH NON-UNIQUE KEY bukrs gjahr belnr.

    TYPES tt_repo_items_sorted TYPE SORTED TABLE OF zif_ksef_xml_types=>ty_repo_item
      WITH NON-UNIQUE KEY vbeln posnr.

    TYPES tt_repo_podmiot3_sorted TYPE SORTED TABLE OF zif_ksef_xml_types=>ty_repo_podmiot3
      WITH NON-UNIQUE KEY vbeln parvw kunnr.

    TYPES:
      BEGIN OF ty_vendor_keyed,
        bukrs      TYPE bukrs,
        name1      TYPE adrc-name1,
        city1      TYPE adrc-city1,
        post_code1 TYPE adrc-post_code1,
        street     TYPE adrc-street,
        house_num1 TYPE adrc-house_num1,
        stceg      TYPE t001-stceg,
        country    TYPE adrc-country,
        land1      TYPE t001-land1,
        butxt      TYPE t001-butxt,
      END OF ty_vendor_keyed,
      tt_vendor_keyed TYPE HASHED TABLE OF ty_vendor_keyed WITH UNIQUE KEY bukrs.

    TYPES:
      BEGIN OF ty_customer_keyed,
        vbeln TYPE vbrk-vbeln,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
        ort01 TYPE kna1-ort01,
        pstlz TYPE kna1-pstlz,
        stras TYPE kna1-stras,
        stceg TYPE kna1-stceg,
        land1 TYPE kna1-land1,
        zterm TYPE knb1-zterm,
      END OF ty_customer_keyed,
      tt_customer_keyed TYPE HASHED TABLE OF ty_customer_keyed WITH UNIQUE KEY vbeln.

    METHODS add_message
      IMPORTING iv_severity TYPE symsgty
                iv_code     TYPE string
                iv_text     TYPE string
                iv_ksef_id  TYPE zlx_ksef_id
      CHANGING  ct_messages TYPE zif_ksef_xml_types=>tt_message.
ENDCLASS.

CLASS zcl_ksef_found_xml_repository IMPLEMENTATION.
  METHOD read_batch.
    CLEAR rt_invoices.

    IF it_ksef_ids IS INITIAL.
      RETURN.
    ENDIF.

    DATA lt_ksef_ids TYPE tt_ksef_id_entry.
    LOOP AT it_ksef_ids INTO DATA(ls_ksef_id).
      IF ls_ksef_id-ksef_id IS INITIAL.
        CONTINUE.
      ENDIF.
      INSERT VALUE ty_ksef_id_entry( ksef_id = ls_ksef_id-ksef_id ) INTO TABLE lt_ksef_ids.
    ENDLOOP.

    IF lt_ksef_ids IS INITIAL.
      RETURN.
    ENDIF.

    DATA lt_ksef_id_range TYPE RANGE OF zlx_ksef_id.
    LOOP AT lt_ksef_ids INTO DATA(ls_ksef_id_entry).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_ksef_id_entry-ksef_id ) TO lt_ksef_id_range.
    ENDLOOP.

    DATA lt_mapping TYPE tt_ksef_mapping.
    SELECT ksef_id,
           docnum
      FROM zlx_ksef_out
      INTO TABLE @DATA(lt_mapping_raw)
      WHERE ksef_id IN @lt_ksef_id_range.

    LOOP AT lt_mapping_raw INTO DATA(ls_mapping_raw).
      INSERT VALUE ty_ksef_mapping(
        ksef_id = ls_mapping_raw-ksef_id
        docnum  = ls_mapping_raw-docnum ) INTO TABLE lt_mapping.
    ENDLOOP.

    DATA lt_docnums TYPE tt_docnum_entry.
    LOOP AT lt_mapping INTO DATA(ls_mapping).
      IF ls_mapping-docnum IS NOT INITIAL.
        INSERT VALUE ty_docnum_entry( docnum = ls_mapping-docnum ) INTO TABLE lt_docnums.
      ENDIF.
    ENDLOOP.

    DATA lt_vbrk TYPE tt_vbrk_base.
    DATA lt_bkpf TYPE tt_bkpf_data.
    DATA lt_bset TYPE tt_bset_data.
    DATA lt_bseg TYPE tt_bseg_data.
    DATA lt_correction TYPE tt_correction_keyed.
    DATA lt_vendor TYPE tt_vendor_keyed.
    DATA lt_customer TYPE tt_customer_keyed.
    DATA lt_items TYPE tt_repo_items_sorted.
    DATA lt_podmiot3 TYPE tt_repo_podmiot3_sorted.

    IF lt_docnums IS NOT INITIAL.
      DATA lt_docnum_range TYPE RANGE OF vbrk-vbeln.
      LOOP AT lt_docnums INTO DATA(ls_docnum_entry).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_docnum_entry-docnum ) TO lt_docnum_range.
      ENDLOOP.

      SELECT vbeln,
             bukrs,
             gjahr,
             belnr,
             waerk,
             netwr,
             fkart,
             sppord,
             fkdat,
             kurrf,
             knumv,
             kunag,
             stceg
        FROM vbrk
        INTO TABLE @lt_vbrk
        WHERE vbeln IN @lt_docnum_range.

      DATA lt_key_fields TYPE SORTED TABLE OF ty_bkpf_data WITH UNIQUE KEY bukrs gjahr belnr.
      LOOP AT lt_vbrk INTO DATA(ls_vbrk).
        IF ls_vbrk-belnr IS INITIAL OR ls_vbrk-bukrs IS INITIAL OR ls_vbrk-gjahr IS INITIAL.
          CONTINUE.
        ENDIF.
        INSERT VALUE ty_bkpf_data(
          bukrs = ls_vbrk-bukrs
          gjahr = ls_vbrk-gjahr
          belnr = ls_vbrk-belnr ) INTO TABLE lt_key_fields.
      ENDLOOP.

      IF lt_key_fields IS NOT INITIAL.
        SELECT bukrs,
               gjahr,
               belnr,
               waers,
               bldat,
               blart,
               cpudt,
               cputm,
               kursf
          FROM bkpf
          INTO TABLE @lt_bkpf
          FOR ALL ENTRIES IN @lt_key_fields
          WHERE bukrs = @lt_key_fields-bukrs
            AND gjahr = @lt_key_fields-gjahr
            AND belnr = @lt_key_fields-belnr.

        SELECT bukrs,
               gjahr,
               belnr,
               mwskz,
               hwste,
               hwbas,
               fwste,
               h2ste,
               fwbas
          FROM bset
          INTO TABLE @lt_bset
          FOR ALL ENTRIES IN @lt_key_fields
          WHERE bukrs = @lt_key_fields-bukrs
            AND gjahr = @lt_key_fields-gjahr
            AND belnr = @lt_key_fields-belnr.

        SELECT bukrs,
               gjahr,
               belnr,
               wrbtr
          FROM bseg
          INTO TABLE @lt_bseg
          FOR ALL ENTRIES IN @lt_key_fields
          WHERE bukrs = @lt_key_fields-bukrs
            AND gjahr = @lt_key_fields-gjahr
            AND belnr = @lt_key_fields-belnr
            AND koart = 'D'.

        SELECT b~bukrs,
               b~gjahr,
               b~belnr,
               z~ksef_num,
               z~xmlgen_date,
               z~xml,
               v~vbeln,
               v~stceg,
               b~rebzg,
               b~rebzj,
               b~kidno
          FROM bseg AS b
          LEFT OUTER JOIN vbrk AS v
            ON v~vbeln = b~kidno
          LEFT OUTER JOIN zlx_ksef_out AS z
            ON z~bukrs = b~bukrs
           AND z~docnum = b~kidno
          FOR ALL ENTRIES IN @lt_key_fields
          WHERE b~bukrs = @lt_key_fields-bukrs
            AND b~gjahr = @lt_key_fields-gjahr
            AND b~belnr = @lt_key_fields-belnr
            INTO TABLE @lt_correction.
      ENDIF.

      DATA lt_bukrs TYPE tt_bukrs_entry.
      LOOP AT lt_vbrk INTO ls_vbrk.
        IF ls_vbrk-bukrs IS NOT INITIAL.
          INSERT VALUE ty_bukrs_entry( bukrs = ls_vbrk-bukrs ) INTO TABLE lt_bukrs.
        ENDIF.
      ENDLOOP.

      IF lt_bukrs IS NOT INITIAL.
        DATA lt_bukrs_range TYPE RANGE OF bukrs.
        LOOP AT lt_bukrs INTO DATA(ls_bukrs_entry).
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_bukrs_entry-bukrs ) TO lt_bukrs_range.
        ENDLOOP.

        SELECT t~bukrs,
               ad~name1,
               ad~city1,
               ad~post_code1,
               ad~street,
               ad~house_num1,
               t~stceg,
               ad~country,
               t~land1,
               t~butxt
          FROM t001 AS t
          LEFT OUTER JOIN adrc AS ad
            ON t~adrnr = ad~addrnumber
          INTO TABLE @DATA(lt_vendor_raw)
          WHERE t~bukrs IN @lt_bukrs_range.

        LOOP AT lt_vendor_raw INTO DATA(ls_vendor_raw).
          INSERT VALUE ty_vendor_keyed(
            bukrs      = ls_vendor_raw-bukrs
            name1      = ls_vendor_raw-name1
            city1      = ls_vendor_raw-city1
            post_code1 = ls_vendor_raw-post_code1
            street     = ls_vendor_raw-street
            house_num1 = ls_vendor_raw-house_num1
            stceg      = ls_vendor_raw-stceg
            country    = ls_vendor_raw-country
            land1      = ls_vendor_raw-land1
            butxt      = ls_vendor_raw-butxt ) INTO TABLE lt_vendor.
        ENDLOOP.
      ENDIF.

      SELECT k~vbeln,
             k~fkdat,
             k~bukrs,
             k~waerk,
             k~kurrf,
             k~fkart,
             k~knumv,
             p~posnr,
             p~arktx,
             p~matnr,
             p~vrkme,
             p~fkimg,
             p~netwr,
             p~mwskz,
             p~lmeng,
             p~mwsbp,
             p~cmpre,
             p~kzwi1,
             p~kzwi2
        FROM vbrk AS k
        LEFT OUTER JOIN vbrp AS p
          ON p~vbeln = k~vbeln
        INTO TABLE @lt_items
        WHERE k~vbeln IN @lt_docnum_range
        ORDER BY p~posnr.

      SELECT v~vbeln,
             k~kunnr,
             k~name1,
             k~ort01,
             k~pstlz,
             k~stras,
             k~stceg,
             k~land1,
             knb1~zterm
        FROM vbrk AS v
        LEFT OUTER JOIN kna1 AS k
          ON v~kunag = k~kunnr
        LEFT OUTER JOIN knb1
          ON knb1~bukrs = v~bukrs
         AND knb1~kunnr = v~kunag
        INTO TABLE @DATA(lt_customer_raw)
        WHERE v~vbeln IN @lt_docnum_range
          AND v~kunag <> ''.

      LOOP AT lt_customer_raw INTO DATA(ls_customer_raw).
        INSERT VALUE ty_customer_keyed(
          vbeln = ls_customer_raw-vbeln
          kunnr = ls_customer_raw-kunnr
          name1 = ls_customer_raw-name1
          ort01 = ls_customer_raw-ort01
          pstlz = ls_customer_raw-pstlz
          stras = ls_customer_raw-stras
          stceg = ls_customer_raw-stceg
          land1 = ls_customer_raw-land1
          zterm = ls_customer_raw-zterm ) INTO TABLE lt_customer.
      ENDLOOP.

      SELECT v~vbeln,
             k~kunnr,
             k~name1,
             k~ort01,
             k~pstlz,
             k~stras,
             k~stceg,
             k~land1,
             v~parvw,
             t~vtext
        FROM vbpa AS v
        LEFT OUTER JOIN kna1 AS k
          ON v~kunnr = k~kunnr
        LEFT OUTER JOIN tpart AS t
          ON v~parvw = t~parvw
        INTO TABLE @lt_podmiot3
        WHERE v~vbeln IN @lt_docnum_range
          AND v~parvw IN ( 'Z1', 'Z2', 'Z3', 'Z4', 'Z5', 'Z6', 'Z7', 'Z8', 'Z9', 'Y1', 'Y2' ).
    ENDIF.

    LOOP AT it_ksef_ids INTO ls_ksef_id.
      DATA(ls_repo_invoice) = VALUE zif_ksef_xml_types=>ty_repo_invoice(
        ksef_id = ls_ksef_id-ksef_id ).

      READ TABLE lt_mapping WITH TABLE KEY ksef_id = ls_ksef_id-ksef_id INTO ls_mapping.
      IF sy-subrc <> 0.
        me->add_message(
          EXPORTING
            iv_severity = 'E'
            iv_code     = 'KSEF_ID_NOT_FOUND'
            iv_text     = 'No mapping found in ZLX_KSEF_OUT for KSeF ID.'
            iv_ksef_id  = ls_ksef_id-ksef_id
          CHANGING
            ct_messages = ls_repo_invoice-messages ).
        APPEND ls_repo_invoice TO rt_invoices.
        CONTINUE.
      ENDIF.

      ls_repo_invoice-docnum = ls_mapping-docnum.

      READ TABLE lt_vbrk INTO ls_vbrk WITH TABLE KEY vbeln = ls_mapping-docnum.
      IF sy-subrc <> 0.
        me->add_message(
          EXPORTING
            iv_severity = 'E'
            iv_code     = 'DOCNUM_NOT_FOUND'
            iv_text     = 'No billing document found for mapped docnum.'
            iv_ksef_id  = ls_ksef_id-ksef_id
          CHANGING
            ct_messages = ls_repo_invoice-messages ).
        APPEND ls_repo_invoice TO rt_invoices.
        CONTINUE.
      ENDIF.

      ls_repo_invoice-bukrs = ls_vbrk-bukrs.
      ls_repo_invoice-gjahr = ls_vbrk-gjahr.
      ls_repo_invoice-belnr = ls_vbrk-belnr.

      ls_repo_invoice-main_info = VALUE zif_ksef_xml_types=>ty_repo_main_info(
        v_waerk  = ls_vbrk-waerk
        v_vbeln  = ls_vbrk-vbeln
        v_netwr  = ls_vbrk-netwr
        v_fkart  = ls_vbrk-fkart
        v_sppord = ls_vbrk-sppord ).

      READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH TABLE KEY bukrs = ls_vbrk-bukrs gjahr = ls_vbrk-gjahr belnr = ls_vbrk-belnr.
      IF sy-subrc = 0.
        ls_repo_invoice-main_info-b_waers = ls_bkpf-waers.
        ls_repo_invoice-main_info-b_bldat = ls_bkpf-bldat.
        ls_repo_invoice-main_info-b_blart = ls_bkpf-blart.
        ls_repo_invoice-main_info-b_cpudt = ls_bkpf-cpudt.
        ls_repo_invoice-main_info-b_cputm = ls_bkpf-cputm.
        ls_repo_invoice-main_info-b_kursf = ls_bkpf-kursf.
      ENDIF.

      READ TABLE lt_bset INTO DATA(ls_bset) WITH KEY bukrs = ls_vbrk-bukrs gjahr = ls_vbrk-gjahr belnr = ls_vbrk-belnr.
      IF sy-subrc = 0.
        ls_repo_invoice-main_info-t_mwskz = ls_bset-mwskz.
        ls_repo_invoice-main_info-t_hwste = ls_bset-hwste.
        ls_repo_invoice-main_info-t_hwbas = ls_bset-hwbas.
        ls_repo_invoice-main_info-t_fwste = ls_bset-fwste.
        ls_repo_invoice-main_info-t_h2ste = ls_bset-h2ste.
        ls_repo_invoice-main_info-t_fwbas = ls_bset-fwbas.
      ENDIF.

      READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY bukrs = ls_vbrk-bukrs gjahr = ls_vbrk-gjahr belnr = ls_vbrk-belnr.
      IF sy-subrc = 0.
        ls_repo_invoice-main_info-g_wrbtr = ls_bseg-wrbtr.
      ENDIF.

      READ TABLE lt_vendor INTO DATA(ls_vendor) WITH TABLE KEY bukrs = ls_vbrk-bukrs.
      IF sy-subrc = 0.
        ls_repo_invoice-vendor_address = VALUE zif_ksef_xml_types=>ty_repo_vendor_address(
          name1      = ls_vendor-name1
          city1      = ls_vendor-city1
          post_code1 = ls_vendor-post_code1
          street     = ls_vendor-street
          house_num1 = ls_vendor-house_num1
          stceg      = ls_vendor-stceg
          country    = ls_vendor-country
          land1      = ls_vendor-land1
          butxt      = ls_vendor-butxt ).
      ENDIF.

      READ TABLE lt_customer INTO DATA(ls_customer) WITH TABLE KEY vbeln = ls_vbrk-vbeln.
      IF sy-subrc = 0.
        ls_repo_invoice-customer_address = VALUE zif_ksef_xml_types=>ty_repo_customer_address(
          kunnr = ls_customer-kunnr
          name1 = ls_customer-name1
          ort01 = ls_customer-ort01
          pstlz = ls_customer-pstlz
          stras = ls_customer-stras
          stceg = ls_customer-stceg
          land1 = ls_customer-land1
          zterm = ls_customer-zterm ).
      ENDIF.

      READ TABLE lt_correction INTO DATA(ls_correction) WITH KEY bukrs = ls_vbrk-bukrs gjahr = ls_vbrk-gjahr belnr = ls_vbrk-belnr.
      IF sy-subrc = 0.
        ls_repo_invoice-correction = VALUE zif_ksef_xml_types=>ty_repo_correction(
          ksef_num    = ls_correction-ksef_num
          xmlgen_date = ls_correction-xmlgen_date
          xml         = ls_correction-xml
          vbeln       = ls_correction-vbeln
          stceg       = ls_correction-stceg
          bukrs       = ls_correction-bukrs
          gjahr       = ls_correction-gjahr
          rebzg       = ls_correction-rebzg
          rebzj       = ls_correction-rebzj
          kidno       = ls_correction-kidno ).
        ls_repo_invoice-xml_old = ls_correction-xml.
      ENDIF.

      DATA lt_items_for_doc TYPE zif_ksef_xml_types=>tt_repo_items.
      READ TABLE lt_items TRANSPORTING NO FIELDS WITH KEY vbeln = ls_mapping-docnum.
      IF sy-subrc = 0.
        DATA(lv_items_index) = sy-tabix.
        LOOP AT lt_items INTO DATA(ls_item) FROM lv_items_index.
          IF ls_item-vbeln <> ls_mapping-docnum.
            EXIT.
          ENDIF.
          APPEND ls_item TO lt_items_for_doc.
        ENDLOOP.
      ENDIF.
      ls_repo_invoice-items = lt_items_for_doc.

      DATA lt_podmiot3_for_doc TYPE zif_ksef_xml_types=>tt_repo_podmiot3.
      READ TABLE lt_podmiot3 TRANSPORTING NO FIELDS WITH KEY vbeln = ls_mapping-docnum.
      IF sy-subrc = 0.
        DATA(lv_podmiot_index) = sy-tabix.
        LOOP AT lt_podmiot3 INTO DATA(ls_podmiot3) FROM lv_podmiot_index.
          IF ls_podmiot3-vbeln <> ls_mapping-docnum.
            EXIT.
          ENDIF.
          APPEND ls_podmiot3 TO lt_podmiot3_for_doc.
        ENDLOOP.
      ENDIF.
      ls_repo_invoice-podmiot3 = lt_podmiot3_for_doc.

      APPEND ls_repo_invoice TO rt_invoices.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_message.
    APPEND VALUE zif_ksef_xml_types=>ty_message(
      severity = iv_severity
      code     = iv_code
      text     = iv_text
      ksef_id  = iv_ksef_id ) TO ct_messages.
  ENDMETHOD.
ENDCLASS.
