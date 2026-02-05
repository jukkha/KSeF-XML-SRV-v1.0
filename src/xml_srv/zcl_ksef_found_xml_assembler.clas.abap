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

    rs_invoice-header-nagl_kodformularza = 'FA'.
    rs_invoice-header-nagl_wariantformularza = '3'.

    IF is_repo_invoice-main_info-b_cpudt IS NOT INITIAL
       AND is_repo_invoice-main_info-b_cputm IS NOT INITIAL.
      rs_invoice-header-nagl_datawytworzeniafa =
        |{ is_repo_invoice-main_info-b_cpudt DATE = ISO }T{ is_repo_invoice-main_info-b_cputm TIME = ISO }Z|.
    ENDIF.

    rs_invoice-header-fa_kodwaluty = is_repo_invoice-main_info-v_waerk.
    rs_invoice-header-fa_p_1 = |{ is_repo_invoice-main_info-b_bldat DATE = ISO }|.
    rs_invoice-header-fa_p_2 = is_repo_invoice-main_info-v_vbeln.
    rs_invoice-header-fa_p_15 = is_repo_invoice-main_info-g_wrbtr.
    rs_invoice-header-fa_kurswalutyz = is_repo_invoice-main_info-b_kursf.

    CASE is_repo_invoice-main_info-v_fkart.
      WHEN 'F2'.
        rs_invoice-header-fa_rodzajfaktury = 'VAT'.
      WHEN 'G2' OR 'S1'.
        rs_invoice-header-fa_rodzajfaktury = 'KOR'.
      WHEN 'FAZ'.
        rs_invoice-header-fa_rodzajfaktury = 'ZAL'.
      WHEN OTHERS.
        CLEAR rs_invoice-header-fa_rodzajfaktury.
    ENDCASE.

    CASE is_repo_invoice-main_info-t_mwskz.
      WHEN 'T1' OR 'T4'.
        rs_invoice-header-fa_p_13_1 = is_repo_invoice-main_info-t_fwbas.
        rs_invoice-header-fa_p_14_1 = is_repo_invoice-main_info-t_hwste.
        IF is_repo_invoice-main_info-v_waerk <> 'PLN'.
          rs_invoice-header-fa_p_14_1w = is_repo_invoice-main_info-t_fwste.
        ENDIF.
      WHEN 'T2'.
        rs_invoice-header-fa_p_13_2 = is_repo_invoice-main_info-t_fwbas.
        rs_invoice-header-fa_p_14_2 = is_repo_invoice-main_info-t_hwste.
        IF is_repo_invoice-main_info-v_waerk = 'PLN'.
          rs_invoice-header-fa_p_14_2w = is_repo_invoice-main_info-t_fwste.
        ENDIF.
      WHEN 'T3'.
        rs_invoice-header-fa_p_13_3 = is_repo_invoice-main_info-t_fwbas.
        rs_invoice-header-fa_p_14_3 = is_repo_invoice-main_info-t_hwste.
        IF is_repo_invoice-main_info-v_waerk = 'PLN'.
          rs_invoice-header-fa_p_14_3w = is_repo_invoice-main_info-t_fwste.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    rs_invoice-header-fa_nrzamowienia = is_repo_invoice-main_info-v_sppord.

    IF is_repo_invoice-correction-ksef_num IS NOT INITIAL.
      rs_invoice-header-fa_nrkseffakorygowanej = is_repo_invoice-correction-ksef_num.
      rs_invoice-header-fa_nrksef = '1'.
    ENDIF.
    rs_invoice-header-fa_nrfakorygowanej = is_repo_invoice-correction-vbeln.
    IF is_repo_invoice-correction-xmlgen_date IS NOT INITIAL.
      rs_invoice-header-fa_datawystfakorygowanej = |{ is_repo_invoice-correction-xmlgen_date DATE = ISO }|.
    ENDIF.

    rs_invoice-podmiot1-prefikspodatnika = is_repo_invoice-vendor_address-stceg(2).
    rs_invoice-podmiot1-nip = is_repo_invoice-vendor_address-stceg+2.
    rs_invoice-podmiot1-nazwa = is_repo_invoice-vendor_address-butxt.
    rs_invoice-podmiot1-kodkraju = is_repo_invoice-vendor_address-land1.
    rs_invoice-podmiot1-adresl1 = |{ is_repo_invoice-vendor_address-street }{ is_repo_invoice-vendor_address-house_num1 }|.
    rs_invoice-podmiot1-adresl2 = |{ is_repo_invoice-vendor_address-post_code1 }{ is_repo_invoice-vendor_address-city1 }|.

    rs_invoice-podmiot2-nip = is_repo_invoice-customer_address-stceg+2.
    IF rs_invoice-podmiot2-nip IS INITIAL.
      rs_invoice-podmiot2-kodue = is_repo_invoice-customer_address-stceg(2).
      rs_invoice-podmiot2-nrvatue = is_repo_invoice-customer_address-stceg+2.
      IF rs_invoice-podmiot2-kodue IS INITIAL AND rs_invoice-podmiot2-nrvatue IS INITIAL.
        rs_invoice-podmiot2-kodkraju = is_repo_invoice-customer_address-land1.
      ENDIF.
      rs_invoice-podmiot2-brakid = '1'.
    ENDIF.
    rs_invoice-podmiot2-nazwa = is_repo_invoice-customer_address-name1.
    rs_invoice-podmiot2-adresl1 = is_repo_invoice-customer_address-stras.
    rs_invoice-podmiot2-adresl2 = |{ is_repo_invoice-customer_address-pstlz }{ is_repo_invoice-customer_address-ort01 }|.
    rs_invoice-podmiot2-kodkraju = is_repo_invoice-customer_address-land1.

    LOOP AT is_repo_invoice-podmiot3 INTO DATA(ls_repo_podmiot3).
      APPEND VALUE zif_ksef_xml_types=>ty_podmiot(
        nip      = ls_repo_podmiot3-stceg+2
        kodue    = COND #( WHEN ls_repo_podmiot3-stceg+2 IS INITIAL THEN ls_repo_podmiot3-stceg(2) )
        nrvatue  = COND #( WHEN ls_repo_podmiot3-stceg+2 IS INITIAL THEN ls_repo_podmiot3-stceg+2 )
        brakid   = COND #( WHEN ls_repo_podmiot3-stceg+2 IS INITIAL THEN '1' ELSE '' )
        nazwa    = ls_repo_podmiot3-name1
        kodkraju = ls_repo_podmiot3-land1
        adresl1  = ls_repo_podmiot3-stras
        adresl2  = |{ ls_repo_podmiot3-pstlz }{ ls_repo_podmiot3-ort01 }|
        rola     = zif_ksef_constants=>br-(ls_repo_podmiot3-parvw)
        rolainna = COND #( WHEN zif_ksef_constants=>br-(ls_repo_podmiot3-parvw) IS INITIAL THEN '1' ELSE '' )
        opisroli = COND #( WHEN zif_ksef_constants=>br-(ls_repo_podmiot3-parvw) IS INITIAL THEN ls_repo_podmiot3-vtext ELSE '' ) )
      TO rs_invoice-podmiot3.
    ENDLOOP.

    SORT rs_invoice-podmiot3 BY rola nip idnabywcy nazwa.

    LOOP AT is_repo_invoice-items INTO DATA(ls_repo_item).
      APPEND VALUE zif_ksef_xml_types=>ty_invoice_item(
        nrwierszafa = ls_repo_item-posnr
        uu_id       = ls_repo_item-posnr
        p_7         = ls_repo_item-arktx
        indeks      = ls_repo_item-matnr
        p_8a        = ls_repo_item-vrkme
        p_8b        = ls_repo_item-lmeng
        p_9a        = COND #( WHEN ls_repo_item-lmeng IS INITIAL THEN '' ELSE ls_repo_item-netwr / ls_repo_item-lmeng )
        p_9b        = ls_repo_item-cmpre
        p_10        = ls_repo_item-kzwi1 - ls_repo_item-kzwi2
        p_11        = ls_repo_item-netwr
        p_11a       = ls_repo_item-netwr + ls_repo_item-mwsbp
        p_12        = COND #( WHEN ls_repo_item-netwr IS INITIAL THEN '' ELSE ( ls_repo_item-mwsbp * 100 ) / ls_repo_item-netwr )
        kurswaluty  = ls_repo_item-kurrf ) TO rs_invoice-items.
    ENDLOOP.

    SORT rs_invoice-items BY nrwierszafa uu_id.
  ENDMETHOD.
ENDCLASS.
