CLASS zcl_ksef_data_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_items     TYPE STANDARD TABLE OF zksef_s_item WITH DEFAULT KEY,
      tt_podmiot3  TYPE STANDARD TABLE OF zlx_ksef_podmiot3 WITH DEFAULT KEY,
      tt_podmiot2k TYPE STANDARD TABLE OF zlx_ksef_podmiot WITH DEFAULT KEY,
      tt_zal_items TYPE STANDARD TABLE OF zlx_ksef_zal_items WITH DEFAULT KEY.

    METHODS fill_table
      IMPORTING
        !iv_docnum    TYPE char10
        !iv_cocode    TYPE bukrs
        !iv_gjahr     TYPE gjahr
        !iv_belnr     TYPE belnr_d
      EXPORTING
        !es_header    TYPE zksef_s_head
        !et_item      TYPE tt_items
        !et_podmiot3  TYPE tt_podmiot3
        !et_podmiot2k TYPE tt_podmiot2k
        !et_zal_items TYPE tt_zal_items
        !et_error     TYPE zkstg_t_msg.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS fill_kor_pod1
      IMPORTING iv_xml_old    TYPE zlx_xml
                is_new_pod1   TYPE zlx_ksef_podmiot
      EXPORTING es_pod1k      TYPE zlx_ksef_podmiot
                ev_fill_pod1k TYPE abap_bool.

    METHODS fill_pod2k
      IMPORTING
        iv_xml_old      TYPE zlx_xml
        is_new_pod2     TYPE zlx_ksef_podmiot
        it_new_podmiot3 TYPE tt_podmiot3
      EXPORTING
        et_podmiot2k    TYPE tt_podmiot2k.

    METHODS fill_items_kor
      IMPORTING
        iv_xml_old   TYPE zlx_xml
        it_items_new TYPE tt_items
      EXPORTING
        et_items_out TYPE tt_items
        et_error     TYPE zkstg_t_msg.

    METHODS   fill_zal_items_kor
      IMPORTING
        iv_xml_old    TYPE zlx_xml
        it_zitems_new TYPE tt_zal_items
      EXPORTING
        et_zitems_out TYPE tt_zal_items
        et_error      TYPE zkstg_t_msg.

    METHODS parse_podmiot_from_xml
      IMPORTING iv_xml            TYPE zlx_xml
                iv_tagname        TYPE string
      RETURNING VALUE(rs_podmiot) TYPE zlx_ksef_podmiot.

    METHODS parse_tag_from_xml
      IMPORTING iv_xml      TYPE zlx_xml
                iv_tagname1 TYPE string
                iv_tagname2 TYPE string
      EXPORTING ev_tagname1 TYPE any
                ev_tagname2 TYPE any.

    METHODS parse_podmiot3_from_xml
      IMPORTING
        iv_xml             TYPE zlx_xml
      RETURNING
        VALUE(rt_pod3_old) TYPE tt_podmiot2k.

    METHODS parse_items_from_xml
      IMPORTING
        iv_xml_old          TYPE zlx_xml
      RETURNING
        VALUE(rt_items_old) TYPE tt_items.

    METHODS parse_zal_items_from_xml
      IMPORTING
        iv_xml_old           TYPE zlx_xml
      RETURNING
        VALUE(rt_zitems_old) TYPE tt_zal_items.

    METHODS read_text_value
      IMPORTING io_reader     TYPE REF TO if_sxml_reader
      RETURNING VALUE(rv_val) TYPE string.

    METHODS podmiot_differs
      IMPORTING is_a           TYPE zlx_ksef_podmiot
                is_b           TYPE zlx_ksef_podmiot
      RETURNING VALUE(rv_diff) TYPE abap_bool.

    METHODS items_differs
      IMPORTING
        iv_xml_old     TYPE zlx_xml
        it_items_new   TYPE tt_items
      RETURNING
        VALUE(rv_diff) TYPE abap_bool.

    METHODS zal_items_differs
      IMPORTING
        iv_xml_old     TYPE zlx_xml
        it_zitems_new  TYPE tt_zal_items
      RETURNING
        VALUE(rv_diff) TYPE abap_bool.

ENDCLASS.


CLASS zcl_ksef_data_helper IMPLEMENTATION.


  METHOD fill_table.

*------VBRK+BKPF+BSET-main info
    SELECT SINGLE
     v~waerk, v~vbeln, v~netwr, v~fkart, v~sppord,
     t~mwskz, t~hwste, t~hwbas, t~fwste, t~h2ste, t~fwbas,
     b~waers, b~bldat, b~blart, b~cpudt, b~cputm, b~kursf,
     g~wrbtr
    FROM vbrk AS v
    LEFT OUTER JOIN bkpf AS b
     ON  v~belnr = b~belnr
     AND v~gjahr = b~gjahr
     AND v~bukrs = b~bukrs
    LEFT OUTER JOIN bset AS t
     ON  b~belnr = t~belnr
     AND b~gjahr = t~gjahr
     AND b~bukrs = t~bukrs
    LEFT OUTER JOIN bseg AS g
     ON  b~belnr = g~belnr
     AND b~gjahr = g~gjahr
     AND b~bukrs = g~bukrs
     AND g~koart = 'D'
   INTO @DATA(ls_main_info)
    WHERE v~vbeln = @iv_docnum
    AND   v~bukrs = @iv_cocode
    AND   v~gjahr = @iv_gjahr
    AND   v~belnr = @iv_belnr.

*------Correction
    SELECT SINGLE
           z~ksef_num, z~xmlgen_date, z~xml,
           v~vbeln, v~stceg,
           b~bukrs, b~gjahr,
           b~rebzg, b~rebzj, b~kidno
      FROM bseg AS b
      LEFT OUTER JOIN vbrk AS v
        ON v~vbeln = b~kidno
      LEFT OUTER JOIN zlx_ksef_out AS z
        ON  z~bukrs      = b~bukrs
*        AND z~gjahr      = b~rebzj
        AND z~docnum     = b~kidno
      WHERE b~bukrs = @iv_cocode
        AND b~gjahr = @iv_gjahr
        AND b~belnr = @iv_belnr
      INTO @DATA(ls_correction).

    """""""""""""""""TEST KOR
*    SELECT SINGLE
*               z~ksef_num, z~xmlgen_date, z~xml,
*               z~docnum AS vbeln
*          FROM zlx_ksef_out AS z
*         WHERE z~bukrs = @iv_cocode
*           AND z~gjahr = @iv_gjahr
*           AND z~docnum = '0090000112'
*           INTO @DATA(ls_correction).

*--------Seller adres
    SELECT SINGLE ad~name1, ad~city1, ad~post_code1, ad~street, ad~house_num1,
      t~stceg, ad~country, t~land1, t~butxt
      FROM t001 AS t
      LEFT OUTER JOIN adrc AS ad ON t~adrnr = ad~addrnumber
      WHERE t~bukrs = @iv_cocode
     INTO @DATA(ls_vendor_adr).

*---------Items
    SELECT
    k~vbeln, k~fkdat, k~bukrs, k~waerk, k~kurrf, k~fkart, k~knumv,
    p~posnr, p~arktx, p~matnr, p~vrkme, p~fkimg, p~netwr, p~mwskz,
    p~lmeng, p~mwsbp, p~cmpre, p~kzwi1, p~kzwi2
  FROM vbrk AS k
  LEFT OUTER JOIN vbrp AS p
    ON p~vbeln = k~vbeln
    INTO TABLE @DATA(lt_item)
    WHERE k~vbeln = @iv_docnum
     ORDER BY p~posnr.

*--------Buyer adres
    SELECT SINGLE k~kunnr, k~name1, k~ort01,
    k~pstlz, k~stras, k~stceg, k~land1,
    knb1~zterm
    FROM vbrk AS v
    LEFT OUTER JOIN kna1 AS k ON v~kunag = k~kunnr
    LEFT OUTER JOIN knb1 ON knb1~bukrs = v~bukrs AND knb1~kunnr = v~kunag
    INTO @DATA(ls_customer_adr)
    WHERE v~bukrs = @iv_cocode
    AND   v~vbeln = @iv_docnum
    AND   v~gjahr = @iv_gjahr
    AND   v~kunag <> ''.

*----------Podmiot3
    SELECT k~kunnr, k~name1, k~ort01,
    k~pstlz, k~stras, k~stceg, k~land1,
    v~parvw, t~vtext
    FROM vbpa AS v
    LEFT OUTER JOIN kna1 AS k ON v~kunnr = k~kunnr
    LEFT OUTER JOIN tpart AS t ON v~parvw = t~parvw
    INTO TABLE @DATA(lt_podmiot3)
    WHERE v~vbeln = @iv_docnum
    AND v~parvw IN ( 'Z1', 'Z2', 'Z3', 'Z4', 'Z5', 'Z6', 'Z7', 'Z8', 'Z9', 'Y1', 'Y2' ).

*************************************
    DATA lv_zfbdt_out TYPE dzfbdt.
    DATA lv_zbd3t     TYPE dzbd3t.
    DATA lv_current_tstmp  TYPE timestampl.
    DATA lv_ctime TYPE string.
    DATA lv_date TYPE d.
    lv_date = sy-datum.
    DATA lv_datetime TYPE string.
    lv_datetime = ls_main_info-cpudt && ls_main_info-cputm.


    GET TIME STAMP FIELD lv_current_tstmp.
    lv_ctime = lv_current_tstmp.

    DATA: lv_zterm TYPE char4,
          lv_due   TYPE dats.

    lv_zterm = ls_customer_adr-zterm.


    CALL FUNCTION 'FI_TERMS_OF_PAYMENT_PROPOSE'
      EXPORTING
        i_bldat         = lv_date
        i_budat         = lv_date
        i_zfbdt         = lv_date
        i_zterm         = lv_zterm
      IMPORTING
        e_zfbdt         = lv_zfbdt_out
        e_zbd3t         = lv_zbd3t
      EXCEPTIONS
        terms_not_found = 1
        OTHERS          = 2.

    IF sy-subrc = 0 AND lv_zfbdt_out IS NOT INITIAL.
      lv_due = lv_zfbdt_out.
      lv_due = lv_due + lv_zbd3t.
    ENDIF.

    FIELD-SYMBOLS <ls_error> TYPE zkstg_msg.

*->Faktura
*-->Naglowek
    es_header-nagl_kodformularza     = 'FA'.
    es_header-nagl_wariantformularza = '3'.
    es_header-nagl_datawytworzeniafa = |{ lv_datetime+0(4) }-{ lv_datetime+4(2) }-{ lv_datetime+6(2) }T{ lv_datetime+8(2) }:{ lv_datetime+10(2) }:{ lv_datetime+12(2) }Z|.
    es_header-nagl_systeminfo        = ''.

*-->Podmiot1
    es_header-p1_prefikspodatnika    = ls_vendor_adr-stceg(2).
    es_header-p1_nreori              = ''.

*--->DaneIdentyfikacyjne
*    es_header-p1_nip                 = |{ ls_vendor_adr-stceg+2(10) }|.
    es_header-p1_nip                 = '6751809716'.
    es_header-p1_nazwa               = ls_vendor_adr-butxt.

*--->Adres
    es_header-p1_kodkraju            = ls_vendor_adr-land1.
    es_header-p1_adresl1             = ls_vendor_adr-street && ls_vendor_adr-house_num1.
    es_header-p1_adresl2             = ls_vendor_adr-post_code1 && ls_vendor_adr-city1.
    es_header-p1_gln                 = ''.

*--->AdresKoresp
    es_header-p1_adrkor_kodkraju     = ''.
    IF es_header-p1_adrkor_kodkraju IS NOT INITIAL.
      es_header-p1_adrkor_adresl1    = ''.
      es_header-p1_adrkor_adresl2      = ''.
      es_header-p1_adrkor_gln          = ''.
    ENDIF.

*--->DaneKontaktowe
    es_header-p1_email               = ''.
    es_header-p1_telefon             = ''.
    es_header-p1_statusinfopodatnika = ''.

*-->Podmiot2
    es_header-p2_nreori          = ''.

*--->DaneIdentyfikacyjne
*    es_header-p2_nip             = |{ ls_customer_adr-stceg+2(10) }|.
    es_header-p2_nip             = '3456712345'.
    IF es_header-p2_nip IS INITIAL.
      es_header-p2_kodue           = ls_customer_adr-stceg(2).
      es_header-p2_nrvatue         =  |{ ls_customer_adr-stceg+2(10) }|.
      IF es_header-p2_kodue IS INITIAL OR es_header-p2_nrvatue IS INITIAL.
        es_header-p2_kodkraju        = ''.
        es_header-p2_nrid            = ''.
      ENDIF.
      es_header-p2_brakid = 1.
    ENDIF.
    es_header-p2_nazwa           = ls_customer_adr-name1.

*--->Adres
    es_header-p2_adr_kodkraju    = ls_customer_adr-land1.
    es_header-p2_adr_adresl1     = ls_customer_adr-stras.
    es_header-p2_adr_adresl2     = ls_customer_adr-pstlz && ls_customer_adr-ort01.
    es_header-p2_adr_gln         = ''.

*--->AdresKoresp
    es_header-p2_adrkor_kodkraju = ''.
    IF es_header-p2_adrkor_kodkraju IS NOT INITIAL.
      es_header-p2_adrkor_adresl1  = ''.
      es_header-p2_adrkor_adresl2  = ''.
      es_header-p2_adrkor_gln      = ''.
    ENDIF.

*--->DaneKontaktowe
    es_header-p2_email           = ''.
    es_header-p2_telefon         = ''.
    es_header-p2_nrklienta       = ''.
    es_header-p2_idnabywcy       = ''.
    es_header-p2_jst             = '2'.
    IF es_header-p2_jst IS INITIAL.
      APPEND INITIAL LINE TO et_error ASSIGNING <ls_error>.
      <ls_error>-code = iv_docnum.
      <ls_error>-text = 'Podmiot2_JST'.
      <ls_error>-type = 'E'.
    ENDIF.
    es_header-p2_gv              = '2'.
    IF es_header-p2_gv IS INITIAL.
      APPEND INITIAL LINE TO et_error ASSIGNING <ls_error>.
      <ls_error>-code = iv_docnum.
      <ls_error>-text = 'Podmiot2_GV'.
      <ls_error>-type = 'E'.
    ENDIF.

**********************************************
*-->Podmiot3
    LOOP AT lt_podmiot3 ASSIGNING FIELD-SYMBOL(<ls_podmiot3_data>).
      APPEND INITIAL LINE TO et_podmiot3 ASSIGNING FIELD-SYMBOL(<ls_podmiot3>).

      <ls_podmiot3>-idnabywcy       = ''.
      <ls_podmiot3>-nreori          = ''.

*--->DaneIdentyfikacyjne
      <ls_podmiot3>-nip             = |{ <ls_podmiot3_data>-stceg+2(10) }|.
      IF <ls_podmiot3>-nip IS INITIAL.
        <ls_podmiot3>-idwew           = ''.
        IF <ls_podmiot3>-idwew IS INITIAL.
          <ls_podmiot3>-kodue           = <ls_podmiot3_data>-stceg(2).
          <ls_podmiot3>-nrvatue         = |{ <ls_podmiot3_data>-stceg+2(10) }|.
          IF <ls_podmiot3>-kodue IS INITIAL AND <ls_podmiot3>-nrvatue IS INITIAL.
            <ls_podmiot3>-kodkraju        = ''.
            <ls_podmiot3>-nrid            = ''.
          ENDIF.
        ENDIF.
        <ls_podmiot3>-brakid = 1.
      ENDIF.
      <ls_podmiot3>-nazwa           = <ls_podmiot3_data>-name1.

*--->Adres
      <ls_podmiot3>-adr_kodkraju    = <ls_podmiot3_data>-land1.
      <ls_podmiot3>-adr_adresl1     = <ls_podmiot3_data>-stras.
      <ls_podmiot3>-adr_adresl2     = <ls_podmiot3_data>-pstlz && <ls_podmiot3_data>-ort01.
      <ls_podmiot3>-adr_gln         = ''.

*--->AdresKoresp
      <ls_podmiot3>-adrkor_kodkraju = ''.
      <ls_podmiot3>-adrkor_adresl1  = ''.
      <ls_podmiot3>-adrkor_adresl2  = ''.
      <ls_podmiot3>-adrkor_gln      = ''.

*--->DaneKontaktaktowe
      <ls_podmiot3>-email           = ''.
      <ls_podmiot3>-telefon         = ''.
      <ls_podmiot3>-rola = zif_ksef_constants=>br-(<ls_podmiot3_data>-parvw).
      IF <ls_podmiot3>-rola IS INITIAL.
        <ls_podmiot3>-rolainna        = 1.
        <ls_podmiot3>-opisroli        = <ls_podmiot3_data>-vtext.
      ENDIF.

      <ls_podmiot3>-udzial          = ''.
      <ls_podmiot3>-nrklienta       = ''.
    ENDLOOP.
************************************************

*-->PodmiotUpowazniony
    es_header-pu_nreori          = ''.

*--->DaneIdentyfikacyjne
    es_header-pu_nip             = ''.
    es_header-pu_nazwa           = ''.

*--->Adres
    es_header-pu_kodkraju        = ''.
    es_header-pu_adresl1         = ''.
    es_header-pu_adresl2         = ''.
    es_header-pu_gln             = ''.

*--->AdresKoresp
    es_header-pu_adrkor_kodkraju = ''.
    es_header-pu_adrkor_adresl1  = ''.
    es_header-pu_adrkor_adresl2  = ''.
    es_header-pu_adrkor_gln      = ''.

*--->DaneKontaktowe
    es_header-pu_emailpu         = ''.
    es_header-pu_telefonpu       = ''.
    es_header-pu_rolapu          = ''.

*-->Fa
    es_header-fa_kodwaluty       = ls_main_info-waers.
    es_header-fa_p_1 = |{ lv_date DATE = ISO }|.
    es_header-fa_p_1m            = ls_vendor_adr-land1.
*    IF ls_main_info-fkart = 'F2'.
    es_header-fa_p_2             = ls_main_info-vbeln + 4000.
*    ENDIF.
    es_header-fa_wz              = ''.
    es_header-fa_p_6             = ''. "|{ lv_date DATE = ISO }|.

*--->OkresFa
    es_header-fa_p_6_od          = ''.
    IF es_header-fa_p_6_od IS NOT INITIAL.
      es_header-fa_p_6_do          = ''.
    ENDIF.
    IF ls_main_info-mwskz = 'T1' OR ls_main_info-mwskz = 'T4'.
      es_header-fa_p_13_1 = ls_main_info-fwbas.
      es_header-fa_p_14_1 = ls_main_info-hwste.
      IF ls_main_info-waers <> 'PLN'.
        es_header-fa_p_14_1w = ls_main_info-fwste.
      ENDIF.
    ENDIF.
    IF ls_main_info-mwskz = 'T2'.
      es_header-fa_p_13_2 = ls_main_info-fwbas.
      es_header-fa_p_14_2 = ls_main_info-hwste.
      IF ls_main_info-waers = 'PLN'.
        es_header-fa_p_14_2w = ls_main_info-fwste.
      ENDIF.
    ENDIF.
    IF ls_main_info-mwskz = 'T3'.
      es_header-fa_p_13_3 = ls_main_info-fwbas.
      es_header-fa_p_14_3 = ls_main_info-hwste.
      IF ls_main_info-waers = 'PLN'.
        es_header-fa_p_14_3w = ls_main_info-fwste.
      ENDIF.
    ENDIF.
    es_header-fa_p_13_4          = ''.
    es_header-fa_p_14_4          = ''.
    es_header-fa_p_14_4w         = ''.
    es_header-fa_p_13_5          = ''.
    es_header-fa_p_14_5          = ''.
    es_header-fa_p_13_6_1        = ''.
    es_header-fa_p_13_6_2        = ''.
    es_header-fa_p_13_6_3        = ''.
    es_header-fa_p_13_7          = ''.
    es_header-fa_p_13_8          = ''.
    es_header-fa_p_13_9          = ''.
    es_header-fa_p_13_10         = ''.
    es_header-fa_p_13_11         = ''.
    es_header-fa_p_15            = ls_main_info-wrbtr.
    es_header-fa_kurswalutyz     = ls_main_info-kursf.

*--->Adnotacje
    es_header-fa_p_16            = '2'.
    es_header-fa_p_17            = '2'.
    es_header-fa_p_18            = '2'.
    es_header-fa_p_18a           = '2'.

*--->Zwolnienie
    IF  ls_main_info-mwskz = 'T0' OR ls_main_info-mwskz = 'T2'
     OR ls_main_info-mwskz = 'T3' OR ls_main_info-mwskz = 'T4'.
      es_header-fa_p_19n           = '1'.
      IF es_header-fa_p_19n IS INITIAL.
        es_header-fa_p_19            = ''.
        IF es_header-fa_p_19 IS NOT INITIAL.
          es_header-fa_p_19a           = ''.
          IF es_header-fa_p_19a IS INITIAL.
            es_header-fa_p_19b           = ''.
            IF es_header-fa_p_19b IS INITIAL.
              es_header-fa_p_19c           = ''.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*--->NoweSrodkiTransportu
    IF  ls_main_info-mwskz = 'T0' OR ls_main_info-mwskz = 'T2'
     OR ls_main_info-mwskz = 'T3' OR ls_main_info-mwskz = 'T4'.
      es_header-fa_p_22n   = '1'.
    ENDIF.
    IF es_header-fa_p_22n <> 1.
      es_header-fa_p_22            = ''.
      es_header-fa_p_42_5          = ''.
*---->NowySrodekTransportu
      es_header-fa_p_22a           = ''.
      es_header-fa_p_nrwierszanst  = ''.
      es_header-fa_p_22bmk         = ''.
      es_header-fa_p_22bmd         = ''.
      es_header-fa_p_22bk          = ''.
      es_header-fa_p_22bnr         = ''.
      es_header-fa_p_22brp         = ''.
      es_header-fa_p_22b           = ''.
      IF es_header-fa_p_22b IS NOT INITIAL.
        es_header-fa_p_22b1          = ''.
        IF es_header-fa_p_22b1 IS INITIAL.
          es_header-fa_p_22b2          = ''.
          IF es_header-fa_p_22b2 IS INITIAL.
            es_header-fa_p_22b3          = ''.
            IF es_header-fa_p_22b3 IS INITIAL.
              es_header-fa_p_22b4          = ''.
            ENDIF.
          ENDIF.
        ENDIF.
        es_header-fa_p_22bt          = ''.
      ELSE.
        es_header-fa_p_22c           = ''.
        es_header-fa_p_22c1          = ''.
        IF es_header-fa_p_22c IS INITIAL.
          es_header-fa_p_22d           = ''.
          es_header-fa_p_22d1          = ''.
        ENDIF.
      ENDIF.
    ENDIF.
    es_header-fa_p_23            = '2'.
    IF es_header-fa_p_23 IS INITIAL.
      APPEND INITIAL LINE TO et_error ASSIGNING <ls_error>.
      <ls_error>-code = iv_docnum.
      <ls_error>-text = 'Fa_P_23'.
      <ls_error>-type = 'E'.
    ENDIF.

*--->PMarzy
    es_header-fa_p_pmarzyn       = '1'.
    IF es_header-fa_p_pmarzyn IS INITIAL.
      es_header-fa_p_pmarzy        = ''.
      IF  es_header-fa_p_pmarzy IS NOT INITIAL.
        es_header-fa_p_pmarzy_2      = ''.
        IF es_header-fa_p_pmarzy_2 IS INITIAL.
          es_header-fa_p_pmarzy_3_1    = ''.
          IF es_header-fa_p_pmarzy_3_1 IS INITIAL.
            es_header-fa_p_pmarzy_3_2    = ''.
            IF es_header-fa_p_pmarzy_3_2 IS INITIAL.
              es_header-fa_p_pmarzy_3_3    = ''.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CASE ls_main_info-fkart.
      WHEN 'F2'.
        es_header-fa_rodzajfaktury   = 'VAT'.
      WHEN 'G2'.
        es_header-fa_rodzajfaktury   = 'KOR'.
      WHEN 'S1'.
        es_header-fa_rodzajfaktury   = 'KOR'.
      WHEN 'FAZ'.
        es_header-fa_rodzajfaktury   = 'ZAL'.
*      WHEN 'FZ'.
*        es_header-fa_rodzajfaktury   = 'ROZ'.
*      WHEN ''.
*        es_header-fa_rodzajfaktury   = 'ZAL'.
*      WHEN ''.
*        es_header-fa_rodzajfaktury   = 'UPR'.
*      WHEN ''.
*        es_header-fa_rodzajfaktury   = 'KOR_ZAL'.
*      WHEN ''.
*        es_header-fa_rodzajfaktury   = 'KOR_ROZ'.
    ENDCASE.
*
    IF es_header-fa_rodzajfaktury = 'VAT'
    OR es_header-fa_rodzajfaktury = 'KOR'
    OR es_header-fa_rodzajfaktury = 'ROZ'.
**************Items*******************
      LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
        APPEND INITIAL LINE TO et_item ASSIGNING FIELD-SYMBOL(<ls_items>).
        <ls_items>-nrwierszafa = sy-tabix.
        <ls_items>-uu_id       = <ls_item>-posnr.
        <ls_items>-p_6a        = ''.
        <ls_items>-p_7         = <ls_item>-arktx.
        <ls_items>-indeks      = <ls_item>-matnr.
        <ls_items>-gtin        = ''.
        <ls_items>-pkwiu       = ''.
        <ls_items>-cn          = ''.
        <ls_items>-pkob        = ''.
        <ls_items>-p_8a        = <ls_item>-vrkme.
        <ls_items>-p_8b        = <ls_item>-lmeng.
        <ls_items>-p_9a        = ( <ls_item>-netwr ) / <ls_item>-lmeng.
        <ls_items>-p_9b        = <ls_item>-cmpre.
        <ls_items>-p_10        = <ls_item>-kzwi1 - <ls_item>-kzwi2.
        <ls_items>-p_11        = <ls_item>-netwr.
        <ls_items>-p_11a       = <ls_item>-netwr + <ls_item>-mwsbp.
        <ls_items>-p_11vat     = ''.
        <ls_items>-p_12        = ( <ls_item>-mwsbp * 100 ) / <ls_item>-netwr.
        <ls_items>-p_12_xii    = ''.
        <ls_items>-p_12_zal_15 = ''.
        <ls_items>-kwotaakcyzy = ''.
        <ls_items>-gtu         = ''.
        <ls_items>-procedura   = ''.
        <ls_items>-kurswaluty  = <ls_item>-kurrf.
        <ls_items>-stanprzed   = ''. "- '1' for old data in correction
      ENDLOOP.
*************************************************
    ENDIF.

*---->ZaliczkaCzesciowa
    es_header-fa_p_6z                 = ''.
    es_header-fa_p_15z                = ''.
    es_header-fa_kurswalutyzw         = ''.
    es_header-fa_fp                   = ''.
    es_header-fa_tp                   = ''.

*----->DodatkowyOpis
    es_header-fa_nrwiersza            = ''.
    es_header-fa_klucz                = ''.
    es_header-fa_wartosc              = ''.

*****************ROZ
*----->FakturaZaliczkowa
    IF es_header-fa_rodzajfaktury   = 'ROZ'
    OR es_header-fa_rodzajfaktury   = 'KOR_ROZ'.
      es_header-fa_nrkseffazaliczkowej  = ls_correction-ksef_num.
      IF ls_correction-ksef_num IS INITIAL.
        es_header-fa_nrksefzn             = 1.
        es_header-fa_nrfazaliczkowej      = ls_correction-vbeln.
        IF ls_correction-vbeln IS INITIAL.
          APPEND INITIAL LINE TO et_error ASSIGNING <ls_error>.
          <ls_error>-code = iv_docnum.
          <ls_error>-text = 'The down payment invoice was not found'.
          <ls_error>-type = 'E'.
        ENDIF.
      ENDIF.
    ENDIF.

    es_header-fa_zwrotakcyzy          = ''.
*---->Rozliczenie
*----->Obciazenia
    es_header-fa_obc_kwota            = ''.
    es_header-fa_obc_powod            = ''.
    es_header-fa_sumaobciazen         = ''.

*----->Odliczenia
    es_header-fa_odl_kwota            = ''.
    es_header-fa_odl_powod            = ''.
    es_header-fa_sumaodliczen         = ''.
    es_header-fa_dozaplaty            = ''.
    es_header-fa_dorozliczenia        = ''.
*----->Platnosc
    es_header-fa_zaplacono            = ''.
    es_header-fa_datazaplaty          = ''.
    es_header-fa_znacznikzaplatyczesciowej = ''.
*----->ZaplataCzesciowa
    es_header-fa_zc_kwotazaplatyczesciowej = ''.
    es_header-fa_zc_datazaplatyczesciowej  = ''.
    es_header-fa_zc_formaplatnosci         = ''.
    es_header-fa_zc_platnoscinna           = ''.
    es_header-fa_zc_opisplatnosci          = ''.
*----->TerminPlatnosci
    es_header-fa_termin                    = |{ lv_due DATE = ISO }|.
    IF es_header-fa_termin = '0000-00-00'.
      es_header-fa_termin = ''.
    ENDIF.
*----->TerminOpis
    es_header-fa_ilosc                     = ''.
    es_header-fa_jednostka                 = ''.
    es_header-fa_zdarzeniepoczatkowe       = ''.
    es_header-fa_formaplatnosci            = ''.
    es_header-fa_platnoscinna              = ''.
    es_header-fa_opisplatnosci             = ''.
*----->RachunekBankowy
    es_header-fa_nrrb                      = ''.
    es_header-fa_swift                     = ''.
    es_header-fa_rachunekwlasnybanku       = ''.
    es_header-fa_nazwabanku                = ''.
    es_header-fa_opisrachunku              = ''.

*----->???
    es_header-fa_rbf_nrrb_2                = ''.
    es_header-fa_rbf_swift_2               = ''.
    es_header-fa_rbf_rachunekwlasnybanku_2 = ''.
    es_header-fa_rbf_nazwabanku_2          = ''.
    es_header-fa_rbf_opisrachunku_2        = ''.

*----->Skonto
    es_header-fa_warunkiskonta             = ''.
    es_header-fa_wysokoscskonta            = ''.
    es_header-fa_linkdoplatnosci           = ''.
    es_header-fa_ipksef                    = ''.
*----->WarunkiTransakcji
*----->Umowy
    es_header-fa_dataumowy                 = ''.
    es_header-fa_nrumowy                   = ''.

*---->Zamowienia
    es_header-fa_datazamowienia            = ''.
    es_header-fa_nrzamowienia              = ls_main_info-sppord.
    es_header-fa_nrpartiitowaru            = ''.
    es_header-fa_warunkidostawy            = ''.
    es_header-fa_kursumowny                = ''.
    es_header-fa_walutaumowna              = ''.

*---->Transport
    es_header-fa_rodzajtransportu          = ''.
    es_header-fa_transportinny             = ''.
    es_header-fa_opisinnegotransportu      = ''.

*---->Przewoznik
*----->DaneIdentyfikacyjne
    es_header-fa_nip                       = ''.
    es_header-fa_kodue                     = ''.
    es_header-fa_nrvatue                   = ''.
    es_header-fa_kodkraju                  = ''.
    es_header-fa_nrid                      = ''.
    es_header-fa_brakid                    = ''.
    es_header-fa_nazwa                     = ''.

*------>AdresPrzewoznika
    es_header-fa_adr_kodkraju              = ''.
    es_header-fa_adr_adresl1               = ''.
    es_header-fa_adr_adresl2               = ''.
    es_header-fa_adr_gln                   = ''.
    es_header-fa_nrzleceniatransportu      = ''.
    es_header-fa_opisladunku               = ''.
    es_header-fa_ladunekinny               = ''.
    es_header-fa_opisinnegoladunku         = ''.
    es_header-fa_jednostkaopakowania       = ''.
    es_header-fa_datagodzrozptransportu    = ''.
    es_header-fa_datagodzzaktransportu     = ''.

*------>WysylkaZ
    es_header-fa_wysylkaz_kodkraju         = ''.
    es_header-fa_wysylkaz_adresl1          = ''.
    es_header-fa_wysylkaz_adresl2          = ''.
    es_header-fa_wysylkaz_gln              = ''.

*----->WysylkaPrzez
    es_header-fa_wysylkap_kodkraju         = ''.
    es_header-fa_wysylkap_adresl1          = ''.
    es_header-fa_wysylkap_adresl2          = ''.
    es_header-fa_wysylkap_gln              = ''.

*----->WysylkaDo
    es_header-fa_wysylkad_kodkraju         = ''.
    es_header-fa_wysylkad_adresl1          = ''.
    es_header-fa_wysylkad_adresl2          = ''.
    es_header-fa_wysylkad_gln              = ''.
    es_header-fa_podmiotposredniczacy      = ''.

*************Down payment
    IF es_header-fa_rodzajfaktury = 'ZAL'
    OR es_header-fa_rodzajfaktury = 'KOR_ZAL'.
      DATA lv_wartosczamowienia TYPE p LENGTH 16 DECIMALS 2 VALUE 0.
      LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<ls_zal_item>).
        APPEND INITIAL LINE TO et_zal_items ASSIGNING FIELD-SYMBOL(<ls_zal_items>).
*---->Zamowienie
*----->ZamowienieWiersz                          "mandatory for zam
        <ls_zal_items>-nrwierszazam              = sy-tabix.
        <ls_zal_items>-uu_idz                    = <ls_zal_item>-posnr.
        <ls_zal_items>-p_7z                      = <ls_zal_item>-arktx.
        <ls_zal_items>-indeksz                   = <ls_zal_item>-matnr.
        <ls_zal_items>-gtinz                     = ''.
        <ls_zal_items>-pkwiuz                    = ''.
        <ls_zal_items>-cnz                       = ''.
        <ls_zal_items>-pkobz                     = ''.
        <ls_zal_items>-p_8az                     = <ls_zal_item>-vrkme.
        <ls_zal_items>-p_8bz                     = <ls_zal_item>-lmeng.
        <ls_zal_items>-p_9az                     = ( <ls_zal_item>-netwr ) / <ls_zal_item>-lmeng.
        <ls_zal_items>-p_11nettoz                = <ls_zal_item>-netwr.
        <ls_zal_items>-p_11vatz                  = <ls_zal_item>-netwr + <ls_zal_item>-mwsbp.
        <ls_zal_items>-p_12z                     = ( <ls_zal_item>-mwsbp * 100 ) / <ls_zal_item>-netwr.
        <ls_zal_items>-p_12z_xii                 = ''.
        <ls_zal_items>-p_12z_zal_15              = ''.
        <ls_zal_items>-gtuz                      = ''.
        <ls_zal_items>-proceduraz                = ''.
        <ls_zal_items>-kwotaakcyzyz              = ''.
        <ls_zal_items>-stanprzedz                = ''. "- '1' for old data in correction

        lv_wartosczamowienia = lv_wartosczamowienia + <ls_zal_items>-p_11nettoz + <ls_zal_items>-p_11vatz .
      ENDLOOP.
      es_header-fa_wartosczamowienia         = lv_wartosczamowienia. "mandatory
    ENDIF.
*---->Stopka
*----->Informacje
    es_header-st_stopkafaktury             = ''.
*----->Rejestry
    es_header-st_pelnanazwa                = ''.
    es_header-st_krs                       = ''.
    es_header-st_regon                     = ''.
    es_header-st_bdo                       = ''.

**********Correction invoice
*--->DaneFaKorygowanej
    IF es_header-fa_rodzajfaktury = 'KOR'
    OR es_header-fa_rodzajfaktury = 'KOR_ZAL'
    OR es_header-fa_rodzajfaktury = 'KOR_ROZ'.
      es_header-fa_przyczynakorekty = ''.
      es_header-fa_typkorekty       = ''.
      es_header-fa_datawystfakorygowanej = ls_correction-xmlgen_date.
      es_header-fa_nrfakorygowanej = ls_correction-vbeln.
      es_header-fa_nrkseffakorygowanej  = ls_correction-ksef_num.
      IF es_header-fa_nrkseffakorygowanej IS NOT INITIAL.
        es_header-fa_nrksef  = 1.
      ELSE.
        es_header-fa_nrksefn = 1.
      ENDIF.
      es_header-fa_okresfakorygowanej   = ''.  "period of correction
      es_header-fa_nrfakorygowany       = ''.  "true invoice number if it was wrong

      IF ls_correction-xml IS NOT INITIAL.

        DATA ls_pod1k TYPE zlx_ksef_podmiot.
        DATA ls_pod2k TYPE zlx_ksef_podmiot.
        DATA lv_fill_p1k TYPE abap_bool.
        DATA lv_fill_p2k TYPE abap_bool.

        """"""""""""""Prepare structures for comparison
        DATA ls_new_pod1 TYPE zlx_ksef_podmiot.
        DATA ls_new_pod2 TYPE zlx_ksef_podmiot.

        ls_new_pod1-prefikspodatnika = es_header-p1_prefikspodatnika.
        ls_new_pod1-nip              = es_header-p1_nip.
        ls_new_pod1-nazwa            = es_header-p1_nazwa.
        ls_new_pod1-kodkraju         = es_header-p1_kodkraju.
        ls_new_pod1-adresl1          = es_header-p1_adresl1.
        ls_new_pod1-adresl2          = es_header-p1_adresl2.
        ls_new_pod1-gln              = es_header-p1_gln.

        ls_new_pod2-nip              = es_header-p2_nip.
        ls_new_pod2-kodue            = es_header-p2_kodue.
        ls_new_pod2-nrvatue          = es_header-p2_nrvatue.
        ls_new_pod2-nrid             = es_header-p2_nrid.
        ls_new_pod2-brakid           = es_header-p2_brakid.
        ls_new_pod2-nazwa            = es_header-p2_nazwa.
        ls_new_pod2-kodkraju         = es_header-p2_adr_kodkraju.
        ls_new_pod2-adresl1          = es_header-p2_adr_adresl1.
        ls_new_pod2-adresl2          = es_header-p2_adr_adresl2.
        ls_new_pod2-gln              = es_header-p2_adr_gln.


        """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        me->fill_kor_pod1( EXPORTING iv_xml_old    = ls_correction-xml
                                     is_new_pod1   = ls_new_pod1
                           IMPORTING es_pod1k      = ls_pod1k
                                     ev_fill_pod1k = lv_fill_p1k ).

        IF lv_fill_p1k = abap_true.
*---->Podmiot1K
          es_header-fa_p1k_prefikspodatnika = ls_pod1k-prefikspodatnika.
*----->DaneIdentyfikacyjne
          es_header-fa_p1k_nip              = ls_pod1k-nip.
          es_header-fa_p1k_nazwa            = ls_pod1k-nazwa.
*----->Adres
          es_header-fa_p1k_kodkraju         = ls_pod1k-kodkraju.
          es_header-fa_p1k_adresl1          = ls_pod1k-adresl1.
          es_header-fa_p1k_adresl2          = ls_pod1k-adresl2.
          es_header-fa_p1k_gln              = ls_pod1k-gln.
        ENDIF.

        me->fill_pod2k(
          EXPORTING
            iv_xml_old      = ls_correction-xml
            is_new_pod2     = ls_new_pod2
            it_new_podmiot3 = et_podmiot3
          IMPORTING
            et_podmiot2k    = et_podmiot2k ).

*-----------Items
        IF et_item IS NOT INITIAL.
          DATA(lv_items_changed) = me->items_differs( iv_xml_old   = ls_correction-xml
                                                      it_items_new = et_item ).
          IF lv_items_changed = abap_true.
            DATA lt_items_out TYPE tt_items.
            DATA lt_err_items TYPE zkstg_t_msg.

            me->fill_items_kor( EXPORTING iv_xml_old   = ls_correction-xml
                                           it_items_new = et_item
                                 IMPORTING et_items_out = lt_items_out
                                           et_error     = lt_err_items ).

            et_item  = lt_items_out.
            APPEND LINES OF lt_err_items TO et_error.
          ELSE.
            CLEAR et_item.
          ENDIF.
        ENDIF.
*----------------<
*-----------ZamowienieWiersz
        IF et_zal_items IS NOT INITIAL.
          DATA(lv_zitems_changed) = me->zal_items_differs( iv_xml_old   = ls_correction-xml
                                                           it_zitems_new = et_zal_items ).
          IF lv_zitems_changed = abap_true.
            DATA lt_zitems_out TYPE tt_zal_items.
            DATA lt_err_zitems TYPE zkstg_t_msg.

            me->fill_zal_items_kor( EXPORTING iv_xml_old    = ls_correction-xml
                                              it_zitems_new = et_zal_items
                                    IMPORTING et_zitems_out = lt_zitems_out
                                              et_error      = lt_err_items ).

            et_zal_items  = lt_zitems_out.
            APPEND LINES OF lt_err_items TO et_error.
          ELSE.
            CLEAR et_zal_items.
          ENDIF.
        ENDIF.
*----------------<
        IF es_header-fa_rodzajfaktury = 'KOR_ZAL'
        OR es_header-fa_rodzajfaktury = 'KOR_ROZ'.
          me->parse_tag_from_xml( EXPORTING iv_xml      = ls_correction-xml
                                            iv_tagname1 = 'P_15'
                                            iv_tagname2 = 'KursWalutyZ'
                                  IMPORTING ev_tagname1 = es_header-fa_p_15zk
                                            ev_tagname2 = es_header-fa_kurswalutyzk ).
*        es_header-fa_p_15zk               = ''. "sum before correction
*        es_header-fa_kurswalutyzk         = ''. "exchange rate
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO et_error ASSIGNING <ls_error>.
        <ls_error>-code = iv_docnum.
        <ls_error>-text = 'The corrected invoice was not found'.
        <ls_error>-type = 'E'.
      ENDIF.
    ENDIF.
*********************************************************

  ENDMETHOD.
  METHOD fill_kor_pod1.
    DATA(ls_old_pod1) = me->parse_podmiot_from_xml( iv_xml     = iv_xml_old
                                                    iv_tagname = 'Podmiot1' ).
    ev_fill_pod1k = abap_false.
    CLEAR: es_pod1k.

    IF me->podmiot_differs( is_a = ls_old_pod1 is_b = is_new_pod1 ) = abap_true.
      es_pod1k      = ls_old_pod1.
      ev_fill_pod1k = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD parse_podmiot_from_xml.
    DATA lv_xml_xstr TYPE xstring.
    TRY.
        lv_xml_xstr = cl_abap_codepage=>convert_to( source = iv_xml codepage  = 'UTF-8' ).
      CATCH cx_root INTO DATA(lx).
    ENDTRY.

    DATA(lo_reader) = cl_sxml_string_reader=>create( input = lv_xml_xstr ).
    DATA(lv_in_target) = abap_false.
    DATA lv_name TYPE string.

    CLEAR rs_podmiot.

    TRY.
        DO.
          lo_reader->next_node( ).

          lv_name = lo_reader->name.

          CASE lo_reader->node_type.

            WHEN if_sxml_node=>co_nt_element_open.

              IF lv_name = iv_tagname.
                lv_in_target = abap_true.
                CONTINUE.
              ENDIF.

              IF lv_in_target = abap_true.
                CASE lv_name.
                  WHEN 'PrefiksPodatnika'.
                    rs_podmiot-prefikspodatnika = me->read_text_value( lo_reader ).
                  WHEN 'NIP'.
                    rs_podmiot-nip = me->read_text_value( lo_reader ).
                  WHEN 'KodUE'.
                    rs_podmiot-kodue = me->read_text_value( lo_reader ).
                  WHEN 'NrVatUE'.
                    rs_podmiot-nrvatue = me->read_text_value( lo_reader ).
                  WHEN 'NrID'.
                    rs_podmiot-nrid = me->read_text_value( lo_reader ).
                  WHEN 'BrakID'.
                    rs_podmiot-brakid = me->read_text_value( lo_reader ).
                  WHEN 'Nazwa'.
                    rs_podmiot-nazwa = me->read_text_value( lo_reader ).
                  WHEN 'KodKraju'.
                    rs_podmiot-kodkraju = me->read_text_value( lo_reader ).
                  WHEN 'AdresL1'.
                    rs_podmiot-adresl1 = me->read_text_value( lo_reader ).
                  WHEN 'AdresL2'.
                    rs_podmiot-adresl2 = me->read_text_value( lo_reader ).
                  WHEN 'GLN'.
                    rs_podmiot-gln = me->read_text_value( lo_reader ).
                ENDCASE.
              ENDIF.

            WHEN if_sxml_node=>co_nt_element_close.
              IF lv_in_target = abap_true AND lv_name = iv_tagname.
                EXIT.
              ENDIF.

          ENDCASE.

        ENDDO.

      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD read_text_value.
    rv_val = ``.

    TRY.
        io_reader->next_node( ).

        IF io_reader->node_type = if_sxml_node=>co_nt_value.
          rv_val = io_reader->value.
        ELSE.
          rv_val = ``.
        ENDIF.

      CATCH cx_sxml_parse_error.
        rv_val = ``.
    ENDTRY.
  ENDMETHOD.

  METHOD podmiot_differs.
    rv_diff = abap_false.

    IF is_a-prefikspodatnika <> is_b-prefikspodatnika. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-nip <> is_b-nip. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-kodue <> is_b-kodue. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-nrvatue <> is_b-nrvatue. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-nrid <> is_b-nrid. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-brakid <> is_b-brakid. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-nazwa <> is_b-nazwa. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-kodkraju <> is_b-kodkraju. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-adresl1 <> is_b-adresl1. rv_diff = abap_true. RETURN. ENDIF.
    IF is_a-adresl2 <> is_b-adresl2. rv_diff = abap_true.  RETURN. ENDIF.
    IF is_a-gln <> is_b-gln. rv_diff = abap_true. RETURN. ENDIF.

  ENDMETHOD.

  METHOD fill_items_kor.
    CLEAR: et_items_out, et_error.

    DATA(lt_items_old) = me->parse_items_from_xml( iv_xml_old = iv_xml_old ).

    DATA lt_old_h TYPE HASHED TABLE OF zksef_s_item WITH UNIQUE KEY uu_id.
    lt_old_h = lt_items_old.

    DATA lt_new_h TYPE HASHED TABLE OF zksef_s_item WITH UNIQUE KEY uu_id.
    lt_new_h = it_items_new.

    FIELD-SYMBOLS: <ls_new> TYPE zksef_s_item,
                   <ls_old> TYPE zksef_s_item,
                   <ls_err> TYPE zkstg_msg.

    LOOP AT it_items_new ASSIGNING <ls_new>.

      DATA(ls_new_out) = <ls_new>.
      ls_new_out-stanprzed = ''.

      READ TABLE lt_old_h ASSIGNING <ls_old> WITH TABLE KEY uu_id = <ls_new>-uu_id.
      IF sy-subrc = 0.
        DATA(ls_old_out) = <ls_old>.
        ls_old_out-stanprzed = '1'.

        ls_old_out-nrwierszafa = <ls_new>-nrwierszafa.

        APPEND ls_old_out TO et_items_out.
        APPEND ls_new_out TO et_items_out.
      ELSE.
        APPEND ls_new_out TO et_items_out.

        APPEND INITIAL LINE TO et_error ASSIGNING <ls_err>.
        <ls_err>-type = 'W'.
        <ls_err>-code = <ls_new>-uu_id.
        <ls_err>-text = |No matching old item in XML for uu_id={ <ls_new>-uu_id }|.
      ENDIF.

    ENDLOOP.

    LOOP AT lt_items_old ASSIGNING <ls_old>.
      READ TABLE lt_new_h TRANSPORTING NO FIELDS WITH TABLE KEY uu_id = <ls_old>-uu_id.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO et_error ASSIGNING <ls_err>.
        <ls_err>-type = 'W'.
        <ls_err>-code = <ls_old>-uu_id.
        <ls_err>-text = |Old item exists in XML but not in new SAP items, uu_id={ <ls_old>-uu_id }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_items_from_xml.
    DATA lv_xml_xstr TYPE xstring.
    TRY.
        lv_xml_xstr = cl_abap_codepage=>convert_to( source = iv_xml_old codepage = 'UTF-8' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    DATA(lo_reader) = cl_sxml_string_reader=>create( input = lv_xml_xstr ).

    DATA lv_in_row TYPE abap_bool VALUE abap_false.
    DATA lv_name   TYPE string.

    DATA ls_item TYPE zksef_s_item.
    CLEAR rt_items_old.

    TRY.
        DO.
          lo_reader->next_node( ).
          lv_name = lo_reader->name.

          CASE lo_reader->node_type.

            WHEN if_sxml_node=>co_nt_element_open.

              IF lv_name = 'FaWiersz'.
                CLEAR ls_item.
                lv_in_row = abap_true.
                CONTINUE.
              ENDIF.

              IF lv_in_row = abap_true.
                CASE lv_name.
                  WHEN 'NrWierszaFa'.
                    ls_item-nrwierszafa = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'UU_ID'.
                    ls_item-uu_id = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_6A'.
                    ls_item-p_6a = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_7'.
                    ls_item-p_7 = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'Indeks'.
                    ls_item-indeks = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'GTIN'.
                    ls_item-gtin = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'PKWIU'.
                    ls_item-pkwiu = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'CN'.
                    ls_item-cn = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'PKOB'.
                    ls_item-pkob = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_8A'.
                    ls_item-p_8a = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_8B'.
                    ls_item-p_8b = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_9A'.
                    ls_item-p_9a = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_9B'.
                    ls_item-p_9b = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_10'.
                    ls_item-p_10 = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_11'.
                    ls_item-p_11 = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_11A'.
                    ls_item-p_11a = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_11Vat'.
                    ls_item-p_11vat = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_12'.
                    ls_item-p_12 = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_12_XII'.
                    ls_item-p_12_xii = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_12_Zal_15'.
                    ls_item-p_12_zal_15 = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'KwotaAkcyzy'.
                    ls_item-kwotaakcyzy = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'GTU'.
                    ls_item-gtu = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'Procedura'.
                    ls_item-procedura = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'KursWaluty'.
                    ls_item-kurswaluty = me->read_text_value( io_reader = lo_reader ).
                ENDCASE.
              ENDIF.

            WHEN if_sxml_node=>co_nt_element_close.

              IF lv_in_row = abap_true
                 AND ( lv_name = 'FaWiersz' ).

                lv_in_row = abap_false.
                ls_item-stanprzed = ''.

                IF ls_item-uu_id IS NOT INITIAL.
                  APPEND ls_item TO rt_items_old.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD items_differs.

    rv_diff = abap_false.

    DATA(lt_old) = me->parse_items_from_xml( iv_xml_old = iv_xml_old ).

    IF lt_old IS INITIAL OR it_items_new IS INITIAL.
      IF lt_old IS INITIAL AND it_items_new IS INITIAL.
        rv_diff = abap_false.
      ELSE.
        rv_diff = abap_true.
      ENDIF.
      RETURN.
    ENDIF.

    DATA lt_old_h TYPE HASHED TABLE OF zksef_s_item WITH UNIQUE KEY uu_id.
    lt_old_h = lt_old.

    FIELD-SYMBOLS: <ls_new> TYPE zksef_s_item,
                   <ls_old> TYPE zksef_s_item.

    LOOP AT it_items_new ASSIGNING <ls_new>.
      READ TABLE lt_old_h ASSIGNING <ls_old> WITH TABLE KEY uu_id = <ls_new>-uu_id.
      IF sy-subrc <> 0.
        rv_diff = abap_true.
        RETURN.
      ENDIF.

      IF <ls_new>-p_6a  <> <ls_old>-p_6a  OR
         <ls_new>-p_7   <> <ls_old>-p_7   OR
         <ls_new>-p_8a  <> <ls_old>-p_8a  OR
         <ls_new>-p_8b  <> <ls_old>-p_8b  OR
         <ls_new>-p_9a  <> <ls_old>-p_9a  OR
         <ls_new>-p_9b  <> <ls_old>-p_9b  OR
         <ls_new>-p_10  <> <ls_old>-p_10  OR
         <ls_new>-p_11  <> <ls_old>-p_11  OR
         <ls_new>-p_11a <> <ls_old>-p_11a OR
         <ls_new>-p_11vat <> <ls_old>-p_11vat OR
         <ls_new>-p_12  <> <ls_old>-p_12.
        rv_diff = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.

    DATA lt_new_h TYPE HASHED TABLE OF zksef_s_item WITH UNIQUE KEY uu_id.
    lt_new_h = it_items_new.

    LOOP AT lt_old ASSIGNING <ls_old>.
      READ TABLE lt_new_h TRANSPORTING NO FIELDS WITH TABLE KEY uu_id = <ls_old>-uu_id.
      IF sy-subrc <> 0.
        rv_diff = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD fill_pod2k.
    CLEAR et_podmiot2k.

    DATA(ls_old_p2) = me->parse_podmiot_from_xml(
                        iv_xml     = iv_xml_old
                        iv_tagname = 'Podmiot2' ).

    DATA(lt_old_p3) = me->parse_podmiot3_from_xml( iv_xml = iv_xml_old ).

    " 1) Podmiot2
    IF me->podmiot_differs( is_a = ls_old_p2 is_b = is_new_pod2 ) = abap_true.
      APPEND ls_old_p2 TO et_podmiot2k.
    ENDIF.

    " 2) Podmiot3 (in order)
    DATA lv_idx TYPE sy-tabix.
    LOOP AT lt_old_p3 ASSIGNING FIELD-SYMBOL(<ls_old_p3>).
      lv_idx = sy-tabix.

      READ TABLE it_new_podmiot3 ASSIGNING FIELD-SYMBOL(<ls_new_p3>) INDEX lv_idx.
      IF sy-subrc <> 0.
        APPEND  <ls_old_p3> TO et_podmiot2k.
        CONTINUE.
      ENDIF.

      DATA(ls_old) = <ls_old_p3> .
      DATA(ls_new) = <ls_new_p3> .

      IF me->podmiot_differs( is_a = ls_old is_b = CONV #( ls_new ) ) = abap_true.
        APPEND ls_old TO et_podmiot2k.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD parse_podmiot3_from_xml.
    CLEAR rt_pod3_old.

    DATA lv_xml_xstr TYPE xstring.
    TRY.
        lv_xml_xstr = cl_abap_codepage=>convert_to( source = iv_xml codepage = 'UTF-8' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    DATA(lo_reader) = cl_sxml_string_reader=>create( input = lv_xml_xstr ).

    DATA: lv_name  TYPE string,
          lv_in_p3 TYPE abap_bool VALUE abap_false,
          ls_p3    TYPE zlx_ksef_podmiot.

    TRY.
        DO.
          lo_reader->next_node( ).

          lv_name = lo_reader->name.

          CASE lo_reader->node_type.
            WHEN if_sxml_node=>co_nt_element_open.
              IF lv_name = 'Podmiot3'.
                CLEAR ls_p3.
                lv_in_p3 = abap_true.
                CONTINUE.
              ENDIF.

              IF lv_in_p3 = abap_true.

                CASE lv_name.
                  WHEN 'IDNabywcy'.
                    ls_p3-idnabywcy = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'NIP'.
                    ls_p3-nip = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'KodUE'.
                    ls_p3-kodue = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'NrVatUE'.
                    ls_p3-nrvatue = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'NrID'.
                    ls_p3-nrid = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'BrakID'.
                    ls_p3-brakid = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'Nazwa'.
                    ls_p3-nazwa = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'KodKraju'.
                    ls_p3-kodkraju = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'AdresL1'.
                    ls_p3-adresl1 = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'AdresL2'.
                    ls_p3-adresl2 = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'GLN'.
                    ls_p3-gln = me->read_text_value( io_reader = lo_reader ).
                ENDCASE.
              ENDIF.

            WHEN if_sxml_node=>co_nt_element_close.

              IF lv_in_p3 = abap_true AND lv_name = 'Podmiot3'.
                IF ls_p3-nip IS NOT INITIAL
                   OR ls_p3-nazwa IS NOT INITIAL
                   OR ls_p3-idnabywcy IS NOT INITIAL.
                  APPEND ls_p3 TO rt_pod3_old.
                ENDIF.

                CLEAR ls_p3.
                lv_in_p3 = abap_false.
                CONTINUE.
              ENDIF.

              IF lv_in_p3 = abap_true.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zal_items_differs.
    rv_diff = abap_false.

    DATA(lt_old) = me->parse_zal_items_from_xml( iv_xml_old = iv_xml_old ).

    IF lt_old IS INITIAL OR it_zitems_new IS INITIAL.
      IF lt_old IS INITIAL AND it_zitems_new IS INITIAL.
        rv_diff = abap_false.
      ELSE.
        rv_diff = abap_true.
      ENDIF.
      RETURN.
    ENDIF.

    DATA lt_old_h TYPE HASHED TABLE OF zlx_ksef_zal_items WITH UNIQUE KEY uu_idz.
    lt_old_h = lt_old.

    FIELD-SYMBOLS: <ls_zal_new> TYPE zlx_ksef_zal_items,
                   <ls_zal_old> TYPE zlx_ksef_zal_items.

    LOOP AT it_zitems_new ASSIGNING <ls_zal_new>.
      READ TABLE lt_old_h ASSIGNING <ls_zal_old> WITH TABLE KEY uu_idz = <ls_zal_new>-uu_idz.
      IF sy-subrc <> 0.
        rv_diff = abap_true.
        RETURN.
      ENDIF.

      IF <ls_zal_new>-p_7z       <> <ls_zal_old>-p_7z       OR
         <ls_zal_new>-indeksz    <> <ls_zal_old>-indeksz    OR
         <ls_zal_new>-p_8az      <> <ls_zal_old>-p_8az      OR
         <ls_zal_new>-p_8bz      <> <ls_zal_old>-p_8bz      OR
         <ls_zal_new>-p_9az      <> <ls_zal_old>-p_9az      OR
         <ls_zal_new>-p_11nettoz <> <ls_zal_old>-p_11nettoz OR
         <ls_zal_new>-p_11vatz   <> <ls_zal_old>-p_11vatz   OR
         <ls_zal_new>-p_12z      <> <ls_zal_old>-p_12z.

        rv_diff = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.

    DATA lt_new_h TYPE HASHED TABLE OF zlx_ksef_zal_items WITH UNIQUE KEY uu_idz.
    lt_new_h = it_zitems_new.

    LOOP AT lt_old ASSIGNING <ls_zal_old>.
      READ TABLE lt_new_h TRANSPORTING NO FIELDS WITH TABLE KEY uu_idz = <ls_zal_old>-uu_idz.
      IF sy-subrc <> 0.
        rv_diff = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_zal_items_from_xml.

    DATA lv_xml_xstr TYPE xstring.
    TRY.
        lv_xml_xstr = cl_abap_codepage=>convert_to( source = iv_xml_old codepage = 'UTF-8' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    DATA(lo_reader) = cl_sxml_string_reader=>create( input = lv_xml_xstr ).

    DATA lv_in_row TYPE abap_bool VALUE abap_false.
    DATA lv_name   TYPE string.

    DATA ls_zitem TYPE zlx_ksef_zal_items.
    CLEAR rt_zitems_old.

    TRY.
        DO.
          lo_reader->next_node( ).
          lv_name = lo_reader->name.

          CASE lo_reader->node_type.

            WHEN if_sxml_node=>co_nt_element_open.

              IF lv_name = 'ZamowienieWiersz'.
                CLEAR ls_zitem.
                lv_in_row = abap_true.
                CONTINUE.
              ENDIF.

              IF lv_in_row = abap_true.
                CASE lv_name.
                  WHEN 'NrWierszaZam'.
                    ls_zitem-nrwierszazam = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'UU_IDZ'.
                    ls_zitem-uu_idz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_7Z'.
                    ls_zitem-p_7z = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'IndeksZ'.
                    ls_zitem-indeksz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'GTINZ'.
                    ls_zitem-gtinz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'PKWiUZ'.
                    ls_zitem-pkwiuz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'CNZ'.
                    ls_zitem-cnz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'PKOBZ'.
                    ls_zitem-pkobz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_8AZ'.
                    ls_zitem-p_8az = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_8BZ'.
                    ls_zitem-p_8bz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_9AZ'.
                    ls_Zitem-p_9az = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_11NettoZ'.
                    ls_zitem-p_11nettoz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_11VatZ'.
                    ls_zitem-p_11vatz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_12Z'.
                    ls_zitem-p_12z = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_12Z_XII'.
                    ls_zitem-p_12z_xii = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'P_12Z_Zal_15'.
                    ls_zitem-p_12z_zal_15 = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'KwotaAkcyzyZ'.
                    ls_zitem-kwotaakcyzyz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'GTUZ'.
                    ls_zitem-gtuz = me->read_text_value( io_reader = lo_reader ).
                  WHEN 'ProceduraZ'.
                    ls_zitem-proceduraz = me->read_text_value( io_reader = lo_reader ).
                ENDCASE.
              ENDIF.

            WHEN if_sxml_node=>co_nt_element_close.

              IF lv_in_row = abap_true
                 AND ( lv_name = 'ZamowienieWiersz' ).

                lv_in_row = abap_false.
                ls_zitem-stanprzedz = ''.

                IF ls_zitem-uu_idz IS NOT INITIAL.
                  APPEND ls_zitem TO rt_zitems_old.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.

  ENDMETHOD.

  METHOD fill_zal_items_kor.
    CLEAR: et_zitems_out, et_error.

    DATA(lt_zitems_old) = me->parse_zal_items_from_xml( iv_xml_old = iv_xml_old ).

    DATA lt_old_h TYPE HASHED TABLE OF zlx_ksef_zal_items WITH UNIQUE KEY uu_idz.
    lt_old_h = lt_zitems_old.

    DATA lt_new_h TYPE HASHED TABLE OF zlx_ksef_zal_items WITH UNIQUE KEY uu_idz.
    lt_new_h = it_zitems_new.

    FIELD-SYMBOLS: <ls_zal_new> TYPE zlx_ksef_zal_items,
                   <ls_zal_old> TYPE zlx_ksef_zal_items,
                   <ls_err>     TYPE zkstg_msg.

    LOOP AT it_zitems_new ASSIGNING <ls_zal_new>.

      DATA(ls_zal_new_out) = <ls_zal_new>.
      ls_zal_new_out-stanprzedz = ''.

      READ TABLE lt_old_h ASSIGNING <ls_zal_old> WITH TABLE KEY uu_idz = <ls_zal_new>-uu_idz.
      IF sy-subrc = 0.
        DATA(ls_zal_old_out) = <ls_zal_old>.
        ls_zal_old_out-stanprzedz = '1'.

        ls_zal_old_out-nrwierszazam = <ls_zal_new>-nrwierszazam.

        APPEND ls_zal_old_out TO et_zitems_out.
        APPEND ls_zal_new_out TO et_zitems_out.
      ELSE.
        APPEND ls_zal_new_out TO et_zitems_out.

        APPEND INITIAL LINE TO et_error ASSIGNING <ls_err>.
        <ls_err>-type = 'W'.
        <ls_err>-code = <ls_zal_new>-uu_idz.
        <ls_err>-text = |No matching old item in XML for uu_idz={ <ls_zal_new>-uu_idz }|.
      ENDIF.

    ENDLOOP.

    LOOP AT lt_zitems_old ASSIGNING <ls_zal_old>.
      READ TABLE lt_new_h TRANSPORTING NO FIELDS WITH TABLE KEY uu_idz = <ls_zal_old>-uu_idz.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO et_error ASSIGNING <ls_err>.
        <ls_err>-type = 'W'.
        <ls_err>-code = <ls_zal_old>-uu_idz.
        <ls_err>-text = |Old item exists in XML but not in new SAP items, uu_idz={ <ls_zal_old>-uu_idz }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD parse_tag_from_xml.
    DATA lv_xml_xstr TYPE xstring.
    TRY.
        lv_xml_xstr = cl_abap_codepage=>convert_to( source = iv_xml codepage  = 'UTF-8' ).
      CATCH cx_root INTO DATA(lx).
    ENDTRY.

    DATA(lo_reader) = cl_sxml_string_reader=>create( input = lv_xml_xstr ).
    DATA(lv_in_target) = abap_false.
    DATA lv_name TYPE string.

    TRY.
        DO.
          lo_reader->next_node( ).
          lv_name = lo_reader->name.

          CASE lo_reader->node_type.
            WHEN if_sxml_node=>co_nt_element_open.
              IF lv_name = iv_tagname1.
                ev_tagname1 = me->read_text_value( io_reader = lo_reader ).
                lv_in_target = abap_true.
                CONTINUE.
              ENDIF.
              IF lv_name = iv_tagname2.
                ev_tagname2 = me->read_text_value( io_reader = lo_reader ).
                lv_in_target = abap_true.
                CONTINUE.
              ENDIF.
            WHEN if_sxml_node=>co_nt_element_close.
              IF lv_in_target = abap_true AND ev_tagname1 IS NOT INITIAL AND ev_tagname2 IS NOT INITIAL.
                EXIT.
              ENDIF.
          ENDCASE.
        ENDDO.
      CATCH cx_sxml_parse_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
