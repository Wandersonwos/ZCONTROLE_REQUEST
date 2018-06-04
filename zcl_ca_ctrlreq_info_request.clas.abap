************************************************************************
************************************************************************
*                  ██████╗     ██████╗   ███████╗                      *
*                  ██╔══██╗   ██╔════╝   ██╔════╝                      *
*                  ██████╔╝   ██║        ███████╗                      *
*                  ██╔══██╗   ██║        ╚════██║                      *
*                  ██║  ██║██╗╚██████╗██╗███████║                      *
*                  ╚═╝  ╚═╝╚═╝ ╚═════╝╚═╝╚══════╝                      *
************************************************************************
************************************************************************
* Request Control System
* This tool enabble to:
* Standardize requests
* Copy request transport tool
* Log Transport
************************************************************************
* Gui Version(2008 - 2016)
*                       Bruno Rodrigues(BSR) - brunosilva.r@gmail.com
* Web Version(2016 - 2017)
*                       Bruno Rodrigues(BSR) - brunosilva.r@gmail.com
*                       Wanderson Oliveira   - wanderson.wos@gmail.com
*                       Paulo Cabral         - paulohscabral@gmail.com
*                       Rafael Viana         -
************************************************************************
* All rights Reserved©®
************************************************************************

CLASS zcl_ca_ctrlreq_info_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_nome_ambientes,
        dev TYPE sy-sysid VALUE 'ED0',
        qas TYPE sy-sysid VALUE 'EQ0',
        prd TYPE sy-sysid VALUE 'EP0',
      END OF c_nome_ambientes.

    TYPES:
      BEGIN OF ty_ambientes_solicitados,
        dev TYPE flag,
        qas TYPE flag,
        prd TYPE flag,
      END OF ty_ambientes_solicitados,
      tty_ctslg_system TYPE TABLE OF ctslg_system .

    METHODS get_status_transp_ambientes
      IMPORTING
        iw_ambientes TYPE ty_ambientes_solicitados
      CHANGING
        !c_request   TYPE zif_ca_ctrlreq_request_reposit=>ty_report.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS get_rel_info
      IMPORTING
        iw_ambientes TYPE ty_ambientes_solicitados
      CHANGING
        !c_rdev      TYPE any
        !c_rqas      TYPE any
        !c_rprd      TYPE any
        !c_request   TYPE zif_ca_ctrlreq_request_reposit=>ty_report .
    METHODS set_sid_data
      IMPORTING
        !i_ambiente TYPE c
        i_request   TYPE zif_ca_ctrlreq_request_reposit=>ty_report
      CHANGING
        !c_systems  TYPE tty_ctslg_system
        !c_info     TYPE any
        !c_infot    TYPE any
        !c_hora     TYPE any .

ENDCLASS.



CLASS ZCL_CA_CTRLREQ_INFO_REQUEST IMPLEMENTATION.


  METHOD get_rel_info.

    DATA: lt_comm_systems TYPE STANDARD TABLE OF tmscsyslst,
          lt_es_cofile    TYPE STANDARD TABLE OF ctslg_cofile,
          lt_systems      TYPE STANDARD TABLE OF ctslg_system,
          lw_es_cofile    TYPE ctslg_cofile.

    IF iw_ambientes-dev = abap_true.
      APPEND VALUE #(  sysnam = c_nome_ambientes-dev ) TO lt_comm_systems.
    ENDIF.

    IF iw_ambientes-qas = abap_true.
      APPEND VALUE #(  sysnam = c_nome_ambientes-qas ) TO lt_comm_systems.
    ENDIF.

    IF iw_ambientes-prd = abap_true.
      APPEND VALUE #(  sysnam = c_nome_ambientes-prd ) TO lt_comm_systems.
    ENDIF.

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr       = c_request-trkorr
        iv_dir_type     = 'T'
        it_comm_systems = lt_comm_systems
      IMPORTING
        es_cofile       = lw_es_cofile.
*
    APPEND lw_es_cofile TO lt_es_cofile.
    READ TABLE lt_es_cofile INDEX 1 ASSIGNING FIELD-SYMBOL(<lw_field_cofile>).
    MOVE <lw_field_cofile>-systems TO lt_systems.

    IF iw_ambientes-dev = abap_true.

      CLEAR c_request-hdev.

      me->set_sid_data(
        EXPORTING
          i_ambiente = c_nome_ambientes-dev
          i_request  = c_request
        CHANGING
          c_systems  = lt_systems
          c_info     = c_rdev
          c_infot    = c_request-tdev
          c_hora     = c_request-hdev      ).

      IF c_rdev IS INITIAL.
        CLEAR c_request-tdev.
        CLEAR c_request-hdev.
      ELSE.
        c_request-dataex = c_request-tdev.
        c_request-horaex = c_request-hdev.
      ENDIF.

    ENDIF.

    IF c_rdev = zif_ca_ctrlreq_request=>cc_status-nao_transportado.
      RETURN.
    ENDIF.

    IF iw_ambientes-qas = abap_true .

      CLEAR c_request-hqas.

      me->set_sid_data(
        EXPORTING
          i_ambiente = c_nome_ambientes-qas
          i_request  = c_request
        CHANGING
          c_systems  = lt_systems
          c_info     = c_rqas
          c_infot    = c_request-tqas
          c_hora     = c_request-hqas      ).

      IF c_rqas IS INITIAL.
        CLEAR c_request-tqas.
        CLEAR c_request-hqas.
      ELSE.
        c_request-dataex = c_request-tqas.
        c_request-horaex = c_request-hqas.
      ENDIF.

    ENDIF.

    IF iw_ambientes-prd = abap_true.

      CLEAR c_request-hprd.

      me->set_sid_data(
       EXPORTING
         i_ambiente = c_nome_ambientes-prd
         i_request  = c_request
       CHANGING
         c_systems  = lt_systems
         c_info     = c_rprd
         c_infot    = c_request-tprd
         c_hora     = c_request-hprd    ).

      IF c_rprd IS INITIAL.
        CLEAR c_request-tprd.
        CLEAR c_request-hprd.
      ELSE.
        c_request-dataex = c_request-tprd.
        c_request-horaex = c_request-hprd.
      ENDIF.

    ENDIF.

  ENDMETHOD. " zf_get_rel_info


  METHOD get_status_transp_ambientes.

    DATA: vl_tabix   LIKE sy-tabix,
          vl_dev(1),vl_qas(1),vl_prd(1),
          values_tab TYPE dd07v .

    me->get_rel_info(
      EXPORTING
       iw_ambientes = iw_ambientes
      CHANGING
        c_rdev      = vl_dev
        c_rqas      = vl_qas
        c_rprd      = vl_prd
        c_request   = c_request  ).

    IF iw_ambientes-dev = abap_true.
      c_request-dev = vl_dev.
    ENDIF.

    IF iw_ambientes-qas = abap_true.
      c_request-qas = vl_qas.
    ENDIF.


    IF iw_ambientes-prd = abap_true.
      c_request-prd = vl_prd.
    ENDIF.


  ENDMETHOD.


  METHOD set_sid_data.

    DATA:   lt_steps   TYPE ctslg_steps,
            lt_actions TYPE ctslg_actions.

    READ TABLE c_systems INTO DATA(lw_systems) WITH KEY systemid = i_ambiente.

    IF sy-subrc = 0.
      IF lw_systems-rc = 0 OR lw_systems-rc = 4.
        c_info = zif_ca_ctrlreq_request=>cc_status-sucesso.
      ELSE.
        c_info = zif_ca_ctrlreq_request=>cc_status-falha.
      ENDIF.

    ELSEIF i_request-trfunction EQ zif_ca_ctrlreq_request=>cc_tipo-transporte_copia.
      IF i_ambiente =  c_nome_ambientes-dev.
        c_info = zif_ca_ctrlreq_request=>cc_status-nao_transportado.
      ELSE.
        c_info = zif_ca_ctrlreq_request=>cc_status-inativo.
      ENDIF.
    ENDIF.

    lt_steps = lw_systems-steps.

    DELETE lt_steps WHERE stepid = 'E'.

    LOOP AT lt_steps INTO DATA(wa_steps) WHERE stepid = 'I'.

      IF wa_steps-rc > 4.
        EXIT.
      ENDIF.

      lt_actions = wa_steps-actions.


      LOOP AT lt_actions INTO DATA(wa_actions) WHERE rc = 0 OR
                                               rc = 4.

      ENDLOOP.

      c_infot = wa_actions-date.
      c_hora  = wa_actions-time.

    ENDLOOP.

    IF sy-subrc NE 0.
      CLEAR wa_steps.
      LOOP AT lt_steps INTO wa_steps WHERE stepid = 'e'.

        IF wa_steps-rc > 4.
          EXIT.
        ENDIF.

        lt_actions = wa_steps-actions.

        CLEAR wa_actions.
        LOOP AT lt_actions INTO wa_actions WHERE rc = 0 OR
                                                 rc = 4.

        ENDLOOP.

        c_infot = wa_actions-date.
        c_hora  = wa_actions-time.

      ENDLOOP.


    ENDIF.
    IF sy-subrc NE 0 AND NOT
    ( c_info EQ zif_ca_ctrlreq_request=>cc_status-falha OR
      c_info EQ zif_ca_ctrlreq_request=>cc_status-nao_transportado ) .

      CLEAR c_info.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
