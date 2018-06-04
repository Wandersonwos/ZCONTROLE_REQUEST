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

CLASS zcl_ca_ctrlreq_request_reposit DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ca_ctrlreq_request_reposit .

    CLASS-METHODS
      get_instance
        RETURNING
          VALUE(r_return) TYPE REF TO zif_ca_ctrlreq_request_reposit .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA
      go_instance TYPE REF TO zcl_ca_ctrlreq_request_reposit .

    DATA:
      gt_bdcdata      TYPE bdcdata_tab ,
      gt_agrupa_msg   TYPE TABLE OF bdcmsgcoll ,
      gv_hora_inicio  TYPE sy-uzeit ,
      gv_hora_fim     TYPE sy-uzeit ,
      go_shdb         TYPE REF TO zif_ca_ctrlreq_shdb,
      go_bapi_request TYPE REF TO zif_ca_ctrlreq_bapis_requests,
      go_banco_dados  TYPE REF TO zif_ca_ctrlreq_banco_dados .

    METHODS limpa_tabela
      CHANGING
        !ct_request TYPE zif_ca_ctrlreq_banco_dados=>ty_ztca_requests_tab .

    METHODS salva_request_banco_dados
      IMPORTING
        io_request TYPE REF TO zif_ca_ctrlreq_request
      RAISING
        zcx_ca_ctrlreq_excecoes .

    METHODS atualiza_request
      IMPORTING
        io_request TYPE REF TO zif_ca_ctrlreq_request
      RAISING
        zcx_ca_ctrlreq_excecoes .

    METHODS salva_request
      IMPORTING
        !i_shdb         TYPE REF TO zif_ca_ctrlreq_shdb OPTIONAL
        !i_bapi_request TYPE REF TO zif_ca_ctrlreq_bapis_requests OPTIONAL
      CHANGING
        co_request      TYPE REF TO zif_ca_ctrlreq_request
      RAISING
        zcx_ca_ctrlreq_excecoes .

    METHODS monta_request_importacao
      IMPORTING
        iv_descricao  TYPE e07t-as4text
        iv_trfunction TYPE e070-trfunction
        iv_request    TYPE e070-trkorr
      CHANGING
        co_request    TYPE REF TO zif_ca_ctrlreq_request
      RAISING
        zcx_ca_ctrlreq_excecoes .


ENDCLASS.



CLASS ZCL_CA_CTRLREQ_REQUEST_REPOSIT IMPLEMENTATION.


  METHOD atualiza_request.

    zcl_ca_ctrlreq_bapis_requests=>get_instance( )->atualiza_descricao_request( io_request ).

    me->salva_request_banco_dados( io_request ).

  ENDMETHOD.


  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      go_instance  = NEW #( ).
    ENDIF.

    r_return = go_instance.

  ENDMETHOD.


  METHOD limpa_tabela.

    DATA: lt_e070        TYPE zif_ca_ctrlreq_request=>tty_cod_requests,
          lo_banco_dados TYPE REF TO zif_ca_ctrlreq_banco_dados,
          lv_commit_work TYPE abap_bool.

    lv_commit_work = abap_false.
    lo_banco_dados = zcl_ca_ctrlreq_banco_dados=>get_instance( ).

    SELECT trkorr
    INTO TABLE lt_e070
    FROM e070
    FOR ALL ENTRIES IN ct_request
    WHERE trkorr = ct_request-trkorr.
*    AND   trfunction IN ('K','W').


    LOOP AT ct_request INTO DATA(wa_request).
      READ TABLE lt_e070 INTO DATA(wa_e070) WITH KEY trkorr = wa_request-trkorr.
      IF sy-subrc NE 0.

        TRY.
            lo_banco_dados->deletar_request( wa_request-trkorr  ).
          CATCH zcx_ca_ctrlreq_excecoes INTO DATA(ex).
        ENDTRY.

        DELETE ct_request WHERE trkorr EQ wa_request-trkorr.
        lv_commit_work = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_commit_work EQ abap_true.
      lo_banco_dados->commit_work( ).
    ENDIF.

  ENDMETHOD.


  METHOD monta_request_importacao.

    DATA: lw_request    TYPE ztca_requests,
          lv_trfunction TYPE e070-trfunction,
          lv_demanda(9),
          lv_tipo(2).

    SPLIT iv_descricao AT '_' INTO lv_trfunction
                                   lw_request-modulo
                                   lw_request-process_area
                                   lv_demanda(9)
                                   lw_request-as4text.


    co_request = NEW  zcl_ca_ctrlreq_request(
        i_request               = iv_request
        i_demanda               = CONV #( lv_demanda+2(7) )
        i_tipo_demanda          = lv_demanda(2)
        i_process_area          = lw_request-process_area
        i_modulo                = lw_request-modulo
        i_consultor             = lw_request-consultor
        i_funcional             = lw_request-funcional
        i_data                  = sy-datum
        i_descricao_sem_prefixo = lw_request-as4text
        i_tipo_request          = iv_trfunction
        i_request_origem        = lw_request-request_origem
    ).



  ENDMETHOD.


  METHOD salva_request.

    DATA(lv_hora_inicio) = sy-uzeit.

    go_banco_dados = zcl_ca_ctrlreq_banco_dados=>get_instance( ).

*    IF sy-uname = zif_ca_ctrlreq_request=>cc_usuario_teste-wanderson or
*       sy-uname = zif_ca_ctrlreq_request=>cc_usuario_teste-rafael    .

    IF i_bapi_request IS NOT BOUND.
      go_bapi_request = zcl_ca_ctrlreq_bapis_requests=>get_instance( ).
    ELSE.
      go_bapi_request = i_bapi_request.
    ENDIF.

    go_bapi_request->criar_request( CHANGING c_request  = co_request ).

*
*    ELSE.
*
*      "Cria o SHDB
*      IF i_shdb IS NOT BOUND.
*        go_shdb = NEW zcl_ca_ctrlreq_shdb( ).
*      ELSE.
*        go_shdb = i_shdb.
*      ENDIF.
*
*      go_shdb->executa_shdb( CHANGING c_request = co_request ).
*
*    ENDIF.
*
*
*    DATA(lv_hora_fim) = sy-uzeit.
*
    IF co_request->get_tipo_request( ) = zif_ca_ctrlreq_request=>cc_tipo-transporte_copia.

      "Transporta a cópia
      co_request->release_request( ).

    ENDIF.

*      WHEN zif_ca_ctrlreq_request=>cc_tipo-customizing OR
*           zif_ca_ctrlreq_request=>cc_tipo-workbench.
*
*        IF sy-uname <> zif_ca_ctrlreq_request=>cc_usuario_teste-rafael.
*
*
*
*          "Pega o numero da request
*
*
*
*          DATA(lv_request) = go_banco_dados->get_request_hora(
*                                EXPORTING
*                                  i_hora_inicio = lv_hora_inicio
*                                  i_hora_fim    = lv_hora_fim    ).
*
*          IF lv_request IS NOT INITIAL.
*
*            co_request->set_request( lv_request ).
*
*          ELSE.
*
*            RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
*              EXPORTING
*                textid = zcx_ca_ctrlreq_excecoes=>erro_shdb_se10.
*
*          ENDIF.
*
*        ENDIF.
*
*    ENDCASE.

    TRY.
        me->salva_request_banco_dados( co_request  ).

      CLEANUP INTO DATA(lo_excecao).

        go_banco_dados->rollback( ).

    ENDTRY.

  ENDMETHOD.


  METHOD salva_request_banco_dados.
    go_banco_dados = zcl_ca_ctrlreq_banco_dados=>get_instance( ).

    DATA(wa_request) = me->zif_ca_ctrlreq_request_reposit~objeto_to_wa( io_request ).

    IF io_request->get_tipo_request( ) EQ zif_ca_ctrlreq_request=>cc_tipo-transporte_copia.
      go_banco_dados->deletar_request_copia( wa_request-request_origem  ).
    ENDIF.

    TRY.
        go_banco_dados->salvar_request( wa_request ).
        go_banco_dados->commit_work( ).

      CATCH zcx_ca_ctrlreq_excecoes INTO DATA(ex).
        go_banco_dados->rollback( ).
        RAISE EXCEPTION ex.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~get_request.

    IF i_request IS INITIAL.

      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid = zcx_ca_ctrlreq_excecoes=>request_invalida.

    ENDIF.

    DATA(lw_dados_request) = zcl_ca_ctrlreq_banco_dados=>get_instance( )->get_request( i_request = i_request  ).

    r_request = NEW zcl_ca_ctrlreq_request(
            !i_request               = lw_dados_request-trkorr
            !i_demanda               = lw_dados_request-demanda
            !i_tipo_demanda          = lw_dados_request-tipo_demanda
            !i_process_area          = lw_dados_request-process_area
            !i_modulo                = lw_dados_request-modulo
            !i_consultor             = lw_dados_request-consultor
            !i_funcional             = lw_dados_request-funcional
            !i_data                  = lw_dados_request-data
            !i_descricao_sem_prefixo = lw_dados_request-as4text
            !i_tipo_request          = lw_dados_request-trfunction
            !i_request_origem        = lw_dados_request-request_origem ).

  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~get_requests.


    IF iw_range IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid = zcx_ca_ctrlreq_excecoes=>nenhum_filtro_informado.
    ENDIF.

    DATA(lo_banco_dados) = zcl_ca_ctrlreq_banco_dados=>get_instance( ).

    DATA(lt_request) = lo_banco_dados->get_requests( iw_range ).

    me->limpa_tabela( CHANGING ct_request = lt_request ).

    IF lt_request IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_info_reques) = NEW zcl_ca_ctrlreq_info_request( ).

    LOOP AT lt_request INTO DATA(lw_request).

      INSERT INITIAL LINE INTO TABLE rt_report ASSIGNING FIELD-SYMBOL(<lf_report>).

      MOVE-CORRESPONDING lw_request TO <lf_report>.

      lo_info_reques->get_status_transp_ambientes(
        EXPORTING
          iw_ambientes = iw_ambientes
        CHANGING
          c_request    = <lf_report>  ).
* Apaga as cópias das request que foram para a PRD.
      IF <lf_report>-prd = abap_true.
        DELETE lt_request WHERE request_origem = <lf_report>-trkorr and trfunction = zcl_ca_ctrlreq_request=>cc_tipo-transporte_copia.
        DELETE rt_report  WHERE request_origem = <lf_report>-trkorr and trfunction = zcl_ca_ctrlreq_request=>cc_tipo-transporte_copia.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~importa_request.

    IF i_request IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid = zcx_ca_ctrlreq_excecoes=>request_invalida.
    ENDIF.

    DATA: lw_e07t               TYPE e07t,
          lv_trfunction         TYPE e070-trfunction,
          lv_trstatus           TYPE e070-trstatus,
          lv_as4user            TYPE e070-as4user,
          lv_message            TYPE syst_msgv,
          lv_request_cadastrada TYPE ztca_requests-trkorr.

    "Verifica se a request já está na tabela Z
    SELECT SINGLE trkorr
    FROM ztca_requests
    INTO lv_request_cadastrada
    WHERE trkorr = i_request.

    IF lv_request_cadastrada IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid = zcx_ca_ctrlreq_excecoes=>request_ja_existe.
    ENDIF.

    SELECT SINGLE * FROM e07t
    INTO lw_e07t
    WHERE trkorr EQ i_request AND
          langu  EQ sy-langu.

    IF lw_e07t IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid = zcx_ca_ctrlreq_excecoes=>request_invalida.
    ENDIF.

    SELECT SINGLE trfunction trstatus as4user
    FROM e070
    INTO ( lv_trfunction, lv_trstatus, lv_as4user )
    WHERE trkorr EQ i_request.

    IF NOT ( lv_trfunction EQ zif_ca_ctrlreq_request=>cc_tipo-workbench   ) AND
       NOT ( lv_trfunction EQ zif_ca_ctrlreq_request=>cc_tipo-customizing ) .

      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid = zcx_ca_ctrlreq_excecoes=>tipo_invalido.
    ENDIF.

    CALL FUNCTION 'YLC_UTL_CHECK_NAME'
      EXPORTING
        input      = lw_e07t-as4text   " Descrição breve dos objetos Repository
      IMPORTING
        ev_message = lv_message   " Mensagem de retorno
      EXCEPTIONS
        invalid    = 1
        OTHERS     = 2.

    IF sy-subrc EQ 0 ."AND lw_e07t-as4text(1) <> lv_trfunction.

      me->monta_request_importacao(
    EXPORTING
      iv_descricao              = lw_e07t-as4text
      iv_trfunction             = lv_trfunction
      iv_request                = i_request
    CHANGING
      co_request                = r_request
  ).
    ELSE.

      IF lv_trstatus NE zif_ca_ctrlreq_request=>cc_status_req-mod.

        RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
          EXPORTING
            textid       = zcx_ca_ctrlreq_excecoes=>request_ja_liberada
            request_task = i_request.

      ELSEIF lv_trstatus NE zif_ca_ctrlreq_request=>cc_status_req-mod.

        RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
          EXPORTING
            textid       = zcx_ca_ctrlreq_excecoes=>request_ja_liberada
            request_task = i_request.
      ELSEIF sy-uname NE lv_as4user.

        RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
          EXPORTING
            textid  = zcx_ca_ctrlreq_excecoes=>user_invalido
            usuario = lv_as4user.
      ELSE.

        RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
          EXPORTING
            textid = zcx_ca_ctrlreq_excecoes=>descricao_invalida
            msgv1  = lv_message.
      ENDIF.
    ENDIF.




  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~objeto_to_wa.
    rw_request-trkorr           = io_request->get_request( ).
    rw_request-demanda          = io_request->get_demanda( ).
    rw_request-tipo_demanda     = io_request->get_tipo_demanda( ).
    rw_request-process_area     = io_request->get_process_area( ).
    rw_request-consultor        = io_request->get_consultor( ).
    rw_request-as4text          = io_request->get_descricao( ).
    rw_request-funcional        = io_request->get_funcional( ).
    rw_request-data             = sy-datum.
    rw_request-hora             = sy-uzeit.
    rw_request-modulo           = io_request->get_modulo( ).
    rw_request-trfunction       = io_request->get_tipo_request( ).
    rw_request-request_origem   = io_request->get_request_origem( ).
    rw_request-horaex           = io_request->get_hora_exportacao( ).
    rw_request-dataex           = io_request->get_data_exportacao( ).
  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~salvar.

    c_request->is_valida_salvar( ).

    " Atualiza a request
    IF c_request->get_request( ) IS NOT INITIAL.

      me->atualiza_request( c_request ).

    ELSE.

      me->salva_request(
        EXPORTING
          i_shdb = i_shdb
          i_bapi_request = i_bapi_request
        CHANGING
          co_request = c_request ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
