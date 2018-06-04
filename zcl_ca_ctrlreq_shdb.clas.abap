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

CLASS zcl_ca_ctrlreq_shdb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_ca_ctrlreq_shdb .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_agrupa_msg TYPE STANDARD TABLE OF bdcmsgcoll WITH DEFAULT KEY .

    CONSTANTS: c_sist_destino_transp TYPE string VALUE 'EQ0'. "CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'

    METHODS mapeia_shdb_request_original
      IMPORTING
        !i_request         TYPE REF TO zif_ca_ctrlreq_request
      CHANGING
        !c_call_transation TYPE REF TO zif_ca_call_transaction_shdb .

    METHODS mapeia_shdb_request_copia
      IMPORTING
        !i_request         TYPE REF TO zif_ca_ctrlreq_request
      CHANGING
        !c_call_transation TYPE REF TO zif_ca_call_transaction_shdb
      RAISING
        zcx_ca_ctrlreq_excecoes .

ENDCLASS.



CLASS ZCL_CA_CTRLREQ_SHDB IMPLEMENTATION.


  METHOD mapeia_shdb_request_copia.

    DATA(l_request_origem) = i_request->get_request_origem( ).

    IF i_request->sou_uma_copia_valida( ) = abap_false.

      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid = zcx_ca_ctrlreq_excecoes=>copia_invalida.
    ENDIF.

    DATA(lt_tasks) = i_request->get_tasks_request_origem( ).

    INSERT VALUE #( trkorr = l_request_origem ) INTO TABLE lt_tasks.

    c_call_transation->adiciona_passo(
      EXPORTING
        iv_okcode  = '=TSCM'
        it_dynpros = VALUE #( ( cprog = 'RDDM0001' dynnr = '0200' ) ) ).

    c_call_transation->adiciona_passo(
      EXPORTING
        iv_okcode  = '=CREA'
        it_dynpros = VALUE #( ( cprog = 'RDDM0001' dynnr = '0200' ) ) ).

    c_call_transation->adiciona_passo(
       EXPORTING
         it_values  = VALUE #( ( fnam = 'KO042-REQ_COPY_T'   fval = abap_true ) )
         iv_okcode  = '=TAKE'
         it_dynpros = VALUE #( ( cprog = 'SAPLSTRH' dynnr = '1411' ) ) ).

    c_call_transation->adiciona_passo(
      EXPORTING
        it_values  = VALUE #( ( fnam = 'KO013-TARSYSTEM'   fval = c_sist_destino_transp )
                              ( fnam = 'KO013-AS4TEXT'     fval = i_request->get_descricao_padrao_sotreq( ) ) )
        iv_okcode  = '=CREA'
        it_dynpros = VALUE #( ( cprog = 'SAPLSTR8' dynnr = '0100' ) ) ).

    LOOP AT lt_tasks INTO DATA(w_trkorr).

      c_call_transation->adiciona_passo(
         EXPORTING
           iv_okcode  = '=INOB'
           it_dynpros = VALUE #( ( cprog = 'SAPMSSY0' dynnr = '0120' ) ) ).


      c_call_transation->adiciona_passo(
        EXPORTING
          it_values  = VALUE #( ( fnam = 'DV_0100_COPY_ONE_REQUEST'  fval = abap_true )
                                ( fnam = 'DV_0100_SOURCE_REQUEST'    fval = w_trkorr-trkorr ) )
          iv_okcode  = '=TAKE'
          it_dynpros = VALUE #( ( cprog = 'SAPLSTRH' dynnr = '0100' ) ) ).

    ENDLOOP.

    c_call_transation->adiciona_passo(
      EXPORTING
        iv_okcode  = '=CANC'
        it_dynpros = VALUE #( ( cprog = 'SAPMSSY0' dynnr = '0120' ) ) ).

    c_call_transation->adiciona_passo(
      EXPORTING
        iv_okcode  = '/EEXIT'
        it_dynpros = VALUE #( ( cprog = 'RDDM0001' dynnr = '0200' ) ) ).

  ENDMETHOD.


  METHOD mapeia_shdb_request_original.

    c_call_transation->adiciona_passo(
         EXPORTING
           iv_okcode  = '=CREA'
           it_dynpros = VALUE #( ( cprog = 'RDDM0001' dynnr = '0100' ) ) ).

    c_call_transation->adiciona_passo(
      EXPORTING
        it_values  = VALUE #( ( fnam = SWITCH #( i_request->is_workbench( )
                                                 WHEN abap_true
                                                 THEN 'KO042-REQ_CONS_K'      " Workbench
                                                 ELSE 'KO042-REQ_CUST_W'    ) " Customizing
                                fval = abap_true ) )
        iv_okcode  = '=TAKE'
        it_dynpros = VALUE #( ( cprog = 'SAPLSTRH' dynnr = '1411' ) ) ).

    c_call_transation->adiciona_passo(
      EXPORTING
        it_values  = VALUE #( ( fnam = 'KO013-AS4TEXT'   fval = i_request->get_descricao_padrao_sotreq( ) )
                              ( fnam = 'KO013-TARSYSTEM' fval = c_sist_destino_transp ) )
        iv_okcode  = '=CREA'
        it_dynpros = VALUE #( ( cprog = 'SAPLSTR8' dynnr = '0102' ) ) ).

    c_call_transation->adiciona_passo(
      EXPORTING
        iv_okcode  = '=TREX'
        it_dynpros = VALUE #( ( cprog = 'SAPMSSY0' dynnr = '0120' ) ) ).

    c_call_transation->adiciona_passo(
      EXPORTING
        iv_okcode  = '/ECANC'
        it_dynpros = VALUE #( ( cprog = 'RDDM0001' dynnr = '0100' ) ) ).

  ENDMETHOD.


  METHOD zif_ca_ctrlreq_shdb~executa_shdb.

      CONSTANTS:
        BEGIN OF c_codigo_transacao,
          work_ou_customiz TYPE sytcode VALUE 'SE10',
          transp_copia     TYPE sytcode VALUE 'SE01',
        END OF c_codigo_transacao.

      DATA: lt_bdcdata     TYPE bdcdata_tab.

      CASE c_request->get_tipo_request( ).

        WHEN zif_ca_ctrlreq_request=>cc_tipo-workbench OR
             zif_ca_ctrlreq_request=>cc_tipo-customizing.

          DATA(lo_call_transation) = zcl_ca_call_transaction_shdb=>factory( c_codigo_transacao-work_ou_customiz ).

          me->mapeia_shdb_request_original(
            EXPORTING
              i_request         = c_request
            CHANGING
              c_call_transation = lo_call_transation ).

          lo_call_transation->executar(
            IMPORTING
              ex_sy_subrc  = DATA(l_codigo_erro)
              ex_mensagens = DATA(lt_mensagens)  ).

          "Erro no SHDB
          IF l_codigo_erro <> 0.

            RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
              EXPORTING
                textid         = zcx_ca_ctrlreq_excecoes=>erro_shdb_se10
                mensagens_shdb = lt_mensagens.

          ENDIF.

        WHEN zif_ca_ctrlreq_request=>cc_tipo-transporte_copia.

          IF c_request->get_request_origem( ) IS INITIAL .

            RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
              EXPORTING
                textid = zcx_ca_ctrlreq_excecoes=>campo_invalido
                msgv1  = text-001.

          ENDIF.

          lo_call_transation = zcl_ca_call_transaction_shdb=>factory( c_codigo_transacao-transp_copia ).

          me->mapeia_shdb_request_copia(
            EXPORTING
              i_request         = c_request
            CHANGING
              c_call_transation = lo_call_transation ).

          DATA lv_modo_processamento TYPE ctu_mode VALUE 'N'.

          lo_call_transation->executar(
            EXPORTING
              im_opcoes_avancadas = VALUE #( dismode = lv_modo_processamento
                                             defsize = abap_true             )
            IMPORTING
              ex_sy_subrc         = l_codigo_erro
              ex_mensagens        = lt_mensagens   ).

          READ TABLE lt_mensagens INTO DATA(wa_mensagens) WITH KEY msgid = 'TK' msgnr = '257'.

          IF sy-subrc <> 0.

            RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
              EXPORTING
                textid         = zcx_ca_ctrlreq_excecoes=>erro_shdb_se10
                mensagens_shdb = lt_mensagens.

          ELSE.

            c_request->set_request( CONV #( wa_mensagens-msgv2 ) ).
            c_request->set_data_exportacao( sy-datum ).
            c_request->set_hora_exportacao( sy-uzeit ).
          ENDIF.

        WHEN OTHERS.

          RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
            EXPORTING
              textid = zcx_ca_ctrlreq_excecoes=>erro_shdb_se10.

      ENDCASE.

  ENDMETHOD.


  METHOD zif_ca_ctrlreq_shdb~release_request.

    DATA et_messages TYPE ctsgerrmsgs.

    IF i_request->get_request( ) IS INITIAL .
      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid = zcx_ca_ctrlreq_excecoes=>request_invalida.
    ENDIF.

    CALL FUNCTION 'TRINT_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                   = i_request->get_request( )
        iv_dialog                   = abap_off
        iv_without_objects_check    = abap_true
        iv_without_locking          = abap_true
      IMPORTING
        et_messages                 = et_messages
      EXCEPTIONS
        cts_initialization_failure  = 1
        enqueue_failed              = 2
        no_authorization            = 3
        invalid_request             = 4
        request_already_released    = 5
        repeat_too_early            = 6
        object_lock_error           = 7
        object_check_error          = 8
        docu_missing                = 9
        db_access_error             = 10
        action_aborted_by_user      = 11
        export_failed               = 12
        execute_objects_check       = 13
        release_in_bg_mode          = 14
        release_in_bg_mode_w_objchk = 15
        error_in_export_methods     = 16
        object_lang_error           = 17
        OTHERS                      = 18.

    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty
                          NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                          INTO DATA(ls_texto_mensagem).

      RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
        EXPORTING
          textid                        = zcx_ca_ctrlreq_excecoes=>erro_liberar_request
          mensagem_erro_liberar_request = ls_texto_mensagem.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
