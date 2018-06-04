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
class zcl_ca_ctrlreq_bapis_requests definition
  public
  final
  create private .

  public section.

    constants:
      begin of cs_destino,
        sistema type tr_target value 'EQ0', "CALL FUNCTION 'TR_GET_TRANSPORT_TARGET'
        mandt   type sy-mandt value '100',
      end of cs_destino.

    interfaces zif_ca_ctrlreq_bapis_requests.

    class-methods get_instance
      returning
        value(r_result) type ref to zif_ca_ctrlreq_bapis_requests.

    methods constructor.

  protected section.
  private section.
    CONSTANTS cs_rfc_dest_qa TYPE string VALUE 'EQ0CLNT100' ##NO_TEXT.

    class-data go_bapis_requests type ref to zif_ca_ctrlreq_bapis_requests.

    data gv_sist_destino_transp type tr_target.

endclass.



class zcl_ca_ctrlreq_bapis_requests implementation.


  method constructor.

    call function 'TR_GET_TRANSPORT_TARGET'
      exporting
        iv_use_default             = abap_true
      importing
        ev_target                  = me->gv_sist_destino_transp    " Transport Target
      exceptions
        wrong_call                 = 1
        invalid_input              = 2
        cts_initialization_failure = 3
        others                     = 4.

    if sy-subrc <> 0.
      me->gv_sist_destino_transp = cs_destino-sistema.
    endif.

  endmethod.


  method get_instance.

    if go_bapis_requests is not bound.
      go_bapis_requests  = new zcl_ca_ctrlreq_bapis_requests( ).
    endif.

    r_result = go_bapis_requests.

  endmethod.


  method zif_ca_ctrlreq_bapis_requests~atualiza_descricao_request.

    data lw_e07t  type e07t.

    if i_request->get_request( ) is initial.
      return.
    endif.

    if i_request->is_liberada( ) eq abap_false.

      lw_e07t-trkorr  = i_request->get_request( ).
      lw_e07t-as4text = i_request->get_descricao_padrao_sotreq( ).
      lw_e07t-langu   = sy-langu.

      data(lw_e070) = zcl_ca_ctrlreq_banco_dados=>get_instance( )->get_e070( lw_e07t-trkorr ).

      call function 'TR_MODIFY_COMM'
        exporting
          wi_e070                        = lw_e070
          wi_e07t                        = lw_e07t
          wi_sel_e07t                    = abap_true    " Flag, whether replace to be made on E07T
        exceptions
          db_access_error                = 1
          chosen_project_closed          = 2
          locked_entries                 = 3
          locked_object_not_deleted      = 4
          ordername_forbidden            = 5
          ordernumber_empty              = 6
          order_change_but_locked_object = 7
          order_released                 = 8
          order_user_locked              = 9
          tr_check_keysyntax_error       = 10
          tr_enqueue_failed              = 11
          no_authorization               = 12
          wrong_client                   = 13
          unallowed_source_client        = 14
          unallowed_user                 = 15
          unallowed_trfunction           = 16
          unallowed_trstatus             = 17
          no_systemname                  = 18
          no_systemtype                  = 19
          others                         = 20.

      if sy-subrc <> 0.

        message id     sy-msgid
                type   sy-msgty
                number sy-msgno
                with   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                into data(ls_texto_mensagem).

        raise exception type zcx_ca_ctrlreq_excecoes
          exporting
            textid                       = zcx_ca_ctrlreq_excecoes=>erro_salvar_request
            mensagem_erro_salvar_request = ls_texto_mensagem.

      endif.

    endif.

  endmethod.


  method zif_ca_ctrlreq_bapis_requests~copia_objetos.

    call function 'TR_COPY_COMM'
      exporting
        wi_dialog                = abap_false
        wi_trkorr_from           = i_request_origem
        wi_trkorr_to             = i_request_destino
        wi_without_documentation = abap_true
      exceptions
        others                   = 1.

    if sy-subrc <> 0.

      message id     sy-msgid
              type   sy-msgty
              number sy-msgno
              with   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into data(ls_texto_mensagem).

      raise exception type zcx_ca_ctrlreq_excecoes
        exporting
          textid                       = zcx_ca_ctrlreq_excecoes=>erro_salvar_request
          mensagem_erro_salvar_request = ls_texto_mensagem.

    endif.

  endmethod.


  method zif_ca_ctrlreq_bapis_requests~criar_request.

    data lv_request type trkorr.

*    IF me->gv_sist_destino_transp IS INITIAL.
*      me->gv_sist_destino_transp = c_sist_destino_transp.
*    ENDIF.

    call function 'TRINT_INSERT_NEW_COMM'
      exporting
        wi_kurztext       = c_request->get_descricao_padrao_sotreq( )    " Short text for task/request
        wi_trfunction     = c_request->get_tipo_request( )    " Request type ('K', 'T', 'C',...)
        iv_tarsystem      = me->gv_sist_destino_transp  " Target system for request
      importing
        we_trkorr         = lv_request    " Task/request
      exceptions
        no_systemname     = 1
        no_systemtype     = 2
        no_authorization  = 3
        db_access_error   = 4
        file_access_error = 5
        enqueue_error     = 6
        number_range_full = 7
        invalid_input     = 8
        others            = 9.

    if sy-subrc <> 0.

      message id     sy-msgid
              type   sy-msgty
              number sy-msgno
              with   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              into data(ls_texto_mensagem).

      raise exception type zcx_ca_ctrlreq_excecoes
        exporting
          textid                       = zcx_ca_ctrlreq_excecoes=>erro_salvar_request
          mensagem_erro_salvar_request = ls_texto_mensagem.

    endif.

    c_request->set_request( lv_request  ).

    if  c_request->get_tipo_request( ) = zif_ca_ctrlreq_request=>cc_tipo-transporte_copia.

      data(lt_tasks) = c_request->get_tasks_request_origem( ).

      insert value #( trkorr = c_request->get_request_origem( ) ) into table lt_tasks.

      loop at lt_tasks assigning field-symbol(<lw_task>).

        me->zif_ca_ctrlreq_bapis_requests~copia_objetos( i_request_origem  = <lw_task>-trkorr
                                                         i_request_destino = lv_request       ).

      endloop.

      c_request->set_data_exportacao( sy-datum ).
      c_request->set_hora_exportacao( sy-uzeit ).

    endif.

  endmethod.


  method zif_ca_ctrlreq_bapis_requests~liberar_request.

    data et_messages type ctsgerrmsgs.

    if i_request->get_request( ) is initial .

      raise exception type zcx_ca_ctrlreq_excecoes
        exporting
          textid = zcx_ca_ctrlreq_excecoes=>request_invalida.

    endif.

    data(lt_request_e_tasks) = i_request->get_tasks( ).

    append value #( trkorr = i_request->get_request( ) ) to lt_request_e_tasks.

    loop at lt_request_e_tasks assigning field-symbol(<lw_request_task>).

      call function 'TRINT_RELEASE_REQUEST'
        exporting
          iv_trkorr                   = <lw_request_task>
          iv_dialog                   = abap_off
          iv_without_objects_check    = abap_true
          iv_without_locking          = abap_true
        importing
          et_messages                 = et_messages
        exceptions
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
          others                      = 18.

      if sy-subrc <> 0.

        " Ignora o erro se for task já liberada
        if sy-subrc = 5 and <lw_request_task> <> i_request->get_request( ).
          continue.
        endif.

        message id sy-msgid type sy-msgty
                            number sy-msgno
                            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                            into data(ls_texto_mensagem).

        concatenate <lw_request_task> ':' ls_texto_mensagem into ls_texto_mensagem separated by space.

        raise exception type zcx_ca_ctrlreq_excecoes
          exporting
            textid                        = zcx_ca_ctrlreq_excecoes=>erro_liberar_request
            mensagem_erro_liberar_request = ls_texto_mensagem
            t_mensagens_erro              = et_messages.

      endif.

    endloop.

  endmethod.
  method zif_ca_ctrlreq_bapis_requests~importar_request.

    data:
      lt_requests type standard table of cts_req with default key.

    if i_request is not bound.
      return.
    endif.

    lt_requests = value #( ( request = i_request->get_request( ) ) ).

    call function 'CTS_API_IMPORT_CHANGE_REQUEST' starting new task 'REQ' destination cs_rfc_dest_qa
      exporting
        system        = me->gv_sist_destino_transp
        client        = cs_destino-mandt    " Target Client
      tables
        requests      = lt_requests
      exceptions
        error_message = 1
        others        = 2.

  endmethod.

endclass.
