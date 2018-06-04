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

 class zcl_ca_ctrlreq_request definition
  public
  create public.

   public section.

     interfaces zif_ca_ctrlreq_request .

     aliases:
      cc_tipo            for zif_ca_ctrlreq_request~cc_tipo,
      cc_tipo_transporte for zif_ca_ctrlreq_request~cc_tipo_transporte.

     methods constructor
       importing
         !i_request               type ztca_requests-trkorr optional
         !i_demanda               type ztca_requests-demanda
         !i_tipo_demanda          type ztca_requests-tipo_demanda
         !i_process_area          type ztca_requests-process_area
         !i_modulo                type ztca_requests-modulo
         !i_consultor             type ztca_requests-consultor
         !i_funcional             type ztca_requests-funcional
         !i_data                  type ztca_requests-data
         !i_descricao_sem_prefixo type ztca_requests-as4text
         !i_tipo_request          type ztca_requests-trfunction
         !i_request_origem        type ztca_requests-request_origem .
   protected section.
   private section.
     types: begin of ty_cod_request,
              trkorr type e070-trkorr,
            end of ty_cod_request,
            tty_cod_requests type sorted table of ty_cod_request with unique key trkorr.

     constants:
       c_radix(3) value 'Rdx' type c .
     constants:
       c_cr(2)    value 'CR' type c .
     data gv_request type ztca_requests-trkorr .
     data gv_modulo type ztca_requests-modulo .
     data gv_consultor type ztca_requests-consultor .
     data gv_funcional type ztca_requests-funcional .
     data gv_data type ztca_requests-data .
     data gv_descricao_sem_prefixo type ztca_requests-as4text .
     data gv_tipo_request type ztca_requests-trfunction .
     data gv_request_origem type ztca_requests-request_origem .
     data gv_demanda type ztca_requests-demanda .
     data gv_tipo_demanda type ztca_requests-tipo_demanda .
     data gv_process_area type ztca_requests-process_area .
     data gv_hora_exportacao type ztca_requests-horaex .
     data gv_data_exportacao type ztca_requests-dataex .
     methods libera_request_task
       importing
         i_request_ou_task type e070-trkorr
       raising
         zcx_ca_ctrlreq_excecoes.
 endclass.



 class zcl_ca_ctrlreq_request implementation.


   method constructor.

     me->gv_request = i_request.
     me->gv_demanda = i_demanda.
     me->gv_tipo_demanda = i_tipo_demanda.
     me->gv_process_area = i_process_area.
     me->gv_modulo = i_modulo.
     me->gv_consultor = i_consultor.
     me->gv_funcional = i_funcional.
     me->gv_data = i_data.
     me->gv_descricao_sem_prefixo = i_descricao_sem_prefixo.
     me->gv_tipo_request = i_tipo_request.
     me->gv_request_origem = i_request_origem.

   endmethod.


   method libera_request_task.

     data et_messages type ctsgerrmsgs.

     call function 'TRINT_RELEASE_REQUEST'
       exporting
         iv_trkorr                   = i_request_ou_task
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

       if sy-subrc = 5.

         raise exception type zcx_ca_ctrlreq_excecoes
           exporting
             textid       = zcx_ca_ctrlreq_excecoes=>request_ja_liberada
             request_task = i_request_ou_task.

       else.

         message id sy-msgid type sy-msgty
                             number sy-msgno
                             with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                             into data(ls_texto_mensagem).

         raise exception type zcx_ca_ctrlreq_excecoes
           exporting
             textid                        = zcx_ca_ctrlreq_excecoes=>erro_liberar_request
             mensagem_erro_liberar_request = ls_texto_mensagem
             t_mensagens_erro              = et_messages.


       endif.

     endif.

   endmethod.


   method zif_ca_ctrlreq_request~check_valido_copiar.

     data lt_qtidade type i.

     me->zif_ca_ctrlreq_request~is_valida_salvar( ).

     if not ( me->gv_tipo_request = cc_tipo-workbench or
              me->gv_tipo_request = cc_tipo-customizing ).

       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>erro_criar_copia.

     endif.


*     DATA(l_request_origem) = me->zif_ca_ctrlreq_request~get_request_origem( ).
*
*     IF l_request_origem IS NOT INITIAL.
*
*       RAISE EXCEPTION TYPE zcx_ca_ctrlreq_excecoes
*         EXPORTING
*           textid = zcx_ca_ctrlreq_excecoes=>campo_invalido
*           msgv1  = text-007.
*
*     ENDIF.

     data(lo_banco_dados) = zcl_ca_ctrlreq_banco_dados=>get_instance( ).


     "Seleciona as Tasks da Request original
     data(lt_trkorr) = lo_banco_dados->get_task_request( me->gv_request ).

     append me->gv_request to lt_trkorr.

     "Verifica se a request tem objetos
     select count(*)
     from e071
     into lt_qtidade
     for all entries in lt_trkorr
     where trkorr eq lt_trkorr-table_line.

     if lt_qtidade = 0.

       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>request_sem_objetos.

     endif.

   endmethod.


   method zif_ca_ctrlreq_request~check_valido_criar_referencia.

     me->zif_ca_ctrlreq_request~is_valida_salvar( ).

     if not ( me->gv_tipo_request = cc_tipo-workbench or
              me->gv_tipo_request = cc_tipo-customizing ).

       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>erro_criar_copia.

     endif.

   endmethod.


   method zif_ca_ctrlreq_request~cria_copia.

     me->zif_ca_ctrlreq_request~check_valido_copiar( ).

     r_request = cast zif_ca_ctrlreq_request(  new zcl_ca_ctrlreq_request(
                                               i_demanda               = me->zif_ca_ctrlreq_request~get_demanda( )
                                               i_tipo_demanda          = me->zif_ca_ctrlreq_request~get_tipo_demanda( )
                                               i_modulo                = me->zif_ca_ctrlreq_request~get_modulo( )
                                               i_consultor             = me->zif_ca_ctrlreq_request~get_consultor( )
                                               i_funcional             = me->zif_ca_ctrlreq_request~get_funcional( )
                                               i_data                  = me->zif_ca_ctrlreq_request~get_data( )
                                               i_descricao_sem_prefixo = me->zif_ca_ctrlreq_request~get_descricao( )
                                               i_tipo_request          = cc_tipo-transporte_copia
                                               i_request_origem        = me->zif_ca_ctrlreq_request~get_request( )
                                               i_process_area          = me->zif_ca_ctrlreq_request~get_process_area( ) ) ).

   endmethod.


   method zif_ca_ctrlreq_request~cria_referencia.

     me->zif_ca_ctrlreq_request~check_valido_criar_referencia( ).

     r_request = cast zif_ca_ctrlreq_request(  new zcl_ca_ctrlreq_request(
                                                i_demanda               = me->zif_ca_ctrlreq_request~get_demanda( )
                                                i_tipo_demanda          = me->zif_ca_ctrlreq_request~get_tipo_demanda( )
                                                i_modulo                = me->zif_ca_ctrlreq_request~get_modulo( )
                                                i_consultor             = me->zif_ca_ctrlreq_request~get_consultor( )
                                                i_funcional             = me->zif_ca_ctrlreq_request~get_funcional( )
                                                i_data                  = me->zif_ca_ctrlreq_request~get_data( )
                                                i_descricao_sem_prefixo = me->zif_ca_ctrlreq_request~get_descricao( )
                                                i_tipo_request          = me->zif_ca_ctrlreq_request~get_tipo_request( )
                                                i_request_origem        = me->zif_ca_ctrlreq_request~get_request( )
                                                i_process_area          = me->zif_ca_ctrlreq_request~get_process_area( ) ) ).


   endmethod.


   method zif_ca_ctrlreq_request~get_consultor.
     r_consultor = gv_consultor .
   endmethod.


   method zif_ca_ctrlreq_request~get_data.
     r_data = gv_data.
   endmethod.


   method zif_ca_ctrlreq_request~get_data_exportacao.
     r_data_exportacao = gv_data_exportacao .
   endmethod.


   method zif_ca_ctrlreq_request~get_demanda.
     r_demanda = gv_demanda.
   endmethod.


   method zif_ca_ctrlreq_request~get_descricao.
     r_descricao = gv_descricao_sem_prefixo.
   endmethod.


   method zif_ca_ctrlreq_request~get_descricao_padrao_sotreq.

     constants cc_separador type c length 1 value '_' ##NO_TEXT.

     case gv_tipo_request.

       when cc_tipo-workbench.

         data(lv_tipo_request) = cc_tipo_transporte-workbench.

       when cc_tipo-customizing.

         lv_tipo_request = cc_tipo_transporte-customizing.

       when cc_tipo-transporte_copia.

         lv_tipo_request = cc_tipo_transporte-transporte_copia.

     endcase.

     concatenate lv_tipo_request gv_modulo gv_process_area into r_info_request separated by cc_separador.

     concatenate gv_tipo_demanda gv_demanda into data(vl_demanda).

     concatenate r_info_request vl_demanda gv_descricao_sem_prefixo into r_info_request  separated by cc_separador.

   endmethod.


   method zif_ca_ctrlreq_request~get_funcional.
     r_funcional = gv_funcional.
   endmethod.


   method zif_ca_ctrlreq_request~get_hora_exportacao.
     r_hora_exportacao = gv_hora_exportacao  .
   endmethod.


   method zif_ca_ctrlreq_request~get_modulo.
     r_modulo = gv_modulo.
   endmethod.


   method zif_ca_ctrlreq_request~get_process_area.
     r_process_area = gv_process_area.
   endmethod.


   method zif_ca_ctrlreq_request~get_request.
     r_request = gv_request.
   endmethod.


   method zif_ca_ctrlreq_request~get_request_origem.
     r_request_origem = gv_request_origem.
   endmethod.


   method zif_ca_ctrlreq_request~get_tasks.

     if me->gv_request is initial.

       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>request_invalida.

     endif.

     "Seleciona as Tasks da Request original
     rt_cod_request = zcl_ca_ctrlreq_banco_dados=>get_instance( )->get_task_request( me->gv_request ).

   endmethod.


   method zif_ca_ctrlreq_request~get_tasks_request_origem.

     if me->zif_ca_ctrlreq_request~sou_uma_copia_valida( ) = abap_false.

       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>copia_invalida.

     endif.

     "Seleciona as Tasks da Request original
     rt_cod_request = zcl_ca_ctrlreq_banco_dados=>get_instance( )->get_task_request( me->gv_request_origem ).

   endmethod.


   method zif_ca_ctrlreq_request~get_tipo_demanda.
     r_tipo_demanda = gv_tipo_demanda.
   endmethod.


   method zif_ca_ctrlreq_request~get_tipo_request.
     r_tipo_request = gv_tipo_request.
   endmethod.


   method zif_ca_ctrlreq_request~is_liberada.

     constants: c_liberada   type e070-trstatus value 'R',
                c_liberada_n type e070-trstatus value 'N'.

     try.
         if zcl_ca_ctrlreq_banco_dados=>get_instance( )->get_status_request( me->gv_request ) = c_liberada or
            zcl_ca_ctrlreq_banco_dados=>get_instance( )->get_status_request( me->gv_request ) = c_liberada_n.
           r_resultado = abap_true.
         endif.

       catch zcx_ca_ctrlreq_excecoes.
     endtry.

   endmethod.


   method zif_ca_ctrlreq_request~is_valida_salvar.

     if gv_tipo_request is initial.
       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>campo_invalido
           msgv1  = text-001.

     endif.

     if gv_modulo is initial.
       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>campo_invalido
           msgv1  = text-002.
     endif.

     if gv_tipo_demanda is initial.
       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>campo_invalido
           msgv1  = text-003.
     endif.

     if gv_demanda is initial.
       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>campo_invalido
           msgv1  = text-004.
     endif.

     if gv_descricao_sem_prefixo is initial.
       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>campo_invalido
           msgv1  = text-005.
     endif.

     if gv_process_area is initial.
       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>campo_invalido
           msgv1  = text-006.
     endif.

   endmethod.


   method zif_ca_ctrlreq_request~is_workbench.
     if gv_tipo_request = cc_tipo-workbench.
       r_workbench = abap_true.
     else.
       r_workbench = abap_false.
     endif.
   endmethod.


   method zif_ca_ctrlreq_request~release_request.

     data et_messages type ctsgerrmsgs.

     if me->gv_request is initial.

       raise exception type zcx_ca_ctrlreq_excecoes
         exporting
           textid = zcx_ca_ctrlreq_excecoes=>request_invalida.

     endif.

     me->zif_ca_ctrlreq_request~is_valida_salvar( ).

     zcl_ca_ctrlreq_bapis_requests=>get_instance( )->liberar_request( me ).


   endmethod.


   method zif_ca_ctrlreq_request~set_data_exportacao.
     gv_data_exportacao = i_data_exportacao.
   endmethod.


   method zif_ca_ctrlreq_request~set_descricao.
     gv_descricao_sem_prefixo = i_descricao.
   endmethod.


   method zif_ca_ctrlreq_request~set_hora_exportacao.
     gv_hora_exportacao = i_hora_exportacao.
   endmethod.


   method zif_ca_ctrlreq_request~set_process_area.
     gv_process_area = i_process_area.
   endmethod.


   method zif_ca_ctrlreq_request~set_request.
     gv_request = i_request.
   endmethod.


   method zif_ca_ctrlreq_request~set_request_origem.
     gv_request_origem = i_request_origem.
   endmethod.


   method zif_ca_ctrlreq_request~set_tipo_request.
     gv_tipo_request = i_tipo_request.
   endmethod.


   method zif_ca_ctrlreq_request~sou_uma_copia_valida.
     r_resultado = abap_true.
     try.
         me->zif_ca_ctrlreq_request~is_valida_salvar(  ).
       catch zcx_ca_ctrlreq_excecoes.
         r_resultado = abap_false.
         return.
     endtry.

     if me->gv_tipo_request <> cc_tipo-transporte_copia.
       r_resultado = abap_false.
       return.
     endif.

     if me->zif_ca_ctrlreq_request~get_request_origem( ) is initial.
       r_resultado = abap_false.
       return.
     endif.


   endmethod.
   method zif_ca_ctrlreq_request~importar_imediat_qa.

     zcl_ca_ctrlreq_bapis_requests=>get_instance( )->importar_request( me ).

   endmethod.

 endclass.
