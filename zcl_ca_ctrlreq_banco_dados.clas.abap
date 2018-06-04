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

class zcl_ca_ctrlreq_banco_dados definition
  public
  create public .

  public section.

    interfaces zif_ca_ctrlreq_banco_dados .

    class-methods get_instance
      returning
        value(r_return) type ref to zif_ca_ctrlreq_banco_dados .
    class-methods set_instance
      importing
        !i_instance type ref to zif_ca_ctrlreq_banco_dados .
  protected section.
  private section.

    class-data go_instance type ref to zif_ca_ctrlreq_banco_dados .
ENDCLASS.



CLASS ZCL_CA_CTRLREQ_BANCO_DADOS IMPLEMENTATION.


  method get_instance.

    if go_instance is not bound.
      go_instance  = new zcl_ca_ctrlreq_banco_dados( ).
    endif.

    r_return = go_instance.

  endmethod.


  method set_instance.
    go_instance = cast #( i_instance ).
  endmethod.


  method zif_ca_ctrlreq_banco_dados~commit_work.
    commit work.
  endmethod.


  method zif_ca_ctrlreq_banco_dados~deletar_request.

    delete from ztca_requests where trkorr eq i_id_request.
    if sy-subrc ne 0.
      raise exception type zcx_ca_ctrlreq_excecoes
        exporting
          textid = zcx_ca_ctrlreq_excecoes=>erro_deletar_request.
    endif.
  endmethod.


  method zif_ca_ctrlreq_banco_dados~deletar_request_copia.

    if i_id_request_origem is initial.
      return.
    endif.

    delete from ztca_requests where request_origem = i_id_request_origem
                                and trfunction     = zif_ca_ctrlreq_request=>cc_tipo-transporte_copia.

  endmethod.


  method zif_ca_ctrlreq_banco_dados~get_descricao_request.
    if i_request is initial.
      return.
    endif.

    select single as4text
        from e07t
        into r_descricao
        where trkorr = i_request
        and langu = 'P'.
  endmethod.


  method zif_ca_ctrlreq_banco_dados~get_e070.

    select single *
      from e070
      into r_e070
      where trkorr = i_request.

    if sy-subrc ne 0.
      raise exception type zcx_ca_ctrlreq_excecoes
        exporting
          textid = zcx_ca_ctrlreq_excecoes=>request_invalida.
    endif.

  endmethod.


  method zif_ca_ctrlreq_banco_dados~get_request.

    select single *
    from ztca_requests
    into r_request
    where trkorr = i_request.

    if sy-subrc <> 0.

      raise exception type zcx_ca_ctrlreq_excecoes
        exporting
          textid = zcx_ca_ctrlreq_excecoes=>request_invalida.

    endif.

  endmethod.


  method zif_ca_ctrlreq_banco_dados~get_requests.
**    SELECT * FROM ztca_requests
* SELECT *
* FROM ztrequests inner join e070 on ztrequests~trkorr = e070~trkorr
*    INTO corresponding fields of table rt_request
*    WHERE ztrequests~trkorr     IN iw_range-request               AND
*          ztrequests~ricef      IN iw_range-cr                    AND
*          ztrequests~projeto    IN iw_range-projeto               AND
*          ztrequests~modulo     IN iw_range-modulo                AND
*          ztrequests~consultor  IN iw_range-consultor             AND
*          ztrequests~funcional  IN iw_range-funcional             AND
*          ztrequests~data       IN iw_range-data                  AND
*          ztrequests~as4text    IN iw_range-descricao_sem_prefixo AND
**         ztrequests~ trfunction IN iw_range-trfunction            AND
*          ztrequests~hora       IN iw_range-hora
*          .


    select * from ztca_requests
       into  table rt_request
       where ztca_requests~trkorr     in iw_range-request               and
             ztca_requests~demanda    in iw_range-demanda               and
             ztca_requests~tipo_demanda    in iw_range-tipo_demanda     and
             ztca_requests~modulo     in iw_range-modulo                and
             ztca_requests~consultor  in iw_range-consultor             and
             ztca_requests~funcional  in iw_range-funcional             and
             ztca_requests~data       in iw_range-data                  and
             ztca_requests~as4text    in iw_range-descricao_sem_prefixo and
             ztca_requests~trfunction in iw_range-trfunction            and
             ztca_requests~process_area in iw_range-process_area            and
             ztca_requests~hora       in iw_range-hora
             .
  endmethod.


  method zif_ca_ctrlreq_banco_dados~get_request_hora.

    constants:
      c_customizing type c value 'K',
      c_workbench   type c value 'W'.

    select single trkorr
      into r_request
      from e070
      where trfunction in (c_customizing, c_workbench)
      and   as4user = sy-uname
      and   as4date = sy-datum
      and   as4time >= i_hora_inicio
      and   as4time <= i_hora_fim.

  endmethod.


  method zif_ca_ctrlreq_banco_dados~get_status_request.

    select single trstatus
       from e070
       into r_status
       where trkorr eq i_cod_request.

    if sy-subrc ne 0.

      raise exception type zcx_ca_ctrlreq_excecoes
        exporting
          textid = zcx_ca_ctrlreq_excecoes=>request_invalida.

    endif.

  endmethod.


  method zif_ca_ctrlreq_banco_dados~get_task_request.

    select trkorr
      from e070
      into table rt_task
      where strkorr = i_request
      and trstatus in ('D','L').

  endmethod.


  method zif_ca_ctrlreq_banco_dados~rollback.
    rollback work.
  endmethod.


  method zif_ca_ctrlreq_banco_dados~salvar_request.

    modify ztca_requests from i_request.
    if sy-subrc ne 0.
      raise exception type zcx_ca_ctrlreq_excecoes
        exporting
          textid = zcx_ca_ctrlreq_excecoes=>erro_salvar_request.
    endif.

  endmethod.
ENDCLASS.
