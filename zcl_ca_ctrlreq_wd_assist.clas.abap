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

class zcl_ca_ctrlreq_wd_assist definition
  public
  inheriting from cl_wd_component_assistance
  create public .

  public section.

    aliases get_text
      for if_wd_component_assistance~get_text .

    constants:
      begin of c_id_botao,
        transporte_copia    type string value 'TRANSPORTE_COPIA', "#EC NOTEXT
        libera_request      type string value 'LIBERA_REQUEST', "#EC NOTEXT
        atualizar           type string value 'ATUALIZAR',  "#EC NOTEXT
        visualizar_log      type string value 'VISUALIZAR_LOG', "#EC NOTEXT
        editar_request      type string value 'EDITAR_REQUEST', "#EC NOTEXT
        exibir_objetos      type string value 'EXIBIR_OBJETOS', "#EC NO TEXT
        cria_por_referencia type string value 'CRIA_POR_REFERENCIA', "#EC NO TEXT
        importar_req_qa     type string value 'IMPORTAR_REQ_QA', "#EC NO TEXT
      end of c_id_botao,
      c_nome_node type string value 'LISTA_REQUESTS'.       "#EC NOTEXT



    methods:

      busca_request
        importing
          !i_ranges type wdr_so_t_range_ref,

      is_request_liberada
        importing
          i_cod_request     type e070-trkorr
        returning
          value(r_resposta) type wdy_boolean
        raising
          zcx_ca_ctrlreq_excecoes,

      get_process_area_ddtext
        changing
          ct_process_area_domain type dd07v_tab.
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_CA_CTRLREQ_WD_ASSIST IMPLEMENTATION.


  method busca_request.

    data(lt_requests) = zcl_ca_ctrlreq_banco_dados=>get_instance( )->get_requests( iw_range = value #( ) ).

  endmethod.


  method get_process_area_ddtext.

    call function 'DDIF_DOMA_GET'
      exporting
        name          = '/ACNADMS/DO_MODUL'
*       STATE         = 'A'
        langu         = 'E'
*   IMPORTING
*       GOTSTATE      =
*       DD01V_WA      =
      tables
        dd07v_tab     = ct_process_area_domain
      exceptions
        illegal_input = 1
        others        = 2.


  endmethod.


  method is_request_liberada.

    r_resposta = zcl_ca_ctrlreq_request_reposit=>get_instance( )->get_request( i_cod_request )->is_liberada( ).

  endmethod.
ENDCLASS.
