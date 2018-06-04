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

class ZCL_CA_CALL_TRANSACTION_SHDB definition
  public
  final
  create private .

public section.

  interfaces ZIF_CA_CALL_TRANSACTION_SHDB .

  methods CONSTRUCTOR
    importing
      !IV_TCODE type SYTCODE .
  class-methods FACTORY
    importing
      !IV_TCODE type SYTCODE
    returning
      value(RE_CALL_TRANS_SHDB_OBJ) type ref to ZIF_CA_CALL_TRANSACTION_SHDB .
protected section.
private section.

  data CODIGO_TRANSACAO type SYTCODE .
  data MAPEAMENTO_SHDB type WDKBDCDATA_TTY .
  data:
    mensagens_tab    TYPE STANDARD TABLE OF bdcmsgcoll .
ENDCLASS.



CLASS ZCL_CA_CALL_TRANSACTION_SHDB IMPLEMENTATION.


METHOD constructor.

    me->codigo_transacao = iv_tcode.

    me->mapeamento_shdb = VALUE #( ( fnam     = codigo_transacao
                                     dynbegin = 'T'              ) ).

  ENDMETHOD.


METHOD factory.

    re_call_trans_shdb_obj = NEW zcl_ca_call_transaction_shdb( iv_tcode ).

  ENDMETHOD.


METHOD zif_ca_call_transaction_shdb~adiciona_passo.

    DATA lt_bdcdata LIKE mapeamento_shdb.

    FIELD-SYMBOLS <ls_bdcdata> LIKE LINE OF mapeamento_shdb.

*----- dynpros
    lt_bdcdata = cl_wdk_bdc_action=>generate_bdcdata_for_dynpros( it_dynpros ).
    APPEND LINES OF lt_bdcdata TO mapeamento_shdb.

*----- navigation command (okcode)
    IF iv_okcode IS NOT INITIAL.
      APPEND INITIAL LINE TO mapeamento_shdb ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = cl_wdk_utilities=>sc_bdc_fnam_okcode.
      <ls_bdcdata>-fval = iv_okcode.
    ENDIF.

*----- field values
    lt_bdcdata = cl_wdk_bdc_action=>generate_bdcdata_for_values( it_values ).
    APPEND LINES OF lt_bdcdata TO mapeamento_shdb.


  ENDMETHOD.


METHOD zif_ca_call_transaction_shdb~executar.

    IF im_opcoes_avancadas IS NOT INITIAL.

      CALL TRANSACTION me->codigo_transacao WITHOUT AUTHORITY-CHECK
      USING            me->mapeamento_shdb
      OPTIONS FROM     im_opcoes_avancadas
      MESSAGES INTO    me->mensagens_tab.

    ELSE.

      CALL TRANSACTION me->codigo_transacao WITHOUT AUTHORITY-CHECK
      USING            me->mapeamento_shdb
      MODE             im_modo
      UPDATE           im_atualizacao
      MESSAGES INTO    me->mensagens_tab.

    ENDIF.

    ex_sy_subrc  = sy-subrc.
    ex_mensagens = me->mensagens_tab.

  ENDMETHOD.


METHOD zif_ca_call_transaction_shdb~get_mapeamento_shdb.

    re_mapeamento_shdb_tab = me->mensagens_tab.

  ENDMETHOD.


METHOD zif_ca_call_transaction_shdb~get_mensagens.

    re_mensagens = me->mensagens_tab.

  ENDMETHOD.


METHOD zif_ca_call_transaction_shdb~set_mapeamento_shdb.

    me->mapeamento_shdb = im_mapeamento_shdb_tab.

  ENDMETHOD.
ENDCLASS.
