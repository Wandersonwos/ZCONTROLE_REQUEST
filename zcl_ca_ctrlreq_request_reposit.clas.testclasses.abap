*CLASS zcl_ca_test_request_reposit DEFINITION FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS
*.
*  PRIVATE SECTION.
*    DATA:
*      f_cut              TYPE REF TO zcl_ca_ctrlreq_request_reposit, "class under test
*      gw_request_mock    TYPE ztca_requests,
*      go_request         TYPE REF TO zif_ca_ctrlreq_request,
*      go_mocker_shdb     TYPE REF TO zif_ca_ctrlreq_shdb,
*      go_request_reposit TYPE REF TO zif_ca_ctrlreq_request_reposit.
*    METHODS:
*      setup,
*      salvar_com_dados_corretos FOR TESTING,
*      salvar_sem_descricao FOR TESTING,
*      salvar_sem_tipo_request FOR TESTING,
*      falha_banco_dados FOR TESTING,
*      teste_padrao_singleton FOR TESTING.
*ENDCLASS.       "zcl_Ca_Test_Request_Reposit
*
*
*CLASS zcl_ca_test_request_reposit IMPLEMENTATION.
*
*  METHOD setup.
*    gw_request_mock = VALUE ztca_requests(
*                              trkorr          = 'ED09999657'
*                              modulo          = 'SD'
*                              consultor       = 'consultor x'
*                              as4text         = 'sou a descrição'
*                              funcional       = 'funcional Y'
*                              data            = '20171501'
**                                 hora            = '103000'
**                                 versao          =
**                                 excluir         =
**                                 dataex          =
**                                 horaex          =
*                              demanda         = '777777'
*                              tipo_demanda    = 'PJ'
**                                 aiprd           =
*                              request_origem  = ''
*                              process_area        = 'HRM'
*                              trfunction      = zif_ca_ctrlreq_request=>cc_tipo-workbench
*     ).
*
*    go_request = CAST zif_ca_ctrlreq_request(  NEW zcl_ca_ctrlreq_request(
*    i_request               = ''
*    i_demanda               = gw_request_mock-demanda
*    i_tipo_demanda          = gw_request_mock-tipo_demanda
*    i_modulo                = gw_request_mock-modulo
*    i_consultor             = gw_request_mock-consultor
*    i_funcional             = gw_request_mock-funcional
*    i_data                  = gw_request_mock-data
*    i_descricao_sem_prefixo = gw_request_mock-as4text
*    i_tipo_request          = gw_request_mock-trfunction
*    i_request_origem        = gw_request_mock-request_origem
*    i_process_area          = gw_request_mock-process_area
*    )
*).
*
*    DATA lo_mocker TYPE REF TO zif_mocka_mocker.
*    DATA lo_request_reposit TYPE REF TO zif_ca_ctrlreq_request_reposit.
*
*    "Configurando SHDB.
*    lo_mocker = zcl_mocka_mocker=>zif_mocka_mocker~mock( zif_ca_ctrlreq_shdb=>cc_nome_interface ).
*    DATA(lo_mocker_method) =  lo_mocker->method( 'executa_shdb' ).
*    lo_mocker_method->with(  i_p1 = go_request ).
**    lo_mocker_method->returns( lv_request ).
*    go_mocker_shdb ?= lo_mocker->generate_mockup( ).
*
*    "Configurando Banco De Dados
*    lo_mocker = zcl_mocka_mocker=>zif_mocka_mocker~mock( zif_ca_ctrlreq_banco_dados=>cc_nome_interface ).
*    lo_mocker_method =  lo_mocker->method( 'salvar_request' ).
*    lo_mocker_method->with(  i_p1 = gw_request_mock  ).
*    lo_mocker_method = lo_mocker->method( 'commit_work' ).
*    lo_mocker_method = lo_mocker->method( 'get_request_hora' ).
*    lo_mocker_method->with(  i_p1 = sy-uzeit i_p2 = sy-uzeit ).
*    lo_mocker_method->returns( gw_request_mock-trkorr  ).
*
*    DATA(lo_mocker_banco_dados) = CAST zif_ca_ctrlreq_banco_dados( lo_mocker->generate_mockup( ) ).
*
*    zcl_ca_ctrlreq_banco_dados=>set_instance(  lo_mocker_banco_dados ).
*    go_request_reposit = zcl_ca_ctrlreq_request_reposit=>get_instance( ).
*  ENDMETHOD.
*
*  METHOD salvar_com_dados_corretos.
*
*    TRY.
*        go_request_reposit->salvar(
*        EXPORTING
*          i_shdb                  = CAST #( go_mocker_shdb )
*        CHANGING
*          c_request               = go_request
*      ).
*        cl_abap_unit_assert=>assert_equals(
*          EXPORTING
*            act                  =     go_request->get_request( )
*            exp                  =     gw_request_mock-trkorr
*        ).
*      CATCH zcx_ca_ctrlreq_excecoes INTO DATA(lo_erro).
*        cl_abap_unit_assert=>fail(
*          EXPORTING
*            msg    =      lo_erro->msgv1
*        ).
*    ENDTRY.
*
*  ENDMETHOD.
*
*  METHOD teste_padrao_singleton.
*    "Testa Padrão Singleton
*    DATA : lo_ca_ctrlreq_request_reposit TYPE REF TO zif_ca_ctrlreq_request_reposit.
*    cl_abap_unit_assert=>assert_not_bound( lo_ca_ctrlreq_request_reposit ).
*    lo_ca_ctrlreq_request_reposit = zcl_ca_ctrlreq_request_reposit=>get_instance( ).
*    cl_abap_unit_assert=>assert_bound( lo_ca_ctrlreq_request_reposit ).
*  ENDMETHOD.
*
*  METHOD salvar_sem_descricao.
*
*    go_request->set_descricao( '' ).
*    TRY.
*        go_request_reposit->salvar(
*        EXPORTING
*          i_shdb                  = CAST #( go_mocker_shdb )
*        CHANGING
*          c_request               = go_request
*      ).
*        cl_abap_unit_assert=>fail(
*                 EXPORTING
*                   msg    =      'Exception não lançada '
*               ).
*      CATCH zcx_ca_ctrlreq_excecoes INTO DATA(lo_erro).
*        cl_abap_unit_assert=>assert_bound(
*          EXPORTING
*            act              =    lo_erro ).
*    ENDTRY.
*  ENDMETHOD.
*
*  METHOD salvar_sem_tipo_request.
*
*    go_request->set_tipo_request( '' ).
*    TRY.
*        go_request_reposit->salvar(
*        EXPORTING
*          i_shdb                  = CAST #( go_mocker_shdb )
*        CHANGING
*          c_request               = go_request
*      ).
*        cl_abap_unit_assert=>fail(
*                  EXPORTING
*                    msg    =      'Exception não lançada '
*                ).
*      CATCH zcx_ca_ctrlreq_excecoes INTO DATA(lo_erro).
*        cl_abap_unit_assert=>assert_bound(
*          EXPORTING
*            act              =    lo_erro ).
*
*
*    ENDTRY.
*  ENDMETHOD.
*
*  METHOD falha_banco_dados.
*    "Configurando Banco De Dados
*
*    DATA(lo_ctrlreq_excecoes) = NEW zcx_ca_ctrlreq_excecoes( ).
*
*    DATA(lo_mocker) = zcl_mocka_mocker=>zif_mocka_mocker~mock( zif_ca_ctrlreq_banco_dados=>cc_nome_interface ).
*    DATA(lo_mocker_method) =  lo_mocker->method( 'salvar_request' ).
*    lo_mocker_method->raises( NEW zcx_ca_ctrlreq_excecoes( ) ).
*    lo_mocker_method = lo_mocker->method( 'get_request_hora' ).
*    lo_mocker_method->with(  i_p1 = sy-uzeit i_p2 = sy-uzeit ).
*    lo_mocker_method->returns( gw_request_mock-trkorr  ).
*    DATA(lo_mocker_banco_dados) = CAST zif_ca_ctrlreq_banco_dados( lo_mocker->generate_mockup( ) ).
*    zcl_ca_ctrlreq_banco_dados=>set_instance(  lo_mocker_banco_dados ).
*
*    TRY.
*        go_request_reposit->salvar(
*          EXPORTING
*            i_shdb                  = CAST #( go_mocker_shdb )
*          CHANGING
*            c_request               = go_request
*        ).
*        cl_abap_unit_assert=>fail(
*                  EXPORTING
*                    msg    =      'Exception não lançada '
*        ).
*      CATCH zcx_ca_ctrlreq_excecoes INTO DATA(lo_erro).
*        cl_abap_unit_assert=>assert_bound(
*          EXPORTING
*            act              =    lo_erro ).
*
*    ENDTRY.
*
*  ENDMETHOD.
*
*ENDCLASS.
