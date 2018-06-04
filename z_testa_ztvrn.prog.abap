*&---------------------------------------------------------------------*
*& Report  Z_TESTA_ZTVRN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_testa_ztvrn.

PARAMETERS p_req TYPE e070-trkorr DEFAULT 'ED0K953528'.

START-OF-SELECTION.

  SELECT SINGLE trkorr
  FROM e070
  INTO p_req
  WHERE trkorr = p_req
  AND trfunction = zif_ca_ctrlreq_request=>cc_tipo-workbench.

  IF sy-subrc <> 0.
    MESSAGE 'Esta request não existe ou não é workbech' TYPE 'S' DISPLAY LIKE 'E'.
    REJECT.
  ENDIF.

  DATA(gw_request_mock) = VALUE ztca_requests(
                                trkorr          =  p_req "'ED0K953570'
                                modulo          = 'SD'
                                consultor       = 'consultor x'
                                as4text         = 'sou a descrição'
                                funcional       = 'funcional Y'
                                data            = '20171501'
*                                 hora            = '103000'
*                                 versao          =
*                                 excluir         =
*                                 dataex          =
*                                 horaex          =
                                demanda         = '777777'
                                tipo_demanda    = 'PJ'
*                                 aiprd           =
                                request_origem  = ''
                                process_area        = 'HRM'
                                trfunction      = zif_ca_ctrlreq_request=>cc_tipo-workbench ).



  DATA(go_request) = CAST zif_ca_ctrlreq_request(  NEW zcl_ca_ctrlreq_request(  i_request               = gw_request_mock-trkorr
                                                                                i_demanda               = gw_request_mock-demanda
                                                                                i_tipo_demanda          = gw_request_mock-tipo_demanda
                                                                                i_modulo                = gw_request_mock-modulo
                                                                                i_consultor             = gw_request_mock-consultor
                                                                                i_funcional             = gw_request_mock-funcional
                                                                                i_data                  = gw_request_mock-data
                                                                                i_descricao_sem_prefixo = gw_request_mock-as4text
                                                                                i_tipo_request          = gw_request_mock-trfunction
                                                                                i_request_origem        = gw_request_mock-request_origem
                                                                                i_process_area          = gw_request_mock-process_area    ) ).

  DATA(go_request_reposit) = zcl_ca_ctrlreq_request_reposit=>get_instance( ).

  TRY.

  DATA(lo_request_copia) = go_request->cria_copia( ).


      go_request_reposit->salvar( CHANGING c_request = lo_request_copia ).

    CATCH zcx_ca_ctrlreq_excecoes INTO DATA(lo_erro).    "
      MESSAGE lo_erro TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
