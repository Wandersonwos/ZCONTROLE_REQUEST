class zcl_zcontrole_request_dpc_ext definition
  public
  inheriting from zcl_zcontrole_request_dpc
  create public .

  public section.
  protected section.

    methods requestset_get_entityset
         redefinition .
  private section.
    methods convert_to_timestamp
      importing
        i_data             type zif_ca_ctrlreq_request_reposit=>ty_report-tprd
        i_hora             type zif_ca_ctrlreq_request_reposit=>ty_report-hprd
      returning
        value(r_timestamp) type zeca_requests-data_hora_prd.
endclass.



class zcl_zcontrole_request_dpc_ext implementation.


  method convert_to_timestamp.

    if i_data is initial.
      r_timestamp = '00000000'.
      return.
    endif.

    call function 'ABI_TIMESTAMP_CONVERT_INTO'
      exporting
        iv_date          = i_data    " Date and time, local date of user
        iv_time          = i_hora    " Date and time, local time for user
      importing
        ev_timestamp     = r_timestamp    " UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
      exceptions
        conversion_error = 1
        others           = 2.

  endmethod.


  method requestset_get_entityset.
**TRY.
*CALL METHOD SUPER->REQUESTSET_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.

    data(lo_banco_dados) = zcl_ca_ctrlreq_banco_dados=>get_instance( ).
    data(lo_req_reposit) = zcl_ca_ctrlreq_request_reposit=>get_instance( ).

    data lr_demanda  type range of numc7.

    try.
        lr_demanda = corresponding #( it_filter_select_options[ property = 'Demanda' ]-select_options ).

      catch cx_sy_itab_line_not_found.

        lr_demanda = value #( ( sign   = if_cwd_constants=>c_sign_inclusive
                                option = if_cwd_constants=>c_option_equals
                                low    = '18001'                              ) ).

    endtry.

    try .
        data(lt_requests) = lo_req_reposit->get_requests(
                                   exporting
                                     iw_range     = value #( demanda = lr_demanda )
                                     iw_ambientes = value #( dev = abap_true
                                                             qas = abap_true
                                                             prd = abap_true )     ).

        loop at lt_requests assigning field-symbol(<lw_requests>).
          append initial line to et_entityset assigning field-symbol(<lw_entityset>).

          <lw_entityset> = corresponding #( <lw_requests> ).

          if <lw_entityset>-dev <> abap_true.
            <lw_entityset>-dev = abap_false.
          endif.
          if <lw_entityset>-qas <> abap_true.
            <lw_entityset>-qas = abap_false.
          endif.
          if <lw_entityset>-prd <> abap_true.
            <lw_entityset>-prd = abap_false.
          endif.

          <lw_entityset>-data_hora            = me->convert_to_timestamp( i_data = <lw_requests>-data
                                                                          i_hora = <lw_requests>-hora ).

          <lw_entityset>-data_hora_exportacao = me->convert_to_timestamp( i_data = <lw_requests>-dataex
                                                                          i_hora = <lw_requests>-horaex ).

          <lw_entityset>-data_hora_dev        = me->convert_to_timestamp( i_data = <lw_requests>-tdev
                                                                          i_hora = <lw_requests>-hdev ).

          <lw_entityset>-data_hora_qas        = me->convert_to_timestamp( i_data = <lw_requests>-tqas
                                                                          i_hora = <lw_requests>-hqas ).

          <lw_entityset>-data_hora_prd        = me->convert_to_timestamp( i_data = <lw_requests>-tprd
                                                                          i_hora = <lw_requests>-hprd ).
        endloop.

        es_response_context-count = lines( lt_requests ).

      catch zcx_ca_ctrlreq_excecoes into data(lo_erro).

        raise exception type /iwbep/cx_mgw_busi_exception
          exporting
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = conv #( lo_erro->get_text( ) ).

    endtry.



  endmethod.
endclass.
