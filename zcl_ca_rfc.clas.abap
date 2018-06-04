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

CLASS zcl_ca_rfc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES zif_ca_ctrlreq_request_reposit .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CA_RFC IMPLEMENTATION.


  METHOD zif_ca_ctrlreq_request_reposit~get_request.

  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~get_requests.

  CALL FUNCTION 'ZFCA_GET_REQUEST'
    EXPORTING
      iw_range     = iw_range
      iw_ambientes = iw_ambientes
    IMPORTING
      rt_request   = rt_report
    .

  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~importa_request.

  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~objeto_to_wa.



  ENDMETHOD.


  METHOD zif_ca_ctrlreq_request_reposit~salvar.

  ENDMETHOD.
ENDCLASS.
