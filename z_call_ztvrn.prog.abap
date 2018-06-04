REPORT z_call_ztvrn.

START-OF-SELECTION.

  CONSTANTS c_url TYPE c LENGTH 1000 VALUE 'http://10.198.208.18:8000/sap/bc/webdynpro/sap/zwd_ca_ctrlreq?WDTHEMEROOT=Radix&sap-client=100&sap-language=PT#'.


  CALL FUNCTION 'CALL_BROWSER'
    EXPORTING
      url                    = c_url    " URL of Browser Call
    EXCEPTIONS
      frontend_not_supported = 1
      frontend_error         = 2
      prog_not_found         = 3
      no_batch               = 4
      unspecified_error      = 5
      OTHERS                 = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
