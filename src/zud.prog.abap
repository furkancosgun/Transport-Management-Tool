*&---------------------------------------------------------------------*
*& Report ZUD - Transport Management Tool
*&---------------------------------------------------------------------*
*& Purpose: Facilitates the download of released Transport Requests (TR)
*& to the local client as ZIP files and supports uploading/importing TRs
*& from local files back to the SAP server.
*&---------------------------------------------------------------------*
REPORT zud.

" -----------------------------------------------------------------------
" Selection Screen Definition
" -----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t01.
  PARAMETERS p_upload RADIOBUTTON GROUP gr1 USER-COMMAND uc1 DEFAULT 'X'.
  PARAMETERS p_downld RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t02.
  " Download Parameters
  PARAMETERS p_trkorr TYPE trkorr MODIF ID m1.
  PARAMETERS p_folder TYPE string MODIF ID m1 LOWER CASE.

  " Upload Parameters
  PARAMETERS p_filenm TYPE string MODIF ID m2 LOWER CASE.
  PARAMETERS p_imprt  RADIOBUTTON GROUP gr2 MODIF ID m2 DEFAULT 'X'.
  PARAMETERS p_pop    RADIOBUTTON GROUP gr2 MODIF ID m2.
  PARAMETERS p_noimp  RADIOBUTTON GROUP gr2 MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b2.

" -----------------------------------------------------------------------
" CLASS lcx_transport_manager_message
" Purpose: Custom exception class for transport management errors
" -----------------------------------------------------------------------
CLASS lcx_transport_manager_message DEFINITION
  INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    TYPES tt_stdout TYPE STANDARD TABLE OF tpstdout WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF mc_general_error,
        msgid TYPE symsgid      VALUE '00',
        msgno TYPE symsgno      VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF mc_general_error.

    DATA msgv1 TYPE msgv1.
    DATA msgv2 TYPE msgv2.
    DATA msgv3 TYPE msgv3.
    DATA msgv4 TYPE msgv4.
    DATA mt_stdout TYPE tt_stdout.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL
                msgv1     TYPE msgv1                    OPTIONAL
                msgv2     TYPE msgv2                    OPTIONAL
                msgv3     TYPE msgv3                    OPTIONAL
                msgv4     TYPE msgv4                    OPTIONAL.

    CLASS-METHODS raise
      IMPORTING iv_message TYPE string
      RAISING   lcx_transport_manager_message.

    CLASS-METHODS raise_syst
      RAISING lcx_transport_manager_message.

    CLASS-METHODS raise_tp_failure
      IMPORTING iv_message TYPE string
                it_stdout  TYPE tt_stdout
      RAISING   lcx_transport_manager_message.

  PRIVATE SECTION.
    DATA message TYPE string.
ENDCLASS.


CLASS lcx_transport_manager_message IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise.
    " Raises exception with a custom string message
    DATA lv_message TYPE c LENGTH 200.

    lv_message = iv_message.

    RAISE EXCEPTION TYPE lcx_transport_manager_message
      EXPORTING textid = lcx_transport_manager_message=>mc_general_error
                msgv1  = lv_message+000(50)
                msgv2  = lv_message+050(50)
                msgv3  = lv_message+100(50)
                msgv4  = lv_message+150(50).
  ENDMETHOD.

  METHOD raise_syst.
    " Converts current SY-MSG variables into an exception
    DATA lv_message TYPE c LENGTH 200.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            INTO lv_message
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    RAISE EXCEPTION TYPE lcx_transport_manager_message
      EXPORTING textid = lcx_transport_manager_message=>mc_general_error
                msgv1  = lv_message+000(50)
                msgv2  = lv_message+050(50)
                msgv3  = lv_message+100(50)
                msgv4  = lv_message+150(50).
  ENDMETHOD.

  METHOD raise_tp_failure.
    " Raises exception carrying a custom message AND tp stdout output
    DATA lv_message TYPE c LENGTH 200.
    DATA lo_ex      TYPE REF TO lcx_transport_manager_message.

    lv_message = iv_message.

    lo_ex = NEW lcx_transport_manager_message(
      textid = lcx_transport_manager_message=>mc_general_error
      msgv1  = lv_message+000(50)
      msgv2  = lv_message+050(50)
      msgv3  = lv_message+100(50)
      msgv4  = lv_message+150(50) ).

    lo_ex->mt_stdout = it_stdout.

    RAISE EXCEPTION lo_ex.
  ENDMETHOD.
ENDCLASS.


" -----------------------------------------------------------------------
" INTERFACE lif_file_manager
" Purpose: Abstract file operations for both Client and Server
" -----------------------------------------------------------------------
INTERFACE lif_file_manager.
  METHODS read
    IMPORTING iv_file        TYPE string
    RETURNING VALUE(rv_data) TYPE xstring
    RAISING   lcx_transport_manager_message.

  METHODS write
    IMPORTING iv_file TYPE string
              iv_data TYPE xstring
    RAISING   lcx_transport_manager_message.

  METHODS get_seperator
    RETURNING VALUE(rv_seperator) TYPE string
    RAISING   lcx_transport_manager_message.
ENDINTERFACE.


" -----------------------------------------------------------------------
" CLASS lcl_client_file_manager
" Purpose: Handles file I/O on the user's local PC (Presentation Server)
" -----------------------------------------------------------------------
CLASS lcl_client_file_manager DEFINITION.
  PUBLIC SECTION.
    CONSTANTS lc_bin_len TYPE i VALUE 1024.

    TYPES ty_binary TYPE x LENGTH lc_bin_len.
    TYPES tt_binary TYPE STANDARD TABLE OF ty_binary WITH DEFAULT KEY.

    INTERFACES lif_file_manager.
ENDCLASS.


CLASS lcl_client_file_manager IMPLEMENTATION.
  METHOD lif_file_manager~get_seperator.
    DATA lv_seperator TYPE c LENGTH 1.

    cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = lv_seperator ).
    rv_seperator = lv_seperator.
  ENDMETHOD.

  METHOD lif_file_manager~read.
    DATA lt_bin TYPE tt_binary.
    DATA lv_len TYPE i.

    cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = iv_file
                                                     filetype   = 'BIN'
                                          IMPORTING  filelength = lv_len
                                          CHANGING   data_tab   = lt_bin
                                          EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING  input_length = lv_len
      IMPORTING  buffer       = rv_data
      TABLES     binary_tab   = lt_bin
      EXCEPTIONS OTHERS       = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_file_manager~write.
    DATA lv_len TYPE i.
    DATA lt_bin TYPE tt_binary.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING  buffer        = iv_data
      IMPORTING  output_length = lv_len
      TABLES     binary_tab    = lt_bin
      EXCEPTIONS OTHERS        = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = lv_len
                                                       filename     = iv_file
                                                       filetype     = 'BIN'
                                            CHANGING   data_tab     = lt_bin
                                            EXCEPTIONS OTHERS       = 1 ).
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


" -----------------------------------------------------------------------
" CLASS lcl_server_file_manager
" Purpose: Handles file I/O on the SAP Application Server (DIR_TRANS)
" -----------------------------------------------------------------------
CLASS lcl_server_file_manager DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_file_manager.

    METHODS constructor RAISING lcx_transport_manager_message.

  PRIVATE SECTION.
    DATA mv_transdir TYPE text255.
ENDCLASS.


CLASS lcl_server_file_manager IMPLEMENTATION.
  METHOD constructor.
    " Retrieve the global transport directory path
    CALL FUNCTION 'RSPO_R_SAPGPARAM'
      EXPORTING  name   = 'DIR_TRANS'
      IMPORTING  value  = mv_transdir
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_file_manager~get_seperator.
    rv_seperator = COND #( WHEN sy-opsys CP '*Windows*' THEN '\' ELSE '/' ).
  ENDMETHOD.

  METHOD lif_file_manager~read.
    DATA lv_file      TYPE string.
    DATA lv_seperator TYPE string.

    lv_seperator = lif_file_manager~get_seperator( ).
    CONCATENATE mv_transdir iv_file INTO lv_file SEPARATED BY lv_seperator.

    OPEN DATASET lv_file FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    READ DATASET lv_file INTO rv_data.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    CLOSE DATASET lv_file.
  ENDMETHOD.

  METHOD lif_file_manager~write.
    DATA lv_file      TYPE string.
    DATA lv_seperator TYPE string.

    lv_seperator = lif_file_manager~get_seperator( ).
    CONCATENATE mv_transdir iv_file INTO lv_file SEPARATED BY lv_seperator.

    OPEN DATASET lv_file FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    TRANSFER iv_data TO lv_file.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    CLOSE DATASET lv_file.
  ENDMETHOD.
ENDCLASS.


" -----------------------------------------------------------------------
" CLASS lcl_transport_manager
" Purpose: Orchestrates TR download, upload, and STMS import logic
" -----------------------------------------------------------------------
CLASS lcl_transport_manager DEFINITION.
  PUBLIC SECTION.
    METHODS constructor RAISING lcx_transport_manager_message.

    METHODS download_request
      IMPORTING iv_request  TYPE trkorr
                iv_filename TYPE string
      RAISING   lcx_transport_manager_message.

    METHODS upload_request
      IMPORTING iv_filename TYPE string
      EXPORTING ev_request  TYPE trkorr
      RAISING   lcx_transport_manager_message.

    METHODS import_request
      IMPORTING iv_request TYPE trkorr
      RAISING   lcx_transport_manager_message.

    METHODS populate_request_tables
      IMPORTING iv_request TYPE trkorr
      RAISING   lcx_transport_manager_message.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF mc_paths,
        cofiles TYPE string VALUE 'cofiles',
        data    TYPE string VALUE 'data',
      END OF mc_paths.

    CONSTANTS:
      BEGIN OF mc_file_types,
        cofile TYPE c LENGTH 1 VALUE 'K',
        data   TYPE c LENGTH 1 VALUE 'R',
        all    TYPE c LENGTH 2 VALUE 'KR',
      END OF mc_file_types.

    CONSTANTS mc_zip_suffix TYPE string VALUE '.zip'.

    CONSTANTS:
      BEGIN OF mc_transport_category,
        cust TYPE string VALUE 'CUST',
        syst TYPE string VALUE 'SYST',
      END OF mc_transport_category.

    TYPES:
      BEGIN OF ty_file,
        name      TYPE string,
        content   TYPE xstring,
        file_type TYPE c LENGTH 1,
      END OF ty_file.

    TYPES tt_file TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

    METHODS get_req_files_from_server
      IMPORTING iv_request     TYPE trkorr
      RETURNING VALUE(rt_file) TYPE tt_file
      RAISING   lcx_transport_manager_message.

    METHODS get_req_files_from_client
      IMPORTING iv_filename    TYPE string
      EXPORTING ev_request     TYPE trkorr
      RETURNING VALUE(rt_file) TYPE tt_file
      RAISING   lcx_transport_manager_message.

    DATA mo_server_file_manager TYPE REF TO lif_file_manager.
    DATA mo_client_file_manager TYPE REF TO lif_file_manager.
ENDCLASS.


CLASS lcl_transport_manager IMPLEMENTATION.
  METHOD constructor.
    mo_server_file_manager = NEW lcl_server_file_manager( ).
    mo_client_file_manager = NEW lcl_client_file_manager( ).
  ENDMETHOD.

  METHOD download_request.
    " Checks if TR is released, zips cofile/data, and saves to client
    DATA lv_filename  TYPE string.
    DATA lv_seperator TYPE string.
    DATA lo_zip       TYPE REF TO cl_abap_zip.
    DATA lt_file      TYPE tt_file.
    FIELD-SYMBOLS <fs_file> LIKE LINE OF lt_file.

    SELECT SINGLE COUNT(*) FROM e070
      WHERE trkorr   = @iv_request
        AND trstatus = @sctsc_state_released.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise( 'Action Aborted: Only RELEASED transport requests can be downloaded.' ).
    ENDIF.

    lt_file = get_req_files_from_server( iv_request ).
    lo_zip = NEW cl_abap_zip( ).

    LOOP AT lt_file ASSIGNING <fs_file>.
      lo_zip->add( name    = <fs_file>-name
                   content = <fs_file>-content ).
    ENDLOOP.

    lv_seperator = mo_client_file_manager->get_seperator( ).

    " Handle trailing separator in path
    IF substring( val = iv_filename
                  off = strlen( iv_filename ) - 1 ) = lv_seperator.
      lv_filename = iv_filename && iv_request.
    ELSE.
      lv_filename = iv_filename && lv_seperator && iv_request.
    ENDIF.

    lv_filename = lv_filename && mc_zip_suffix.

    mo_client_file_manager->write( iv_file = lv_filename
                                   iv_data = lo_zip->save( ) ).
  ENDMETHOD.

  METHOD upload_request.
    " Unzips files and writes them to server's cofiles/data folders
    DATA lv_filename  TYPE string.
    DATA lv_seperator TYPE string.
    DATA lt_file      TYPE tt_file.
    FIELD-SYMBOLS <fs_file> LIKE LINE OF lt_file.

    lv_seperator = mo_server_file_manager->get_seperator( ).
    lt_file = get_req_files_from_client( EXPORTING iv_filename = iv_filename
                                         IMPORTING ev_request  = ev_request ).
    LOOP AT lt_file ASSIGNING <fs_file>.
      lv_filename = COND #( WHEN <fs_file>-file_type = mc_file_types-cofile THEN mc_paths-cofiles
                            WHEN <fs_file>-file_type = mc_file_types-data   THEN mc_paths-data
                            ELSE                                                 '' ).

      IF lv_filename IS INITIAL.
        CONTINUE.
      ENDIF.

      CONCATENATE lv_filename <fs_file>-name INTO lv_filename SEPARATED BY lv_seperator.
      mo_server_file_manager->write( iv_file = lv_filename
                                     iv_data = <fs_file>-content ).
    ENDLOOP.
  ENDMETHOD.

  METHOD import_request.
    " Appends TR to buffer and triggers STMS import
    DATA lt_request TYPE stms_wbo_requests.
    DATA lv_system  TYPE tmsbuffer-sysnam.
    DATA lv_client  TYPE tmsbuffer-tarcli.
    DATA lv_request TYPE tmsbuffer-trkorr.
    FIELD-SYMBOLS <fs_request> LIKE LINE OF lt_request.

    lv_system = sy-sysid.
    lv_client = sy-mandt.
    lv_request = iv_request.

    " Authority check for adding to buffer
    CALL FUNCTION 'TR_AUTHORITY_CHECK_ADMIN'
      EXPORTING  iv_adminfunction = 'TADD'
      EXCEPTIONS OTHERS           = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    " Add TR to the import queue
    CALL FUNCTION 'TMS_UI_APPEND_TR_REQUEST'
      EXPORTING  iv_system      = lv_system
                 iv_request     = lv_request
                 iv_expert_mode = 'X'
                 iv_ctc_active  = 'X'
      EXCEPTIONS OTHERS         = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    " Read TR details for client-specific filling if necessary
    CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_REQUEST'
      EXPORTING  iv_request       = lv_request
                 iv_target_system = lv_system
      IMPORTING  et_request_infos = lt_request
      EXCEPTIONS OTHERS           = 1.

    ASSIGN lt_request[ 1 ] TO <fs_request>.
    IF sy-subrc = 0 AND <fs_request>-e070-korrdev = mc_transport_category-cust.
      CALL FUNCTION 'TMS_MGR_MAINTAIN_TR_QUEUE'
        EXPORTING  iv_command = 'FILLCLIENT'
                   iv_system  = lv_system
                   iv_request = lv_request
                   iv_tarcli  = lv_client
        EXCEPTIONS OTHERS     = 1.
    ENDIF.

    " Authority check for import
    CALL FUNCTION 'TR_AUTHORITY_CHECK_ADMIN'
      EXPORTING  iv_adminfunction = 'IMPS'
      EXCEPTIONS OTHERS           = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    " Trigger Import
    CALL FUNCTION 'TMS_UI_IMPORT_TR_REQUEST'
      EXPORTING  iv_system      = lv_system
                 iv_request     = lv_request
                 iv_tarcli      = lv_client
                 iv_some_active = space
      EXCEPTIONS OTHERS         = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.
  ENDMETHOD.

  METHOD populate_request_tables.
    " Appends TR to buffer and populates E07* tables via tp cmd
    " (object-list import phase) WITHOUT performing a real import.
    " Equivalent of OS-level: tp cmd <TR> <SID> pf=<tp_profile>
    DATA lv_system  TYPE tmsbuffer-sysnam.
    DATA lv_client  TYPE tmsbuffer-tarcli.
    DATA lv_request TYPE tmsbuffer-trkorr.
    DATA lv_rc      TYPE stpa-retcode.
    DATA lv_msg     TYPE stpa-message.
    DATA lt_stdout  TYPE lcx_transport_manager_message=>tt_stdout.
    DATA lv_syst_msg TYPE c LENGTH 200.

    lv_system  = sy-sysid.
    lv_client  = sy-mandt.
    lv_request = iv_request.

    " Authority check for adding to buffer
    CALL FUNCTION 'TR_AUTHORITY_CHECK_ADMIN'
      EXPORTING  iv_adminfunction = 'TADD'
      EXCEPTIONS OTHERS           = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    " Add TR to the import queue (so it's visible in STMS)
    CALL FUNCTION 'TMS_UI_APPEND_TR_REQUEST'
      EXPORTING  iv_system      = lv_system
                 iv_request     = lv_request
                 iv_expert_mode = 'X'
                 iv_ctc_active  = 'X'
      EXCEPTIONS OTHERS         = 1.
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    " Object-list import phase: populates E070, E071, E07T, E071K from
    " the cofile/data files without running the actual import phases.
    " 'CMD' = IF_OCS_TRANSPORT_SYSTEM_ACCESS=>C_CMD_OBJLST_IMPORT.
    CALL FUNCTION 'TRINT_TP_INTERFACE'
      EXPORTING  iv_tp_command                = 'CMD'
                 iv_system_name               = lv_system
                 iv_transport_request         = lv_request
                 iv_client                    = lv_client
      IMPORTING  ev_tp_return_code            = lv_rc
                 ev_tp_message                = lv_msg
      TABLES     tt_stdout                    = lt_stdout
      EXCEPTIONS unsupported_tp_command       = 1
                 invalid_tp_command           = 2
                 missing_parameter            = 3
                 invalid_parameter            = 4
                 get_tpparam_failed           = 5
                 update_tp_destination_failed = 6
                 get_tms_info_failed          = 7
                 permission_denied            = 8
                 tp_call_failed               = 9
                 insert_tpstat_failed         = 10
                 insert_tplog_failed          = 11
                 OTHERS                       = 12.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              INTO lv_syst_msg
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      lcx_transport_manager_message=>raise_tp_failure(
        iv_message = CONV string( lv_syst_msg )
        it_stdout  = lt_stdout ).
    ENDIF.

    " tp return codes > 4 indicate errors (0/4 = OK / warnings).
    IF lv_rc > 4.
      lcx_transport_manager_message=>raise_tp_failure(
        iv_message = |Populate failed (tp RC={ lv_rc }): { lv_msg }|
        it_stdout  = lt_stdout ).
    ENDIF.
  ENDMETHOD.

  METHOD get_req_files_from_client.
    " Reads local ZIP and extracts TR cofile/data components
    DATA lo_zip    TYPE REF TO cl_abap_zip.
    DATA lv_bin    TYPE xstring.
    DATA lv_prefix TYPE string.
    DATA lv_suffix TYPE string.
    FIELD-SYMBOLS <fs_zip>  LIKE LINE OF lo_zip->files.
    FIELD-SYMBOLS <fs_file> LIKE LINE OF rt_file.

    lv_bin = mo_client_file_manager->read( iv_filename ).
    lo_zip = NEW cl_abap_zip( ).
    lo_zip->load( EXPORTING  zip    = lv_bin
                  EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      lcx_transport_manager_message=>raise_syst( ).
    ENDIF.

    LOOP AT lo_zip->files ASSIGNING <fs_zip>.
      IF <fs_zip>-name NA mc_file_types-all.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_file ASSIGNING <fs_file>.
      <fs_file>-name      = <fs_zip>-name.
      <fs_file>-file_type = <fs_zip>-name(1).

      lo_zip->get( EXPORTING  name    = <fs_zip>-name
                   IMPORTING  content = <fs_file>-content
                   EXCEPTIONS OTHERS  = 1 ).

      " Extract TR number from filename if not yet identified
      IF ev_request IS INITIAL.
        SPLIT <fs_file>-name AT '.' INTO lv_prefix lv_suffix.
        IF sy-subrc = 0.
          ev_request = lv_suffix && mc_file_types-cofile && lv_prefix+1.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF NOT line_exists( rt_file[ file_type = mc_file_types-cofile ] ).
      lcx_transport_manager_message=>raise( 'Structure Error: TR Header (Cofile) missing in ZIP.' ).
    ENDIF.

    IF NOT line_exists( rt_file[ file_type = mc_file_types-data ] ).
      lcx_transport_manager_message=>raise( 'Structure Error: TR Data (Data) missing in ZIP.' ).
    ENDIF.
  ENDMETHOD.

  METHOD get_req_files_from_server.
    " Reads the physical TR files from the SAP server
    DATA lv_seperator TYPE string.
    DATA lv_file      TYPE string.
    FIELD-SYMBOLS <fs_file> LIKE LINE OF rt_file.

    " Construct server filename (e.g. K900001.SID)
    lv_file = |{ iv_request+4 }.{ iv_request(3) }|.
    lv_seperator = mo_server_file_manager->get_seperator( ).

    " Read Cofile
    APPEND INITIAL LINE TO rt_file ASSIGNING <fs_file>.
    <fs_file>-name      = |{ mc_file_types-cofile }{ lv_file }|.
    <fs_file>-file_type = mc_file_types-cofile.
    <fs_file>-content   = mo_server_file_manager->read( |{ mc_paths-cofiles }{ lv_seperator }{ <fs_file>-name }| ).

    " Read Data file
    APPEND INITIAL LINE TO rt_file ASSIGNING <fs_file>.
    <fs_file>-name      = |{ mc_file_types-data }{ lv_file }|.
    <fs_file>-file_type = mc_file_types-data.
    <fs_file>-content   = mo_server_file_manager->read( |{ mc_paths-data }{ lv_seperator }{ <fs_file>-name }| ).
  ENDMETHOD.
ENDCLASS.


" -----------------------------------------------------------------------
" CLASS lcl_application
" Purpose: UI Logic, Screen Handling, and Application Entry Point
" -----------------------------------------------------------------------
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS initialization.
    METHODS start_of_selection.
    METHODS at_selection_screen_output.
    METHODS at_selection_screen_for_folder.
    METHODS at_selection_screen_for_file.
    METHODS at_selection_screen_for_trkorr.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF mc_modif_ids,
        download TYPE c LENGTH 2 VALUE 'M1',
        upload   TYPE c LENGTH 2 VALUE 'M2',
      END OF mc_modif_ids.

    CONSTANTS:
      BEGIN OF mc_texts,
        request  TYPE string VALUE 'Transport Request',
        folder   TYPE string VALUE 'Target Local Folder',
        file     TYPE string VALUE 'Source ZIP File',
        t01      TYPE string VALUE 'Operation Mode',
        t02      TYPE string VALUE 'Configuration',
        upload   TYPE string VALUE 'Upload to Server',
        download TYPE string VALUE 'Download to Client',
        import   TYPE string VALUE 'Import after Upload',
        populate TYPE string VALUE 'Populate Transport Tables Only',
        no_imp   TYPE string VALUE 'No Import Action',
      END OF mc_texts.

    CONSTANTS:
      BEGIN OF mc_messages,
        download_ok                TYPE string VALUE 'SUCCESS: Transport downloaded successfully.',
        upload_ok                  TYPE string VALUE 'SUCCESS: Transport uploaded to server.',
        import_ok                  TYPE string VALUE 'SUCCESS: Import process triggered.',
        populate_ok                TYPE string VALUE 'SUCCESS: Transport tables populated (no import performed).',
        request_must_be_provided   TYPE string VALUE 'Error: Please specify a Transport Request.',
        file_name_must_be_provided TYPE string VALUE 'Error: Source ZIP file path is required.',
        folder_must_be_provided    TYPE string VALUE 'Error: Target folder path is required.',
      END OF mc_messages.

    DATA mo_transport_manager TYPE REF TO lcl_transport_manager.
    DATA mx_message           TYPE REF TO cx_root.
ENDCLASS.


CLASS lcl_application IMPLEMENTATION.
  METHOD initialization.
    " Setup dynamic screen texts
    %_p_upload_%_app_%-text = mc_texts-upload.
    %_p_downld_%_app_%-text = mc_texts-download.
    %_p_trkorr_%_app_%-text = mc_texts-request.
    %_p_folder_%_app_%-text = mc_texts-folder.
    %_p_filenm_%_app_%-text = mc_texts-file.
    %_p_imprt_%_app_%-text  = mc_texts-import.
    %_p_pop_%_app_%-text    = mc_texts-populate.
    %_p_noimp_%_app_%-text  = mc_texts-no_imp.

    t01 = mc_texts-t01.
    t02 = mc_texts-t02.

    CONCATENATE sy-sysid 'K*' INTO p_trkorr.

    " Set default paths to desktop
    cl_gui_frontend_services=>get_desktop_directory( CHANGING   desktop_directory = p_folder
                                                     EXCEPTIONS OTHERS            = 1 ).
    cl_gui_frontend_services=>get_desktop_directory( CHANGING   desktop_directory = p_filenm
                                                     EXCEPTIONS OTHERS            = 1 ).
  ENDMETHOD.

  METHOD start_of_selection.
    DATA lv_request TYPE trkorr.

    TRY.
        mo_transport_manager = NEW #( ).

        CASE abap_true.
          WHEN p_downld.
            IF p_trkorr IS INITIAL.
              MESSAGE mc_messages-request_must_be_provided TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
            IF p_folder IS INITIAL.
              MESSAGE mc_messages-folder_must_be_provided TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.

            mo_transport_manager->download_request( iv_request  = p_trkorr
                                                    iv_filename = p_folder ).
            WRITE / mc_messages-download_ok COLOR COL_POSITIVE.

          WHEN p_upload.
            IF p_filenm IS INITIAL.
              MESSAGE mc_messages-file_name_must_be_provided TYPE 'S' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.

            mo_transport_manager->upload_request( EXPORTING iv_filename = p_filenm
                                                  IMPORTING ev_request  = lv_request ).
            WRITE / mc_messages-upload_ok COLOR COL_POSITIVE.

            CASE abap_true.
              WHEN p_imprt.
                mo_transport_manager->import_request( iv_request = lv_request ).
                WRITE / mc_messages-import_ok COLOR COL_POSITIVE.
              WHEN p_pop.
                mo_transport_manager->populate_request_tables( iv_request = lv_request ).
                WRITE / mc_messages-populate_ok COLOR COL_POSITIVE.
            ENDCASE.
        ENDCASE.
      CATCH cx_root INTO mx_message.
        WRITE / mx_message->get_text( ) COLOR COL_NEGATIVE.
        " If the failure carries tp stdout output, dump it for diagnostics
        IF mx_message IS INSTANCE OF lcx_transport_manager_message.
          DATA(lo_tm_msg) = CAST lcx_transport_manager_message( mx_message ).
          LOOP AT lo_tm_msg->mt_stdout ASSIGNING FIELD-SYMBOL(<fs_stdout>).
            WRITE / <fs_stdout>-line COLOR COL_NEGATIVE.
          ENDLOOP.
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD at_selection_screen_output.
    " Dynamic UI: Hide/Show fields based on Radio Button selection
    LOOP AT SCREEN.
      IF p_downld = abap_true.
        IF screen-group1 = mc_modif_ids-download.
          screen-required = 2.
        ELSEIF screen-group1 = mc_modif_ids-upload.
          screen-active = 0.
        ENDIF.
      ELSE.
        IF screen-group1 = mc_modif_ids-download.
          screen-active = 0.
        ELSEIF screen-group1 = mc_modif_ids-upload.
          screen-required = 2.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD at_selection_screen_for_file.
    DATA lt_filetable TYPE filetable.
    DATA lv_rc        TYPE sy-subrc.

    cl_gui_frontend_services=>file_open_dialog( EXPORTING  default_filename = '*.zip'
                                                           file_filter      = |ZIP Files (*.zip)\|*.zip\||
                                                CHANGING   file_table       = lt_filetable
                                                           rc               = lv_rc
                                                EXCEPTIONS OTHERS           = 1 ).
    p_filenm = VALUE #( lt_filetable[ 1 ] OPTIONAL ).
  ENDMETHOD.

  METHOD at_selection_screen_for_folder.
    cl_gui_frontend_services=>directory_browse( CHANGING   selected_folder = p_folder
                                                EXCEPTIONS OTHERS          = 1 ).
  ENDMETHOD.

  METHOD at_selection_screen_for_trkorr.
    " Search Help for released Transport Requests
    DATA ls_selection TYPE trwbo_selection.
    DATA ls_selected  TYPE trwbo_request_header.

    ls_selection-reqstatus = 'R'. " Released only

    CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
      EXPORTING iv_organizer_type   = ''
                is_selection        = ls_selection
      IMPORTING es_selected_request = ls_selected.

    IF ls_selected-trkorr IS NOT INITIAL.
      p_trkorr = ls_selected-trkorr.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" -----------------------------------------------------------------------
" Global Execution Logic
" -----------------------------------------------------------------------
DATA lo_app TYPE REF TO lcl_application.

LOAD-OF-PROGRAM.
  lo_app = NEW lcl_application( ).

INITIALIZATION.
  lo_app->initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lo_app->at_selection_screen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filenm.
  lo_app->at_selection_screen_for_file( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
  lo_app->at_selection_screen_for_folder( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_trkorr.
  lo_app->at_selection_screen_for_trkorr( ).

START-OF-SELECTION.
  lo_app->start_of_selection( ).
