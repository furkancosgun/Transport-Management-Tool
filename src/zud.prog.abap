*&---------------------------------------------------------------------*
*& Report zud - Transport Management Tool
*&---------------------------------------------------------------------*
*& DEV : XFC
*&---------------------------------------------------------------------*
REPORT zud.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t01.
  PARAMETERS p_upload RADIOBUTTON GROUP gr1 USER-COMMAND uc1 DEFAULT 'X'.
  PARAMETERS p_downld RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t02.
  PARAMETERS p_trkorr TYPE trkorr MODIF ID m1.
  PARAMETERS p_folder TYPE string MODIF ID m1.

  PARAMETERS p_filenm TYPE string MODIF ID m2.
  PARAMETERS p_import AS CHECKBOX MODIF ID m2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

CLASS lcx_tr_manager_message DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_messages,
                 request_must_be_released TYPE string VALUE 'Transport requests must be released before download.',
                 header_file_missing      TYPE string VALUE 'Header file not found in the uploaded archive.',
                 content_file_missing     TYPE string VALUE 'Content file not found in the uploaded archive.',
                 file_read_error          TYPE string VALUE 'Error occurred while reading the file: &1',
                 file_write_error         TYPE string VALUE 'Error occurred while writing the file: &1',
                 transdir_error           TYPE string VALUE 'Failed to retrieve transport directory.',
                 client_separator_error   TYPE string VALUE 'Failed to retrieve client file separator.',
                 server_separator_error   TYPE string VALUE 'Failed to retrieve server file separator.',
                 tms_append_error         TYPE string VALUE 'Failed to append transport request to TMS queue.',
                 tms_import_error         TYPE string VALUE 'Failed to import transport request via TMS.',
                 authorization_error      TYPE string VALUE 'Missing required authorization: &1',
               END OF mc_messages.

    METHODS constructor
      IMPORTING iv_message    TYPE string OPTIONAL
                iv_message_v1 TYPE clike  OPTIONAL
                iv_message_v2 TYPE clike  OPTIONAL
                iv_message_v3 TYPE clike  OPTIONAL
                iv_message_v4 TYPE clike  OPTIONAL.

    CLASS-METHODS raise
      IMPORTING iv_message    TYPE string
                iv_message_v1 TYPE clike OPTIONAL
                iv_message_v2 TYPE clike OPTIONAL
                iv_message_v3 TYPE clike OPTIONAL
                iv_message_v4 TYPE clike OPTIONAL
      RAISING   lcx_tr_manager_message.

    METHODS get_text REDEFINITION.

  PRIVATE SECTION.
    DATA message TYPE string.
ENDCLASS.


CLASS lcx_tr_manager_message IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    message = iv_message.
    REPLACE ALL OCCURRENCES OF '&1' IN me->message WITH iv_message_v1.
    REPLACE ALL OCCURRENCES OF '&2' IN me->message WITH iv_message_v2.
    REPLACE ALL OCCURRENCES OF '&3' IN me->message WITH iv_message_v3.
    REPLACE ALL OCCURRENCES OF '&4' IN me->message WITH iv_message_v4.
  ENDMETHOD.

  METHOD get_text.
    result = me->message.
  ENDMETHOD.

  METHOD raise.
    RAISE EXCEPTION NEW lcx_tr_manager_message( iv_message    = iv_message
                                                iv_message_v1 = iv_message_v1
                                                iv_message_v2 = iv_message_v2
                                                iv_message_v3 = iv_message_v3
                                                iv_message_v4 = iv_message_v4 ).
  ENDMETHOD.
ENDCLASS.


INTERFACE lif_file_manager.
  METHODS read
    IMPORTING iv_path        TYPE string
    RETURNING VALUE(rv_data) TYPE xstring
    RAISING   lcx_tr_manager_message.

  METHODS write
    IMPORTING iv_path TYPE string
              iv_data TYPE xstring
    RAISING   lcx_tr_manager_message.

  METHODS get_seperator
    RETURNING VALUE(rv_seperator) TYPE char1.

  DATA seperator TYPE c LENGTH 1.
ENDINTERFACE.


CLASS lcl_client_file_manager DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_file_manager.

    METHODS constructor.
ENDCLASS.


CLASS lcl_client_file_manager IMPLEMENTATION.
  METHOD constructor.
    lif_file_manager~seperator = lif_file_manager~get_seperator( ).
  ENDMETHOD.

  METHOD lif_file_manager~read.
    TRY.
        rv_data = cl_xlft_gui_utils=>read_file( filename = iv_path ).
      CATCH cx_root.
        lcx_tr_manager_message=>raise( iv_message    = lcx_tr_manager_message=>mc_messages-file_read_error
                                       iv_message_v1 = iv_path ).
    ENDTRY.
  ENDMETHOD.

  METHOD lif_file_manager~write.
    TRY.
        cl_xlft_gui_utils=>write_file( filename = iv_path
                                       data     = iv_data ).
      CATCH cx_root.
        lcx_tr_manager_message=>raise( iv_message    = lcx_tr_manager_message=>mc_messages-file_write_error
                                       iv_message_v1 = iv_path ).
    ENDTRY.
  ENDMETHOD.

  METHOD lif_file_manager~get_seperator.
    cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = rv_seperator ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_server_file_manager DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_file_manager.

    METHODS constructor
      RAISING lcx_tr_manager_message.

    METHODS get_transdir
      RETURNING VALUE(rv_transdir) TYPE text255
      RAISING   lcx_tr_manager_message.

    DATA transdir TYPE string.
ENDCLASS.


CLASS lcl_server_file_manager IMPLEMENTATION.
  METHOD constructor.
    lif_file_manager~seperator = lif_file_manager~get_seperator( ).
    transdir                   = get_transdir( ).
  ENDMETHOD.

  METHOD get_transdir.
    CALL FUNCTION 'RSPO_R_SAPGPARAM'
      EXPORTING
        name   = 'DIR_TRANS'
      IMPORTING
        value  = rv_transdir
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message = lcx_tr_manager_message=>mc_messages-transdir_error ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_file_manager~read.
    DATA lv_filename TYPE string.

    CONCATENATE me->transdir
                me->lif_file_manager~seperator
                iv_path
                INTO lv_filename.

    OPEN DATASET lv_filename FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message    = lcx_tr_manager_message=>mc_messages-file_read_error
                                     iv_message_v1 = iv_path ).
    ENDIF.
    READ DATASET lv_filename INTO rv_data.
    CLOSE DATASET lv_filename.
  ENDMETHOD.

  METHOD lif_file_manager~write.
    DATA lv_filename TYPE string.

    CONCATENATE me->transdir
                me->lif_file_manager~seperator
                iv_path
                INTO lv_filename.

    OPEN DATASET lv_filename FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message    = lcx_tr_manager_message=>mc_messages-file_write_error
                                     iv_message_v1 = iv_path ).
    ENDIF.
    TRANSFER iv_data TO lv_filename.
    CLOSE DATASET lv_filename.
  ENDMETHOD.

  METHOD lif_file_manager~get_seperator.
    IF sy-opsys = 'Windows NT'.
      rv_seperator = '\'.
    ELSE.
      rv_seperator = '/'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_tr_manager DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      RAISING lcx_tr_manager_message.

    METHODS download_request
      IMPORTING iv_request TYPE e070-trkorr
                iv_folder  TYPE string
      RAISING   lcx_tr_manager_message.

    METHODS upload_request
      IMPORTING iv_filename TYPE string
      EXPORTING ev_request  TYPE trkorr
      RAISING   lcx_tr_manager_message.

    METHODS import_queue
      IMPORTING iv_request TYPE trkorr
      RAISING   lcx_tr_manager_message.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF mc_paths,
                 cofiles TYPE string VALUE 'cofiles',
                 data    TYPE string VALUE 'data',
               END OF mc_paths.

    CONSTANTS: BEGIN OF mc_file_types,
                 cofile TYPE c LENGTH 1 VALUE 'K',
                 data   TYPE c LENGTH 1 VALUE 'R',
                 all    TYPE c LENGTH 2 VALUE 'KR',
               END OF mc_file_types.

    CONSTANTS mc_zip_suffix TYPE string VALUE '.zip'.

    CONSTANTS: BEGIN OF mc_transport_category,
                 cust TYPE string VALUE 'CUST',
                 syst TYPE string VALUE 'SYST',
               END OF mc_transport_category.

    TYPES: BEGIN OF mty_file,
             name      TYPE string,
             content   TYPE xstring,
             file_type TYPE c LENGTH 1,
           END OF mty_file.

    TYPES mty_files TYPE STANDARD TABLE OF mty_file WITH EMPTY KEY.

    METHODS get_req_files_from_server
      IMPORTING iv_request      TYPE trkorr
      RETURNING VALUE(rt_files) TYPE mty_files
      RAISING   lcx_tr_manager_message.

    METHODS get_req_files_from_client
      IMPORTING iv_filename     TYPE string
      EXPORTING ev_request      TYPE trkorr
      RETURNING VALUE(rt_files) TYPE mty_files
      RAISING   lcx_tr_manager_message.

    DATA server_file_manager TYPE REF TO lif_file_manager.
    DATA client_file_manager TYPE REF TO lif_file_manager.
ENDCLASS.


CLASS lcl_tr_manager IMPLEMENTATION.
  METHOD constructor.
    server_file_manager = NEW lcl_server_file_manager( ).
    client_file_manager = NEW lcl_client_file_manager( ).
  ENDMETHOD.

  METHOD get_req_files_from_server.
    DATA lv_filename TYPE string.

    lv_filename = |{ iv_request+4 }.{ iv_request(3) }|.

    " Header File
    APPEND INITIAL LINE TO rt_files ASSIGNING FIELD-SYMBOL(<fs_file>).
    <fs_file>-name      = |{ mc_file_types-cofile }{ lv_filename }|.
    <fs_file>-file_type = mc_file_types-cofile.
    <fs_file>-content   = server_file_manager->read(
        iv_path = |{ mc_paths-cofiles }{ me->server_file_manager->seperator }{ <fs_file>-name }| ).

    " Content File
    APPEND INITIAL LINE TO rt_files ASSIGNING <fs_file>.
    <fs_file>-name      = |{ mc_file_types-data }{ lv_filename }|.
    <fs_file>-file_type = mc_file_types-data.
    <fs_file>-content   = server_file_manager->read(
                              iv_path = |{ mc_paths-data }{ me->server_file_manager->seperator }{ <fs_file>-name }| ).
  ENDMETHOD.

  METHOD download_request.
    DATA lv_filename TYPE string.

    SELECT SINGLE COUNT(*) FROM e070
      WHERE trkorr   = @iv_request
        AND trstatus = @sctsc_state_released.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message = lcx_tr_manager_message=>mc_messages-request_must_be_released ).
    ENDIF.

    DATA(lt_files) = get_req_files_from_server( iv_request ).

    FINAL(lo_zip) = NEW cl_abap_zip( ).

    LOOP AT lt_files REFERENCE INTO DATA(lr_file).
      lo_zip->add( name    = lr_file->name
                   content = lr_file->content ).
    ENDLOOP.

    IF substring( val = iv_folder
                  off = strlen( iv_folder ) - 1 ) = client_file_manager->seperator.
      lv_filename = |{ iv_folder }{ iv_request }{ mc_zip_suffix }|.
    ELSE.
      lv_filename = |{ iv_folder }{ me->client_file_manager->seperator }{ iv_request }{ mc_zip_suffix }|.
    ENDIF.

    client_file_manager->write( iv_path = lv_filename
                                iv_data = lo_zip->save( ) ).
  ENDMETHOD.

  METHOD get_req_files_from_client.
    DATA(lo_zip) = NEW cl_abap_zip( ).

    lo_zip->load( me->client_file_manager->read( iv_path = iv_filename ) ).

    LOOP AT lo_zip->files REFERENCE INTO DATA(lr_file).
      IF lr_file->name(1) NA mc_file_types-all.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING FIELD-SYMBOL(<fs_file>).
      <fs_file>-name = lr_file->name.

      CASE <fs_file>-name(1).
        WHEN mc_file_types-cofile.
          <fs_file>-file_type = mc_file_types-cofile.
        WHEN mc_file_types-data.
          <fs_file>-file_type = mc_file_types-data.
      ENDCASE.

      lo_zip->get( EXPORTING  name    = <fs_file>-name
                   IMPORTING  content = <fs_file>-content
                   EXCEPTIONS OTHERS  = 1 ).
      IF sy-subrc <> 0.
        lcx_tr_manager_message=>raise( iv_message    = lcx_tr_manager_message=>mc_messages-file_read_error
                                       iv_message_v1 = <fs_file>-name ).
      ENDIF.

      " Find request number
      IF ev_request IS INITIAL.
        SPLIT lr_file->name AT '.' INTO FINAL(lv_prefix) FINAL(lv_suffix).
        IF sy-subrc = 0.
          " K000000.XXX
          " result: XXXK000000
          ev_request = |{ lv_suffix }{ mc_file_types-cofile }{ lv_prefix+1 }|.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Validate archive
    IF NOT line_exists( rt_files[ file_type = mc_file_types-cofile ] ).
      lcx_tr_manager_message=>raise( iv_message = lcx_tr_manager_message=>mc_messages-header_file_missing ).
    ENDIF.

    IF NOT line_exists( rt_files[ file_type = mc_file_types-data ] ).
      lcx_tr_manager_message=>raise( iv_message = lcx_tr_manager_message=>mc_messages-content_file_missing ).
    ENDIF.
  ENDMETHOD.

  METHOD upload_request.
    DATA(lt_files) = get_req_files_from_client( EXPORTING iv_filename = iv_filename
                                                IMPORTING ev_request  = ev_request ).

    LOOP AT lt_files REFERENCE INTO DATA(lr_file).
      CASE lr_file->file_type.
        WHEN mc_file_types-cofile.
          server_file_manager->write(
              iv_path = |{ mc_paths-cofiles }{ me->server_file_manager->seperator }{ lr_file->name }|
              iv_data = lr_file->content ).
        WHEN mc_file_types-data.
          server_file_manager->write(
              iv_path = |{ mc_paths-data }{ me->server_file_manager->seperator }{ lr_file->name }|
              iv_data = lr_file->content ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD import_queue.
    CONSTANTS lc_auth_import_tr TYPE wbo_adm-function VALUE 'IMPS'.
    CONSTANTS lc_command        TYPE stpa-command     VALUE 'FILLCLIENT'.
    CONSTANTS lc_auth_append_tr TYPE wbo_adm-function VALUE 'TADD'.

    DATA lt_request_infos TYPE stms_wbo_requests.
    DATA lv_system        TYPE tmscsys-sysnam.
    DATA lv_client        TYPE sy-mandt.
    DATA lv_request       TYPE tmsbuffer-trkorr.
    DATA ls_request_info  LIKE LINE OF lt_request_infos.

    lv_system  = sy-sysid.
    lv_client  = sy-mandt.
    lv_request = iv_request.

    CALL FUNCTION 'TR_AUTHORITY_CHECK_ADMIN'
      EXPORTING
        iv_adminfunction = lc_auth_append_tr
      EXCEPTIONS
        OTHERS           = 1.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message    = lcx_tr_manager_message=>mc_messages-authorization_error
                                     iv_message_v1 = lc_auth_append_tr ).
    ENDIF.

    CALL FUNCTION 'TMS_UI_APPEND_TR_REQUEST'
      EXPORTING
        iv_system      = lv_system
        iv_request     = lv_request
        iv_expert_mode = 'X'
        iv_ctc_active  = 'X'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message = lcx_tr_manager_message=>mc_messages-tms_append_error ).
    ENDIF.

    CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_REQUEST'
      EXPORTING
        iv_request       = lv_request
        iv_target_system = lv_system
      IMPORTING
        et_request_infos = lt_request_infos
      EXCEPTIONS
        OTHERS           = 1.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message = lcx_tr_manager_message=>mc_messages-tms_append_error ).
    ENDIF.

    READ TABLE lt_request_infos INDEX 1 INTO ls_request_info.
    IF sy-subrc = 0.
      IF ls_request_info-e070-korrdev = mc_transport_category-cust.
        CALL FUNCTION 'TMS_MGR_MAINTAIN_TR_QUEUE'
          EXPORTING
            iv_command = lc_command
            iv_system  = lv_system
            iv_request = lv_request
            iv_tarcli  = lv_client
            iv_monitor = 'X'
            iv_verbose = 'X'
          EXCEPTIONS
            OTHERS     = 1.
        IF sy-subrc <> 0.
          lcx_tr_manager_message=>raise( iv_message = lcx_tr_manager_message=>mc_messages-tms_append_error ).
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'TR_AUTHORITY_CHECK_ADMIN'
      EXPORTING
        iv_adminfunction = lc_auth_import_tr
      EXCEPTIONS
        OTHERS           = 1.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message    = lcx_tr_manager_message=>mc_messages-authorization_error
                                     iv_message_v1 = lc_auth_import_tr ).
    ENDIF.

    CALL FUNCTION 'TMS_UI_IMPORT_TR_REQUEST'
      EXPORTING
        iv_system      = lv_system
        iv_request     = lv_request
        iv_tarcli      = lv_client
        iv_some_active = space
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      lcx_tr_manager_message=>raise( iv_message = lcx_tr_manager_message=>mc_messages-tms_import_error ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS initialization.
    METHODS start_of_selection.
    METHODS at_selection_screen_output.
    METHODS at_selection_screen_for_folder.
    METHODS at_selection_screen_for_file.
    METHODS at_selection_screen_for_trkorr.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF mc_modif_ids,
                 download TYPE c LENGTH 2 VALUE 'M1',
                 upload   TYPE c LENGTH 2 VALUE 'M2',
               END OF mc_modif_ids.
    CONSTANTS: BEGIN OF mc_texts,
                 request  TYPE string VALUE 'Request Number',
                 folder   TYPE string VALUE 'Folder Path',
                 file     TYPE string VALUE 'File Path',
                 t01      TYPE string VALUE 'Function Selection',
                 t02      TYPE string VALUE 'Condition Selection',
                 upload   TYPE string VALUE 'Upload',
                 download TYPE string VALUE 'Download',
                 import   TYPE string VALUE 'Import Request',
               END OF mc_texts.
    CONSTANTS: BEGIN OF mc_messages,
                 download_ok                TYPE string VALUE 'Download successful.',
                 upload_ok                  TYPE string VALUE 'Upload successful.',
                 import_ok                  TYPE string VALUE 'Import successful.',
                 request_must_be_provided   TYPE string VALUE 'Request number cannot be empty.',
                 file_name_must_be_provided TYPE string VALUE 'File name cannot be empty.',
                 folder_must_be_provided    TYPE string VALUE 'Folder name cannot be empty.',
               END OF mc_messages.

    DATA mo_tr_manager TYPE REF TO lcl_tr_manager.
ENDCLASS.


CLASS lcl_application IMPLEMENTATION.
  METHOD initialization.
    %_p_upload_%_app_%-text = mc_texts-upload.
    %_p_downld_%_app_%-text = mc_texts-download.
    %_p_trkorr_%_app_%-text = mc_texts-request.
    %_p_folder_%_app_%-text = mc_texts-folder.
    %_p_filenm_%_app_%-text = mc_texts-file.
    %_p_import_%_app_%-text = mc_texts-import.

    t01 = mc_texts-t01.
    t02 = mc_texts-t02.

    CONCATENATE sy-sysid 'K*' INTO p_trkorr.

    cl_gui_frontend_services=>get_desktop_directory( CHANGING   desktop_directory = p_folder
                                                     EXCEPTIONS OTHERS            = 1 ).

    cl_gui_frontend_services=>get_desktop_directory( CHANGING   desktop_directory = p_filenm
                                                     EXCEPTIONS OTHERS            = 1 ).

    TRY.
        mo_tr_manager = NEW #( ).
      CATCH cx_root INTO FINAL(lx_root).
        WRITE lx_root->get_text( ) COLOR COL_NEGATIVE.
    ENDTRY.
  ENDMETHOD.

  METHOD start_of_selection.
    TRY.
        CASE abap_true.
          WHEN p_downld.
            IF p_trkorr IS INITIAL.
              MESSAGE mc_messages-request_must_be_provided TYPE 'E'.
            ENDIF.
            IF p_folder IS INITIAL.
              MESSAGE mc_messages-folder_must_be_provided TYPE 'E'.
            ENDIF.
            mo_tr_manager->download_request( iv_request = p_trkorr
                                             iv_folder  = p_folder ).
            WRITE / mc_messages-download_ok COLOR COL_POSITIVE.
          WHEN p_upload.
            IF p_filenm IS INITIAL.
              MESSAGE mc_messages-file_name_must_be_provided TYPE 'E'.
            ENDIF.
            mo_tr_manager->upload_request( EXPORTING iv_filename = p_filenm
                                           IMPORTING ev_request  = FINAL(lv_request) ).
            WRITE / mc_messages-upload_ok COLOR COL_POSITIVE.
            IF p_import = abap_true.
              mo_tr_manager->import_queue( iv_request = lv_request ).
              WRITE / mc_messages-import_ok COLOR COL_POSITIVE.
            ENDIF.
        ENDCASE.
      CATCH cx_root INTO FINAL(lx_root).
        WRITE / lx_root->get_text( ) COLOR COL_NEGATIVE.
    ENDTRY.
  ENDMETHOD.

  METHOD at_selection_screen_output.
    LOOP AT SCREEN.
      CASE abap_true.
        WHEN p_downld.
          CASE screen-group1.
            WHEN mc_modif_ids-download.
              screen-required = 2.
            WHEN mc_modif_ids-upload.
              screen-active   = 0.
          ENDCASE.
        WHEN p_upload.
          CASE screen-group1.
            WHEN mc_modif_ids-download.
              screen-active   = 0.
            WHEN mc_modif_ids-upload.
              screen-required = 2.
          ENDCASE.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD at_selection_screen_for_file.
    DATA lt_filetable TYPE filetable.
    DATA lv_rc        TYPE sy-subrc.

    cl_gui_frontend_services=>file_open_dialog( EXPORTING  default_filename = '*.zip'
                                                           file_filter      = |*.zip|
                                                CHANGING   file_table       = lt_filetable
                                                           rc               = lv_rc
                                                EXCEPTIONS OTHERS           = 1 ).
    READ TABLE lt_filetable INTO p_filenm INDEX 1.
  ENDMETHOD.

  METHOD at_selection_screen_for_folder.
    cl_gui_frontend_services=>directory_browse( CHANGING   selected_folder = p_folder
                                                EXCEPTIONS OTHERS          = 1 ).
    CALL FUNCTION 'CONTROL_FLUSH'
      EXCEPTIONS
        OTHERS = 1.
  ENDMETHOD.

  METHOD at_selection_screen_for_trkorr.
    DATA ls_selection        TYPE trwbo_selection.
    DATA ls_selected_request TYPE trwbo_request_header.

    ls_selection-reqstatus = 'R'.

    CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
      EXPORTING
        iv_organizer_type   = ''
        is_selection        = ls_selection
      IMPORTING
        es_selected_request = ls_selected_request.

    p_trkorr = ls_selected_request-trkorr.
  ENDMETHOD.
ENDCLASS.

LOAD-OF-PROGRAM.
  DATA(lo_app) = NEW lcl_application( ).

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
