REPORT x.
*
*CLASS application DEFINITION DEFERRED.
*CLASS local_class DEFINITION DEFERRED.
*
*TYPES:
*  ty_out  TYPE zca_s_quermesse_prioridades,
*  tab_out TYPE zca_t_quermesse_prioridades.
*
*
*DATA:
*  application       TYPE REF TO application,
*  container         TYPE REF TO cl_gui_docking_container,
*  grid              TYPE REF TO cl_gui_alv_grid,
*  grid_behaviour    TYPE REF TO cl_dragdrop,
*  gt_outtab         TYPE tab_out,
*  gt_outtab_display TYPE tab_out,
*  gs_layout         TYPE lvc_s_layo.
*
*CLASS application DEFINITION.
*
*  PUBLIC SECTION.
*
*    METHODS handle_grid_drag
*        FOR EVENT ondrag OF cl_gui_alv_grid
*      IMPORTING
*        es_row_no
*        e_column
*        e_dragdropobj.
*
*    METHODS handle_grid_drop
*        FOR EVENT ondrop OF cl_gui_alv_grid
*      IMPORTING
*        e_row
*        e_column
*        e_dragdropobj .
*
*    METHODS handle_grid_drop_complete
*        FOR EVENT ondropcomplete OF cl_gui_alv_grid
*      IMPORTING
*        e_row
*        e_column
*        e_dragdropobj.
*
*  PRIVATE SECTION .
*
*    METHODS update_table
*      IMPORTING
*        im_old_index TYPE lvc_index
*        im_new_index TYPE lvc_index
*        im_line      TYPE zca_s_quermesse_prioridades
*      CHANGING
*        ch_table     TYPE zca_t_quermesse_prioridades .
*
*ENDCLASS.
*
*
*CLASS drag_drop_object DEFINITION.
*
*  PUBLIC SECTION.
*    DATA:
*      gs_line  TYPE ty_out,
*      gv_tabix TYPE lvc_index.
*
*ENDCLASS.
*
*
*CLASS application IMPLEMENTATION.
*
*  METHOD handle_grid_drag.
*
*    DATA:
*      data_object TYPE REF TO drag_drop_object .
*
*    data_object = NEW #( ).
*    IF ( data_object IS NOT BOUND ) .
*      RETURN .
*    ENDIF .
*
*    data_object->gv_tabix = es_row_no-row_id.
*
*    READ TABLE gt_outtab_display INTO data_object->gs_line INDEX
*    es_row_no-row_id.
*
*    e_dragdropobj->object = data_object.
*
*  ENDMETHOD.
*
*  METHOD handle_grid_drop.
*
*    DATA data_object TYPE REF TO drag_drop_object.
*
*    CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.
*
*      data_object ?= e_dragdropobj->object.
*
*      IF e_dragdropobj->effect = cl_dragdrop=>move.
*
*        me->update_table( EXPORTING im_old_index = data_object->gv_tabix
*                                    im_new_index = e_row-index
*                                    im_line      = data_object->gs_line
*                          CHANGING  ch_table     = gt_outtab_display ).
*      ENDIF.
*
*    ENDCATCH.
*
*  ENDMETHOD.
*
*  METHOD handle_grid_drop_complete.
*
*    grid->refresh_table_display( ).
*
*    IF sy-subrc <> 0.
*      e_dragdropobj->abort( ).
*    ENDIF.
*
*  ENDMETHOD.
*
*
*  METHOD update_table .
*
*    IF ( im_old_index IS INITIAL ) OR
*      ( im_new_index IS INITIAL ) .
*      RETURN .
*    ENDIF .
*
*    IF lines( ch_table ) EQ 0 .
*      RETURN .
*    ENDIF .
*
*    " Remove old one
*    DELETE ch_table INDEX im_old_index .
*    ASSIGN ch_table[ im_old_index ] TO FIELD-SYMBOL(<fs_old_position>).
*    IF ( <fs_old_position> IS ASSIGNED ) .
*      <fs_old_position>-data_edicao = sy-datum .
*      <fs_old_position>-hora_edicao = sy-uzeit.
*      <fs_old_position>-user_edicao = sy-uname.
**      <fs_old_position>-nome_edicao = local_class=>get_user_name( sy-uname ).
*    ENDIF .
*
*    " Insert new
*    DATA(lv_new_line) = im_line .
*    lv_new_line-data_edicao = sy-datum .
*    lv_new_line-hora_edicao = sy-uzeit.
*    lv_new_line-user_edicao = sy-uname.
**    lv_new_line-user_edicao = sy-uname.
*    INSERT im_line INTO ch_table INDEX im_new_index.
*
*    " Update index
*    ch_table = VALUE zca_t_quermesse_prioridades(
*      LET lt_temp = ch_table
*      IN
*      FOR l IN lt_temp
*        ( prioridade_new = ( lines( ch_table ) + 1 )
*          seq_nr         = l-seq_nr
*          prioridade     = l-prioridade
*          inc            = l-inc
*          descricao_oc   = l-descricao_oc
*          label_oc       = l-label_oc
*          bc             = l-bc
*          data_edicao    = l-data_edicao
*          hora_edicao    = l-hora_edicao
*          user_edicao    = l-user_edicao
**          nome_edicao    = local_class=>get_user_name( l-user_edicao )
*          estat          = l-estat )
*    ).
*
*  ENDMETHOD .
*
*ENDCLASS.
*
*
*CLASS local_class DEFINITION.
*
*  PUBLIC SECTION.
*
*    TYPES tab_out TYPE STANDARD TABLE OF zca_tquermes_pri WITH DEFAULT KEY.
*
*    "! <p class="shorttext synchronized" lang="pt">Busca os dados</p>
*    METHODS search.
*
*    "! <p class="shorttext synchronized" lang="pt">Configuraçoes iniciais</p>
*    METHODS prepare_data
*      RETURNING VALUE(result) TYPE zca_t_quermesse_prioridades.
*
*    "! <p class="shorttext synchronized" lang="pt">TODO</p>
*    METHODS display_alv.
*
*    "! <p class="shorttext synchronized" lang="pt">Set Status e config iniciais</p>
*    CLASS-METHODS status_0100 .
*
*    "! <p class="shorttext synchronized" lang="pt">Mantem os comandos de tela</p>
*    CLASS-METHODS user_command .
*
*    "! <p class="shorttext synchronized" lang="pt">Prepara o fieldcat de exibição</p>
*    CLASS-METHODS get_user_name
*      IMPORTING
*        !im_data      TYPE xubname
*      RETURNING
*        VALUE(result) TYPE bapiaddr3-fullname .
*
*  PRIVATE SECTION.
*
*    DATA out_tab TYPE zca_t_quermesse_prioridades.
*
*    "! <p class="shorttext synchronized" lang="pt">Criar controles</p>
*    CLASS-METHODS create_controls .
*
*    "! <p class="shorttext synchronized" lang="pt">Manter atividades</p>
*    CLASS-METHODS build_and_assign_handler .
*
*    "! <p class="shorttext synchronized" lang="pt">Prepara o fieldcat de exibição</p>
*    CLASS-METHODS get_fieldcatalog
*      RETURNING
*        VALUE(result) TYPE lvc_t_fcat .
*
*    "! <p class="shorttext synchronized" lang="pt">Salva a lista e atualizações aplicadas</p>
*    CLASS-METHODS save_list.
*
*ENDCLASS.
*
*
*CLASS local_class IMPLEMENTATION.
*
*  METHOD search.
*
*    REFRESH out_tab.
*
*    SELECT * FROM zca_tquermes_pri
*      INTO CORRESPONDING FIELDS OF TABLE out_tab.
*
*  ENDMETHOD.
*
*  METHOD prepare_data.
*
*    result = VALUE zca_t_quermesse_prioridades(
*      FOR l IN me->out_tab
*        (  prioridade_new  = ( lines( result ) + 1 )
*          seq_nr       = l-seq_nr
*          prioridade   = ( lines( result ) + 1 )
*          inc          = l-inc
*          descricao_oc = l-descricao_oc
*          label_oc     = l-label_oc
*          bc           = l-bc
*          data_edicao  = l-data_edicao
*          hora_edicao  = l-hora_edicao
*          user_edicao  = l-user_edicao
*          nome_edicao  = local_class=>get_user_name( l-user_edicao )
*          estat        = l-estat )
*    ).
*
*  ENDMETHOD.
*
*  METHOD display_alv.
*  ENDMETHOD.
*
*
*  METHOD status_0100 .
*
**   SET PF-STATUS 'STATUS_0100' OF PROGRAM 'BCALV_TEST_DRAG_DROP_02'.
*    SET PF-STATUS 'STATUS_0100' .
*    SET TITLEBAR  'STATUS_0100' OF PROGRAM 'BCALV_TEST_DRAG_DROP_02'.
*
*    IF grid IS INITIAL.
*      local_class=>create_controls( ).
*    ENDIF.
*
*  ENDMETHOD .
*
*
*  METHOD create_controls .
*
*    DATA:
*      it_fieldcatalog TYPE lvc_t_fcat .
*
*    container = NEW #( dynnr     = '100'
*                       extension = 312
*                       side      = cl_gui_docking_container=>dock_at_top ).
*
*    grid = NEW #( i_parent = container ).
*
*    application = NEW #( ).
*
*    SET HANDLER application->handle_grid_drag FOR grid.
*    SET HANDLER application->handle_grid_drop FOR grid.
*    SET HANDLER application->handle_grid_drop_complete FOR grid.
*
*    local_class=>build_and_assign_handler( ).
*
*    IF ( lines( gt_outtab_display ) = 0 ).
*      APPEND LINES OF gt_outtab TO gt_outtab_display.
*    ENDIF.
*
*    it_fieldcatalog = get_fieldcatalog( ) .
*    gs_layout-cwidth_opt = abap_on .
*    gs_layout-zebra      = abap_on .
*
*    grid->set_table_for_first_display( EXPORTING i_structure_name = 'ZCA_S_QUERMESSE_PRIORIDADES'
*                                                 is_layout        = gs_layout
*                                       CHANGING  it_outtab        = gt_outtab_display
*                                                 it_fieldcatalog  = it_fieldcatalog ) .
*
*  ENDMETHOD.
*
*
*  METHOD build_and_assign_handler .
*
*    DATA:
*      effect_move TYPE i,
*      handle_grid TYPE i.
*
*    grid_behaviour = NEW #( ).
*    effect_move = cl_dragdrop=>move.
*
*    grid_behaviour->add( flavor         = 'LINE'
*                         dragsrc        = 'X'
*                         droptarget     = 'X'
*                         effect_in_ctrl = effect_move ).
*
*    grid_behaviour->get_handle( IMPORTING handle = handle_grid ).
*
*    gs_layout-s_dragdrop-row_ddid = handle_grid.
*
*  ENDMETHOD .
*
*
*  METHOD get_fieldcatalog .
*
*    result = VALUE lvc_t_fcat(
*      ( fieldname      = 'PRIORIDADE_NEW'
*        fix_column     = abap_on
*        scrtext_l      = 'Nova Prior.'
*        scrtext_m      = 'Nova Prior.'
*        scrtext_s      = 'Nova Prior.'
*        outputlen      = 10 )
*      ( fieldname      = 'SEQ_NR'
*        no_out         = abap_on )
*      ( fieldname      = 'PRIORIDADE'
*        fix_column     = abap_on
*        scrtext_l      = 'Prior.'
*        scrtext_m      = 'Prior.'
*        scrtext_s      = 'Prior.' )
*      ( fieldname      = 'BC'
*        no_out         = abap_on )
*    ).
*
*  ENDMETHOD .
*
*
*  METHOD save_list .
*  ENDMETHOD .
*
*
*  METHOD user_command .
*
*    BREAK-POINT .
*
*    CASE sy-ucomm .
*
*      WHEN 'EXIT' OR 'BACK'.
*
*        IF container IS NOT INITIAL.
*
*          container->free( EXCEPTIONS cntl_system_error = 1
*                                      cntl_error        = 2 ).
*
*          cl_gui_cfw=>flush( EXCEPTIONS cntl_system_error = 1
*                                        cntl_error        = 2 ).
*
*          LEAVE PROGRAM.
*
*        ENDIF.
*
*      WHEN 'SAVE' .
*
*        save_list( ).
*
*      WHEN OTHERS .
*
*    ENDCASE.
*
*  ENDMETHOD .
*
*
*  METHOD get_user_name.
*
*    DATA:
*      ls_address TYPE bapiaddr3,
*      lt_return  TYPE bapiret2_t.
*
*    IF im_data IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*      EXPORTING username = im_data
*      IMPORTING address  = ls_address
*      TABLES    return   = lt_return.
*
*    IF line_exists( lt_return[ type = if_xo_const_message=>error ] ).
*      RETURN.
*    ENDIF.
*
*    result = ls_address-fullname.
*
*  ENDMETHOD.
*
*ENDCLASS.
**&---------------------------------------------------------------------*
**&      Module  STATUS_0100  OUTPUT
**&---------------------------------------------------------------------*
*MODULE status_0100 OUTPUT.
*
*  local_class=>status_0100( ) .
*
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_0100  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE user_command_0100 INPUT.
*
*  local_class=>user_command( ) .
*
*ENDMODULE.
*
*
*INITIALIZATION.
*
*START-OF-SELECTION.
*
*  DATA(go_alv_salv) = NEW local_class( ).
*
*  IF ( go_alv_salv IS BOUND ).
*
*    go_alv_salv->search( ).
*
*    gt_outtab = go_alv_salv->prepare_data( ).
*
*  ENDIF.
*
*
*end-OF-SELECTION .
*  IF ( go_alv_salv IS BOUND ).
*    SET SCREEN 100.
*  ENDIF.



CONSTANTS: c_fruits     TYPE string VALUE 'Fruits',
           c_vegetables TYPE string VALUE 'Vegetables',
           c_meat       TYPE string VALUE 'Meat',
           c_fish       TYPE string VALUE 'Fish'.

DATA: lo_excel                TYPE REF TO zcl_excel,
      lo_worksheet            TYPE REF TO zcl_excel_worksheet,
      lo_range                TYPE REF TO zcl_excel_range,
      lo_data_validation      TYPE REF TO zcl_excel_data_validation.

DATA: row TYPE zexcel_cell_row.


DATA: lv_title          TYPE zexcel_sheet_title.


CONSTANTS: gc_save_file_name TYPE string VALUE '00_DataValidation.xlsx'.
INCLUDE zdemo_excel_outputopt_incl.

PARAMETERS: p_sbook TYPE flag.


START-OF-SELECTION.

  " Creates active sheet
  CREATE OBJECT lo_excel.

  " Get active sheet
  lo_worksheet        = lo_excel->get_active_worksheet( ).
  lv_title = 'Data Validation'.
  lo_worksheet->set_title( lv_title ).
  " Set values for dropdown
  lo_worksheet->set_cell( ip_row = 2 ip_column = 'A' ip_value = c_fish ).
  lo_worksheet->set_cell( ip_row = 4 ip_column = 'A' ip_value = 'Anchovy' ).
  lo_worksheet->set_cell( ip_row = 5 ip_column = 'A' ip_value = 'Carp' ).
  lo_worksheet->set_cell( ip_row = 6 ip_column = 'A' ip_value = 'Catfish' ).
  lo_worksheet->set_cell( ip_row = 7 ip_column = 'A' ip_value = 'Cod' ).
  lo_worksheet->set_cell( ip_row = 8 ip_column = 'A' ip_value = 'Eel' ).
  lo_worksheet->set_cell( ip_row = 9 ip_column = 'A' ip_value = 'Haddock' ).

  lo_range            = lo_excel->add_new_range( ).
  lo_range->name      = c_fish.
  lo_range->set_value( ip_sheet_name    = lv_title
                       ip_start_column  = 'A'
                       ip_start_row     = 4
                       ip_stop_column   = 'A'
                       ip_stop_row      = 9 ).

  lo_worksheet->set_cell( ip_row = 2 ip_column = 'B' ip_value = c_meat ).
  lo_worksheet->set_cell( ip_row = 4 ip_column = 'B' ip_value = 'Pork' ).
  lo_worksheet->set_cell( ip_row = 5 ip_column = 'B' ip_value = 'Beef' ).
  lo_worksheet->set_cell( ip_row = 6 ip_column = 'B' ip_value = 'Chicken' ).
  lo_worksheet->set_cell( ip_row = 7 ip_column = 'B' ip_value = 'Turkey' ).

  lo_range            = lo_excel->add_new_range( ).
  lo_range->name      = c_meat.
  lo_range->set_value( ip_sheet_name    = lv_title
                       ip_start_column  = 'B'
                       ip_start_row     = 4
                       ip_stop_column   = 'B'
                       ip_stop_row      = 7 ).

  lo_worksheet->set_cell( ip_row = 2 ip_column = 'C' ip_value = c_fruits ).
  lo_worksheet->set_cell( ip_row = 4 ip_column = 'C' ip_value = 'Apple' ).
  lo_worksheet->set_cell( ip_row = 5 ip_column = 'C' ip_value = 'Banana' ).
  lo_worksheet->set_cell( ip_row = 6 ip_column = 'C' ip_value = 'Blueberry' ).
  lo_worksheet->set_cell( ip_row = 7 ip_column = 'C' ip_value = 'Ananas' ).
  lo_worksheet->set_cell( ip_row = 8 ip_column = 'C' ip_value = 'Grapes' ).

  lo_range            = lo_excel->add_new_range( ).
  lo_range->name      = c_fruits.
  lo_range->set_value( ip_sheet_name    = lv_title
                       ip_start_column  = 'C'
                       ip_start_row     = 4
                       ip_stop_column   = 'C'
                       ip_stop_row      = 8 ).

  lo_worksheet->set_cell( ip_row = 2 ip_column = 'D' ip_value = c_vegetables ).
  lo_worksheet->set_cell( ip_row = 4 ip_column = 'D' ip_value = 'Cucumber' ).
  lo_worksheet->set_cell( ip_row = 5 ip_column = 'D' ip_value = 'Sweet pepper ' ).
  lo_worksheet->set_cell( ip_row = 6 ip_column = 'D' ip_value = 'Lettuce' ).

  lo_range            = lo_excel->add_new_range( ).
  lo_range->name      = c_vegetables.
  lo_range->set_value( ip_sheet_name    = lv_title
                       ip_start_column  = 'D'
                       ip_start_row     = 4
                       ip_stop_column   = 'D'
                       ip_stop_row      = 6 ).

  lo_worksheet        = lo_excel->add_new_worksheet( ).
  lv_title = 'Table with Data Validation'.
  lo_worksheet->set_title( lv_title ).

  " Maximum Text length
  lo_worksheet->set_cell(  ip_row = 1 ip_column = 'A' ip_value = 'Validate Maximum Text length of <= 10 in Cell A2:' ).
  lo_worksheet->set_cell(  ip_row = 2 ip_column = 'A' ip_value = 'abcdefghij' ).
  lo_data_validation              = lo_worksheet->add_new_data_validation( ).
  lo_data_validation->type        = zcl_excel_data_validation=>c_type_textlength.
  lo_data_validation->operator    = zcl_excel_data_validation=>c_operator_lessthanorequal.
  lo_data_validation->formula1    = 10.
  lo_data_validation->cell_row    = 2.
  lo_data_validation->cell_column = 'A'.

  " Integer Value between 1 and 10
  lo_worksheet->set_cell(  ip_row = 4 ip_column = 'A' ip_value = 'Validate Integer Value between 1 and 10 in Cell A5:' ).
  lo_worksheet->set_cell(  ip_row = 5 ip_column = 'A' ip_value = '5' ).
  lo_data_validation              = lo_worksheet->add_new_data_validation( ).
  lo_data_validation->type        = zcl_excel_data_validation=>c_type_whole.
  lo_data_validation->operator    = zcl_excel_data_validation=>c_operator_between.
  lo_data_validation->formula1    = 1.
  lo_data_validation->formula2    = 10.
  lo_data_validation->prompttitle = 'Range'.
  lo_data_validation->prompt      = 'Enter a value between 1 and 10'.
  lo_data_validation->errortitle  = 'Error'.
  lo_data_validation->error       = 'You have entered a wrong value. Please use only numbers between 1 and 10.'.
  lo_data_validation->cell_row    = 5.
  lo_data_validation->cell_column = 'A'.

  " Evaluation by Formula from issue #161
  lo_worksheet->set_cell(  ip_row = 7 ip_column = 'A' ip_value = 'Validate if B8 contains a "-":' ).
  lo_worksheet->set_cell(  ip_row = 8 ip_column = 'A' ip_value = 'Text' ).
  lo_worksheet->set_cell(  ip_row = 8 ip_column = 'B' ip_value = '-' ).
  lo_data_validation              = lo_worksheet->add_new_data_validation( ).
  lo_data_validation->type        = zcl_excel_data_validation=>c_type_custom.
  lo_data_validation->formula1    = '"IF(B8<>"""";INDIRECT(LEFT(B8;SEARCH(""-"";B8;1)));EMPTY)"'.
  lo_data_validation->cell_row    = 8.
  lo_data_validation->cell_column = 'A'.

  " There was an error when data validation was combined with cell merges this should test that:
  lo_worksheet->set_cell(  ip_row = 10 ip_column = 'A' ip_value = 'Demo for data validation with a dropdown list' ).
  lo_worksheet->set_merge( ip_row = 10 ip_column_start = 'A' ip_column_end = 'F' ).

  " Headlines
  lo_worksheet->set_cell( ip_row = 11 ip_column = 'A' ip_value = c_fruits ).
  lo_worksheet->set_cell( ip_row = 11 ip_column = 'B' ip_value = c_vegetables ).

  row = 12.
  WHILE row < 20. " Starting with 14500 the data validation is dropped 14000 are still ok
    " 1st validation
    lo_data_validation              = lo_worksheet->add_new_data_validation( ).
    lo_data_validation->type        = zcl_excel_data_validation=>c_type_list.
    lo_data_validation->formula1    = c_fruits.
    lo_data_validation->cell_row    = row.
    lo_data_validation->cell_column = 'A'.
    lo_worksheet->set_cell( ip_row = row ip_column = 'A' ip_value = 'Select a value' ).
                                                            " 2nd
    lo_data_validation              = lo_worksheet->add_new_data_validation( ).
    lo_data_validation->type        = zcl_excel_data_validation=>c_type_list.
    lo_data_validation->formula1    = c_vegetables.
    lo_data_validation->cell_row    = row.
    lo_data_validation->cell_column = 'B'.
    lo_worksheet->set_cell( ip_row = row ip_column = 'B' ip_value = 'Select a value' ).
                                                            " 3rd
    lo_data_validation              = lo_worksheet->add_new_data_validation( ).
    lo_data_validation->type        = zcl_excel_data_validation=>c_type_list.
    lo_data_validation->formula1    = c_meat.
    lo_data_validation->cell_row    = row.
    lo_data_validation->cell_column = 'C'.
    lo_worksheet->set_cell( ip_row = row ip_column = 'C' ip_value = 'Select a value' ).
                                                            " 4th
    lo_data_validation              = lo_worksheet->add_new_data_validation( ).
    lo_data_validation->type        = zcl_excel_data_validation=>c_type_list.
    lo_data_validation->formula1    = c_fish.
    lo_data_validation->cell_row    = row.
    lo_data_validation->cell_column = 'D'.
    lo_worksheet->set_cell( ip_row = row ip_column = 'D' ip_value = 'Select a value' ).
    " Increment row
    row = row + 1.
  ENDWHILE.

  IF p_sbook = abap_true.
    DATA: bookings type TABLE OF sbook.

    lo_worksheet        = lo_excel->add_new_worksheet( ).
    lv_title = 'SBOOK'.
    lo_worksheet->set_title( lv_title ).

    SELECT * from sbook INTO TABLE bookings UP TO 4000 ROWS.

    lo_worksheet->bind_table(
      EXPORTING
        ip_table          = bookings
*        it_field_catalog  =     " Table binding field catalog
*        is_table_settings =     " Excel table binding settings
*      IMPORTING
*        es_table_settings =     " Excel table binding settings
    ).
  ENDIF.


*** Create output
  lcl_output=>output( lo_excel ).