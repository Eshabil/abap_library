*&---------------------------------------------------------------------*
*& Report ZES_EGITIM4
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zes_egitim4.
TABLES : zes_t_islem.
DATA : gt_ogrenci TYPE TABLE OF zes_t_ogrencii,
       gs_ogrenci TYPE zes_t_ogrencii,
       gt_kitap   TYPE TABLE OF zes_t_kitap,
       gs_kitap   TYPE zes_t_kitap,
       gt_yazar   TYPE TABLE OF zes_t_yazar,
       gs_yazar   TYPE zes_t_yazar,
       gt_tur     TYPE TABLE OF zes_t_tur,
       gs_tur     TYPE zes_t_tur,
       gt_islem   TYPE TABLE OF zes_t_islem,
       gs_islem   TYPE zes_t_islem,
       gt_report  TYPE TABLE OF zes_s_islem,
       gs_report  TYPE zes_s_islem.

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gs_fieldcat TYPE slis_fieldcat_alv,
       gs_layout   TYPE slis_layout_alv.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS : s_ktpno FOR zes_t_islem-kitapno,
                 s_ogrno FOR zes_t_islem-ogrencino.
SELECTION-SCREEN END OF BLOCK b1.

LOAD-OF-PROGRAM.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.

  PERFORM f_get_data.

END-OF-SELECTION.

  PERFORM merge.
  PERFORM display.


*&---------------------------------------------------------------------*
*& Form MERGE
*&---------------------------------------------------------------------*
FORM merge .

  gs_layout-window_titlebar = 'İşlem structure'.
  gs_layout-lights_fieldname = 'LIGHT'.
  gs_layout-colwidth_optimize = abap_true.

  gs_fieldcat-fieldname = 'LIGHT'.
  gs_fieldcat-ddictxt = 'M'.

  APPEND gs_fieldcat TO gt_fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZES_S_ISLEM'
    CHANGING
      ct_fieldcat      = gt_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  sub_pf_status
*&---------------------------------------------------------------------*
FORM sub_pf_status USING rt_extab TYPE slis_t_extab.

*  data : ls_extab like LINE OF rt_extab.
*  ls_extab-fcode = '&KTPVR'.
*  APPEND ls_extab to rt_extab.

  SET PF-STATUS 'GUI_STATUS' EXCLUDING rt_extab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  user_command  INPUT
*&---------------------------------------------------------------------*
FORM user_command USING b_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CASE b_ucomm.
    WHEN '&KTPVR'.
      CALL SCREEN 0100 STARTING AT 5 5
                      ENDING AT 70 20.

    WHEN '&BACK' OR '&EXIT' OR '&CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
  rs_selfield-refresh = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'GUI_100'.
  SET TITLEBAR 'TITLE_100'.
  PERFORM f_set_prsno.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN '&KYDT'.
      PERFORM f_islem_kaydet.

    WHEN '&IPTAL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_ISLEM_KAYDET
*&---------------------------------------------------------------------*
FORM f_islem_kaydet .

  gs_islem-islemno = zes_t_islem-islemno.
  gs_islem-ogrencino = zes_t_islem-ogrencino.
  gs_islem-kitapno = zes_t_islem-kitapno.
  gs_islem-kitapalimtarihi = zes_t_islem-kitapalimtarihi.
  INSERT zes_t_islem FROM gs_islem.
  IF sy-subrc EQ 0.
    MESSAGE 'Başarı ile kaydedildi.' TYPE 'I'.
    CLEAR : gs_report.
    MOVE-CORRESPONDING gs_islem TO gs_report.
    SELECT SINGLE b~yazarno,
                  b~yazaradi,
                  c~turno,
                  c~turtanim,
                  a~kitapno,
                  a~kitapadi
                          FROM zes_t_kitap AS a
                          INNER JOIN  zes_t_yazar AS b ON b~yazarno = a~yazarno
                          INNER JOIN  zes_t_tur AS c ON c~turno = a~turno
                          INTO (@gs_report-yazarno, @gs_report-yazaradi, @gs_report-turno, @gs_report-turtanim,
                          @gs_report-kitapno, @gs_report-kitapadi)
                          WHERE a~kitapno = @gs_islem-kitapno.

    READ TABLE gt_ogrenci INTO gs_ogrenci WITH KEY ogrencino = gs_islem-ogrencino.
    IF sy-subrc = 0.
      CONCATENATE gs_ogrenci-ogrenciadi gs_ogrenci-ogrencisoyadi INTO gs_report-adisoyadi SEPARATED BY space.
    ENDIF.
    gs_report-light = 2.
    APPEND gs_report TO gt_report.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
FORM f_get_data .

  SELECT * INTO TABLE gt_ogrenci
  FROM zes_t_ogrencii.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_report
            FROM zes_t_islem AS i
            INNER JOIN  zes_t_kitap AS k ON i~kitapno = k~kitapno
            INNER JOIN  zes_t_yazar AS y ON k~yazarno = y~yazarno
            INNER JOIN  zes_t_tur AS t ON k~turno = t~turno
            WHERE i~kitapno IN s_ktpno
  AND i~ogrencino IN s_ogrno.

  LOOP AT gt_report INTO gs_report.

    IF gs_report-kitapteslimtarihi IS INITIAL.
      IF sy-datum - gs_report-kitapalimtarihi GE 20.
        gs_report-light = 1.
      ELSE.
        gs_report-light = 2.
      ENDIF.
    ELSE.
      gs_report-light = 3.
    ENDIF.

    READ TABLE gt_ogrenci INTO gs_ogrenci WITH KEY ogrencino = gs_report-ogrencino.
    IF sy-subrc = 0.
      CONCATENATE gs_ogrenci-ogrenciadi gs_ogrenci-ogrencisoyadi INTO gs_report-adisoyadi SEPARATED BY space.
      MODIFY gt_report FROM gs_report.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY
*&---------------------------------------------------------------------*
FORM display .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat              = gt_fieldcat
      is_layout                = gs_layout
    TABLES
      t_outtab                 = gt_report.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SET_PRSNO
*&---------------------------------------------------------------------*
FORM f_set_prsno.
  CLEAR : zes_t_islem.
  SELECT MAX( islemno ) FROM  zes_t_islem
                        INTO zes_t_islem-islemno.

  ADD 1 TO zes_t_islem-islemno.

ENDFORM.