*----------------------------------------------------------------------*
* Confidential and Proprietary
* Copyright 2020,
* All Rights Reserved
*----------------------------------------------------------------------*
* Program Name  : MZBC_TABLE_MAINTAIN_ROUTINES
* Program Desc  : Include program for table maintenance
* T-code        : N/A
* WRICEF id     : N/A
* Start Date    : 08.07.2020
* Developer     : Wuthichai Limthongbai
* Functional    : N/A
* Tech. Design  : This is include program for assign change log data
*                 in table maintenance. It is designed to used with
*                 event 01 on table with include structure
*                 ZSTEC_CHANGELOG
*----------------------------------------------------------------------*
* Modification History
***********************************************************************
* Author        :
* Date          :
* Change Request:
* Search term   :
* Description   :
***********************************************************************

*----------------------------------------------------------------------*
*  Form f_assign_change_log
*----------------------------------------------------------------------*
*  Assign Change log information in table
*----------------------------------------------------------------------*
FORM f_assign_change_log.

  FIELD-SYMBOLS:
    <lfs_field>  TYPE  any.


  LOOP AT total.

    CHECK <action> EQ 'N' OR
          <action> EQ 'U' .

    ASSIGN COMPONENT 'CREATE_DATE' OF STRUCTURE <vim_total_struc> TO <lfs_field>.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

*   No Create data
    IF <action> EQ 'N' OR
       <lfs_field> IS INITIAL.
      <lfs_field> = sy-datum.
      ASSIGN COMPONENT 'CREATE_TIME' OF STRUCTURE <vim_total_struc> TO <lfs_field>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      <lfs_field> = sy-uzeit.
      ASSIGN COMPONENT 'CREATE_BY' OF STRUCTURE <vim_total_struc> TO <lfs_field>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      <lfs_field> = sy-uname.
*   Update data (Create data exist)
    ELSE.
      ASSIGN COMPONENT 'CHANGE_DATE' OF STRUCTURE <vim_total_struc> TO <lfs_field>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      <lfs_field> = sy-datum.
      ASSIGN COMPONENT 'CHANGE_TIME' OF STRUCTURE <vim_total_struc> TO <lfs_field>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      <lfs_field> = sy-uzeit.
      ASSIGN COMPONENT 'CHANGE_BY' OF STRUCTURE <vim_total_struc> TO <lfs_field>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      <lfs_field> = sy-uname.
    ENDIF.

    READ TABLE extract WITH KEY <vim_xtotal_key>.
    IF sy-subrc EQ 0.
      extract = total.
      MODIFY extract INDEX sy-tabix.
    ENDIF.

    IF total IS NOT INITIAL.
      MODIFY total.
    ENDIF.

  ENDLOOP.

ENDFORM.
