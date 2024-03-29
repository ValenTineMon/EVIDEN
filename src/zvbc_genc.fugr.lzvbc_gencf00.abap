*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZVBC_GENC.......................................*
FORM GET_DATA_ZVBC_GENC.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZTBC_GENC WHERE
(VIM_WHERETAB) .
    CLEAR ZVBC_GENC .
ZVBC_GENC-MANDT =
ZTBC_GENC-MANDT .
ZVBC_GENC-REPID =
ZTBC_GENC-REPID .
ZVBC_GENC-PARAM =
ZTBC_GENC-PARAM .
ZVBC_GENC-PARAM_EXT =
ZTBC_GENC-PARAM_EXT .
ZVBC_GENC-SEQUENCE =
ZTBC_GENC-SEQUENCE .
ZVBC_GENC-PARAM_SIGN =
ZTBC_GENC-PARAM_SIGN .
ZVBC_GENC-PARAM_OPTION =
ZTBC_GENC-PARAM_OPTION .
ZVBC_GENC-VALUE_LOW =
ZTBC_GENC-VALUE_LOW .
ZVBC_GENC-VALUE_HIGH =
ZTBC_GENC-VALUE_HIGH .
ZVBC_GENC-VDESC =
ZTBC_GENC-VDESC .
ZVBC_GENC-CREATE_DATE =
ZTBC_GENC-CREATE_DATE .
ZVBC_GENC-CREATE_TIME =
ZTBC_GENC-CREATE_TIME .
ZVBC_GENC-CREATE_BY =
ZTBC_GENC-CREATE_BY .
ZVBC_GENC-CHANGE_DATE =
ZTBC_GENC-CHANGE_DATE .
ZVBC_GENC-CHANGE_TIME =
ZTBC_GENC-CHANGE_TIME .
ZVBC_GENC-CHANGE_BY =
ZTBC_GENC-CHANGE_BY .
<VIM_TOTAL_STRUC> = ZVBC_GENC.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZVBC_GENC .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZVBC_GENC.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZVBC_GENC-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZTBC_GENC WHERE
  REPID = ZVBC_GENC-REPID AND
  PARAM = ZVBC_GENC-PARAM AND
  PARAM_EXT = ZVBC_GENC-PARAM_EXT AND
  SEQUENCE = ZVBC_GENC-SEQUENCE .
    IF SY-SUBRC = 0.
    DELETE ZTBC_GENC .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZTBC_GENC WHERE
  REPID = ZVBC_GENC-REPID AND
  PARAM = ZVBC_GENC-PARAM AND
  PARAM_EXT = ZVBC_GENC-PARAM_EXT AND
  SEQUENCE = ZVBC_GENC-SEQUENCE .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZTBC_GENC.
    ENDIF.
ZTBC_GENC-MANDT =
ZVBC_GENC-MANDT .
ZTBC_GENC-REPID =
ZVBC_GENC-REPID .
ZTBC_GENC-PARAM =
ZVBC_GENC-PARAM .
ZTBC_GENC-PARAM_EXT =
ZVBC_GENC-PARAM_EXT .
ZTBC_GENC-SEQUENCE =
ZVBC_GENC-SEQUENCE .
ZTBC_GENC-PARAM_SIGN =
ZVBC_GENC-PARAM_SIGN .
ZTBC_GENC-PARAM_OPTION =
ZVBC_GENC-PARAM_OPTION .
ZTBC_GENC-VALUE_LOW =
ZVBC_GENC-VALUE_LOW .
ZTBC_GENC-VALUE_HIGH =
ZVBC_GENC-VALUE_HIGH .
ZTBC_GENC-VDESC =
ZVBC_GENC-VDESC .
ZTBC_GENC-CREATE_DATE =
ZVBC_GENC-CREATE_DATE .
ZTBC_GENC-CREATE_TIME =
ZVBC_GENC-CREATE_TIME .
ZTBC_GENC-CREATE_BY =
ZVBC_GENC-CREATE_BY .
ZTBC_GENC-CHANGE_DATE =
ZVBC_GENC-CHANGE_DATE .
ZTBC_GENC-CHANGE_TIME =
ZVBC_GENC-CHANGE_TIME .
ZTBC_GENC-CHANGE_BY =
ZVBC_GENC-CHANGE_BY .
    IF SY-SUBRC = 0.
    UPDATE ZTBC_GENC ##WARN_OK.
    ELSE.
    INSERT ZTBC_GENC .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZVBC_GENC-UPD_FLAG,
STATUS_ZVBC_GENC-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZVBC_GENC.
  SELECT SINGLE * FROM ZTBC_GENC WHERE
REPID = ZVBC_GENC-REPID AND
PARAM = ZVBC_GENC-PARAM AND
PARAM_EXT = ZVBC_GENC-PARAM_EXT AND
SEQUENCE = ZVBC_GENC-SEQUENCE .
ZVBC_GENC-MANDT =
ZTBC_GENC-MANDT .
ZVBC_GENC-REPID =
ZTBC_GENC-REPID .
ZVBC_GENC-PARAM =
ZTBC_GENC-PARAM .
ZVBC_GENC-PARAM_EXT =
ZTBC_GENC-PARAM_EXT .
ZVBC_GENC-SEQUENCE =
ZTBC_GENC-SEQUENCE .
ZVBC_GENC-PARAM_SIGN =
ZTBC_GENC-PARAM_SIGN .
ZVBC_GENC-PARAM_OPTION =
ZTBC_GENC-PARAM_OPTION .
ZVBC_GENC-VALUE_LOW =
ZTBC_GENC-VALUE_LOW .
ZVBC_GENC-VALUE_HIGH =
ZTBC_GENC-VALUE_HIGH .
ZVBC_GENC-VDESC =
ZTBC_GENC-VDESC .
ZVBC_GENC-CREATE_DATE =
ZTBC_GENC-CREATE_DATE .
ZVBC_GENC-CREATE_TIME =
ZTBC_GENC-CREATE_TIME .
ZVBC_GENC-CREATE_BY =
ZTBC_GENC-CREATE_BY .
ZVBC_GENC-CHANGE_DATE =
ZTBC_GENC-CHANGE_DATE .
ZVBC_GENC-CHANGE_TIME =
ZTBC_GENC-CHANGE_TIME .
ZVBC_GENC-CHANGE_BY =
ZTBC_GENC-CHANGE_BY .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZVBC_GENC USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZVBC_GENC-REPID TO
ZTBC_GENC-REPID .
MOVE ZVBC_GENC-PARAM TO
ZTBC_GENC-PARAM .
MOVE ZVBC_GENC-PARAM_EXT TO
ZTBC_GENC-PARAM_EXT .
MOVE ZVBC_GENC-SEQUENCE TO
ZTBC_GENC-SEQUENCE .
MOVE ZVBC_GENC-MANDT TO
ZTBC_GENC-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZTBC_GENC'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZTBC_GENC TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZTBC_GENC'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
