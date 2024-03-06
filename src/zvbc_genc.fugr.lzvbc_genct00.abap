*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVBC_GENC.......................................*
TABLES: ZVBC_GENC, *ZVBC_GENC. "view work areas
CONTROLS: TCTRL_ZVBC_GENC
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVBC_GENC. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVBC_GENC.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVBC_GENC_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVBC_GENC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVBC_GENC_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVBC_GENC_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVBC_GENC.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVBC_GENC_TOTAL.

*.........table declarations:.................................*
TABLES: ZTBC_GENC                      .
