*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZABAP2MD_DOCELEM................................*
DATA:  BEGIN OF STATUS_ZABAP2MD_DOCELEM              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZABAP2MD_DOCELEM              .
CONTROLS: TCTRL_ZABAP2MD_DOCELEM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZABAP2MD_DOCELEM              .
TABLES: ZABAP2MD_DOCELEM               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
