CLASS zcl_abap2md_local_file DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
             t_dir TYPE string.
    METHODS add_text
      IMPORTING
        i_doc TYPE stringtab.
    METHODS save IMPORTING i_path TYPE t_dir.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: m_data  TYPE xstring,
          m_solix TYPE solix_tab,
          m_size  TYPE i.
ENDCLASS.



CLASS zcl_abap2md_local_file IMPLEMENTATION.

  METHOD add_text.
    DATA: str  TYPE string,
          cp   TYPE abap_encod,
          size TYPE so_obj_len.
    TRY.
        cp = '4110'. " utf-8
        CONCATENATE LINES OF i_doc INTO str SEPARATED BY cl_abap_char_utilities=>newline.
        CALL METHOD cl_bcs_convert=>string_to_solix
          EXPORTING
            iv_string   = str
            iv_codepage = cp
*           iv_add_bom  =
          IMPORTING
            et_solix    = m_solix
            ev_size     = size.
        m_size = size.
      CATCH cx_bcs.
    ENDTRY.
  ENDMETHOD.


  METHOD save.

    " Save the file
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize = m_size
        filename     = i_path
        filetype     = 'BIN'
      CHANGING
        data_tab     = m_solix
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc > 0.
      MESSAGE i400(zabap2md) WITH i_path.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
