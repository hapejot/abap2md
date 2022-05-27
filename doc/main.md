# ZCL_ABAP2MD_MAIN

Main logic for ABAP to Markdown

This class extracts documentation from source code of other classes.
this source code has to be documented in a special fashion.
Also the standard document strings of the SE24 are included for documentation.


## GENERATE_MULTIPLE


    PUBLIC METHOD GENERATE_MULTIPLE

## GENERATE_SINGLE


    PUBLIC METHOD GENERATE_SINGLE
        IMPORTING
            IV_NAME                             TYPE SEOCLNAME
        RETURNING
            VALUE(RT_TEXT)                      TYPE STRINGTAB

## READ_OBJECT_INFO

Trying to read different object types with the passed name.
first try is as a class name.


    PRIVATE METHOD READ_OBJECT_INFO
        IMPORTING
            IV_NAME                             TYPE SEOCLNAME
        RETURNING
            VALUE(R_RESULT)                     TYPE LIF_INFO

**IV_NAME**
:  name of the object
