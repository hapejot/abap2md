#include <stdlib.h>
#include <stdio.h>
#include <sapnwrfc.h>
#include <assert.h>

void print_error( RFC_ERROR_INFO * err ) {
    printfU( cU( "%s: %s\n" ), err->key, err->message );
}


int mainU( int argc, SAP_UC ** argv ) {
    RFC_RC rc;
    RFC_ERROR_INFO err;
    RFC_CONNECTION_HANDLE cn;
    RFC_CONNECTION_PARAMETER params[1] = { {.name = cU( "dest" ),.value =
                                            argv[1]}
    };

    assert( argc == 2 );

    cn = RfcOpenConnection( params, 1, &err );
    if( 0 == cn )
        print_error( &err );
    assert( cn );

    RFC_FUNCTION_DESC_HANDLE fd =
            RfcGetFunctionDesc( cn, cU( "Z_ABAP2MD_GENERATE_MULTI" ), &err );
    if( 0 == fd )
        print_error( &err );
    assert( fd );

    RFC_FUNCTION_HANDLE fn = RfcCreateFunction( fd, &err );
    assert( fn );

    SAP_UC buf[4000];
    RFC_TABLE_HANDLE th;
    rc = RfcGetTable( fn, cU( "IT_NAMES" ), &th, &err );
    assert( 0 == rc );
    while( fgetsU( buf, 4000, stdin ) ) {
        while( !isprint( buf[strlenU( buf ) - 1] ) )
            buf[strlenU( buf ) - 1] = 0;
        RFC_STRUCTURE_HANDLE row = RfcAppendNewRow( th, &err );
        assert( 0 == rc );
        rc = RfcSetStringByIndex( row, 0, buf, strlenU( buf ), &err );
        assert( 0 == rc );


        fprintfU( stderr, cU( "In: %s\n" ), buf );
    }

    RfcInvoke( cn, fn, &err );
    assert( 0 == rc );


    rc = RfcGetTable( fn, cU( "ET_DOC" ), &th, &err );
    assert( 0 == rc );

    unsigned row_cnt;

    rc = RfcGetRowCount( th, &row_cnt, &err );
    assert( 0 == rc );
    for( int i = 0; i < row_cnt; i++ ) {
        unsigned len;
        rc = RfcMoveTo( th, i, &err );
        assert( 0 == rc );
        rc = RfcGetStringByIndex( th, 0, buf, 4000, &len, &err );
        if( rc ) {
            print_error( &err );
        }
        assert( 0 == rc );
        printfU( cU( "%s\n" ), buf );
    }

    return 0;
}
