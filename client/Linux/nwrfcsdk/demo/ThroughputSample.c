#include <stdlib.h>
#include <stdio.h>
#include "sapnwrfc.h"


static SAP_ULLONG executionTime;
static SAP_ULLONG serializationTime;
static SAP_ULLONG deserializationTime;
static SAP_ULLONG applicationTime;
static SAP_ULLONG numberOfCalls;
static SAP_ULLONG sentData;
static SAP_ULLONG receivedData;

static RFC_CONNECTION_HANDLE connection = NULL;

static RFC_THROUGHPUT_HANDLE throughput = NULL;


void errorHandling(RFC_RC rc, const SAP_UC* description, RFC_ERROR_INFO* errorInfo)
{
    printfU(cU("%s: %d\n"), description, rc);
    printfU(cU("%s: %s\n"), errorInfo->key, errorInfo->message);

    if (connection != NULL) 
        RfcCloseConnection(connection, errorInfo);
    if (throughput != NULL) 
        RfcDestroyThroughput(throughput, errorInfo);

    exit(1);
}

void getAllDataFromThroughput(void)
{
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo; 

    rc = RfcGetTotalTime(throughput, &executionTime, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error while retrieving data..."), &errorInfo);
    rc = RfcGetSerializationTime(throughput, &serializationTime, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error while retrieving data..."), &errorInfo);
    rc = RfcGetDeserializationTime(throughput, &deserializationTime, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error while retrieving data..."), &errorInfo);
    rc = RfcGetApplicationTime(throughput, &applicationTime, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error while retrieving data..."), &errorInfo);
    rc = RfcGetNumberOfCalls(throughput, &numberOfCalls, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error while retrieving data..."), &errorInfo);
    rc = RfcGetSentBytes(throughput, &sentData, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error while retrieving data..."), &errorInfo);
    rc = RfcGetReceivedBytes(throughput, &receivedData, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error while retrieving data..."), &errorInfo);
}

void printThroughputData(void)
{
    const unsigned integerWidth = 6;
    printfU(cU("So far %*") SAP_Fllu cU(" call(s) were made:\n"), 2, numberOfCalls);
    printfU(cU("It took overall           %*") SAP_Fllu cU(" milliseconds\n"), integerWidth, executionTime);
    printfU(cU("Serialization took        %*") SAP_Fllu cU(" milliseconds\n"), integerWidth, serializationTime);
    printfU(cU("Deserialization took      %*") SAP_Fllu cU(" milliseconds\n"), integerWidth, deserializationTime);
    printfU(cU("My application consumed   %*") SAP_Fllu cU(" milliseconds\n"), integerWidth, applicationTime);
    printfU(cU("Received from the network %*") SAP_Fllu cU(" bytes\n"), integerWidth, receivedData);
    printfU(cU("Sent over the network     %*") SAP_Fllu cU(" bytes\n\n\n"), integerWidth, sentData);
}

void fill_STFC_Performance(RFC_FUNCTION_HANDLE STFC_PERFORMANCE)
{
    RFC_ERROR_INFO errorInfo;
    const unsigned numberRows = 5000;
    const RFC_CHAR numberRowsAsChar[5] = iU("5000");
    RFC_STRUCTURE_HANDLE itab100row;
    RFC_TABLE_HANDLE itab1000;
    RFC_CHAR buffer[200] = iU("");
    RFC_CHAR numberBuffer[5] = iU("");
    unsigned i, bufferFilledUntil;
    const RFC_CHAR randomText[] = iU("Just some random text to increase the load. Our programm will have to serialize and deserialize all of this data!")
                                  iU("This might consume some time. And this is just line ");

    //Let's assume all setters will work ;-)
    RfcSetChars(STFC_PERFORMANCE, cU("LGIT1000"), numberRowsAsChar, 5, &errorInfo);
    RfcSetChars(STFC_PERFORMANCE, cU("LGET1000"), numberRowsAsChar, 5, &errorInfo);
    RfcGetTable(STFC_PERFORMANCE, cU("ITAB1000"), &itab1000, &errorInfo);

    for(i = 0; i < numberRows; i++)
    {
        memsetU(buffer, cU('\0'), sizeofU(buffer));
        memcpyU(buffer, randomText, sizeofU(randomText));
        memsetU(numberBuffer, cU('\0'), sizeofU(numberBuffer));
        sprintfU(numberBuffer, cU("%i"), i + 1);
        bufferFilledUntil = strlenU(buffer);
        strcpyU(buffer + bufferFilledUntil, numberBuffer); //what happens if this line is commented out
        bufferFilledUntil = strlenU(buffer);

        itab100row = RfcAppendNewRow(itab1000, &errorInfo);
        
        RfcSetChars(itab100row, cU("LINE1"), buffer, bufferFilledUntil, &errorInfo);
        RfcSetChars(itab100row, cU("LINE2"), buffer, bufferFilledUntil, &errorInfo);
        RfcSetChars(itab100row, cU("LINE3"), buffer, bufferFilledUntil, &errorInfo);
        RfcSetChars(itab100row, cU("LINE4"), buffer, bufferFilledUntil, &errorInfo);
        RfcSetChars(itab100row, cU("LINE5"), buffer, bufferFilledUntil, &errorInfo);
    }
}

void exampleWithSimplePing(void)
{
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;

    rc = RfcPing(connection, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error during ping"), &errorInfo);
    printfU(cU("...and done!\n\n"));

    getAllDataFromThroughput();
    printThroughputData();
}

void exampleWithSomeMetaDataLookUp(void)
{
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_FUNCTION_DESC_HANDLE functionDesc = NULL;

    printfU(cU("Resetting throughput..  "));
    rc = RfcResetThroughput(throughput, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error during reset"), &errorInfo);

    functionDesc = RfcGetFunctionDesc(connection, cU("BAPI_FLIGHT_GETLIST"), &errorInfo);
    if (functionDesc == NULL)
        errorHandling(rc, cU("Error in repository lookup"), &errorInfo);
    functionDesc = RfcGetFunctionDesc(connection, cU("BAPI_FLIGHT_GETDETAIL"), &errorInfo);
    if (functionDesc == NULL)
        errorHandling(rc, cU("Error in repository lookup"), &errorInfo);
    functionDesc = RfcGetFunctionDesc(connection, cU("BAPI_TRANSACTION_COMMIT"), &errorInfo);
    if (functionDesc == NULL)
        errorHandling(rc, cU("Error in repository lookup"), &errorInfo);
    functionDesc = RfcGetFunctionDesc(connection, cU("BAPI_FLBOOKING_CREATEFROMDATA"), &errorInfo);
    if (functionDesc == NULL)
        errorHandling(rc, cU("Error in repository lookup"), &errorInfo);
    printfU(cU("  ..and invoke is done!\n\n"));

    getAllDataFromThroughput();
    printThroughputData();
}


void exampleWithSomeLoad(void)
{
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    RFC_FUNCTION_DESC_HANDLE STFC_PERFORMANCE_DESC = NULL;
    RFC_FUNCTION_HANDLE STFC_PERFORMANCE = NULL;

    STFC_PERFORMANCE_DESC = RfcGetFunctionDesc(connection, cU("STFC_PERFORMANCE"), &errorInfo);
    if (STFC_PERFORMANCE_DESC == NULL)
        errorHandling(rc, cU("Error in repository lookup"), &errorInfo);

    STFC_PERFORMANCE = RfcCreateFunction(STFC_PERFORMANCE_DESC, &errorInfo);
    if (STFC_PERFORMANCE == NULL)
        errorHandling(rc, cU("Error during function creation"), &errorInfo);

    printfU(cU("Resetting throughput..  "));
    rc = RfcResetThroughput(throughput, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error during reset"), &errorInfo);

    fill_STFC_Performance(STFC_PERFORMANCE);

    rc = RfcInvoke(connection, STFC_PERFORMANCE, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error during invoke"), &errorInfo);
    printfU(cU("  ..and invoke is done!\n\n"));

    getAllDataFromThroughput();
    printThroughputData();
}


int mainU(int argc, SAP_UC** argv)
{
    // command options: hostname sysnr user passwd client
    RFC_RC rc = RFC_OK;
    RFC_CONNECTION_PARAMETER loginParams[9];
    RFC_ERROR_INFO errorInfo;

    loginParams[0].name = cU("ashost");	                               loginParams[0].value = argc > 1 ? argv[1] : cU("hostname");
    loginParams[1].name = cU("sysnr");	                               loginParams[1].value = argc > 2 ? argv[2] : cU("50");
    loginParams[2].name = cU("user");	                               loginParams[2].value = argc > 3 ? argv[3] : cU("user");
    loginParams[3].name = cU("passwd");	                               loginParams[3].value = argc > 4 ? argv[4] : cU("******");
    loginParams[4].name = cU("client");	                               loginParams[4].value = argc > 5 ? argv[5] : cU("800");
    loginParams[5].name = cU("lang");                                  loginParams[5].value = cU("EN");
    loginParams[6].name = cU("use_repository_roundtrip_optimization"); loginParams[6].value = cU("0"); //also try "1"
    loginParams[7].name = cU("serialization_format");                  loginParams[7].value = cU("cb_serialization"); //also try "classic_serialization"
    loginParams[8].name = cU("compression_type");                      loginParams[8].value = cU("lan"); // also try "wan"


    connection = RfcOpenConnection(loginParams, 9, &errorInfo);
    if (connection == NULL) 
        errorHandling(rc, cU("Error during logon"), &errorInfo);
    printfU(cU("Successfully logged in\n\n"));

    printfU(cU("Creating throughput..\n"));
    throughput = RfcCreateThroughput(&errorInfo);
    printfU(cU(" ..and attaching it.. \n"));

    rc = RfcSetThroughputOnConnection(connection, throughput, &errorInfo);
    if (rc != RFC_OK) 
        errorHandling(rc, cU("Error while setting throughput"), &errorInfo);
    printfU(cU(" ..done!\n"));


    printfU(cU("Let's do a simple RFC_PING\n"));
    exampleWithSimplePing();

    printfU(cU("Let's monitor some metadata lookups\n\n"));
    exampleWithSomeMetaDataLookUp();

    printfU(cU("Let's monitor a call with lots of data\n\n"));
    exampleWithSomeLoad();

    printfU(cU("Let's do another RFC_PING\n"));
    exampleWithSimplePing();


    printfU(cU("Removing throughput from connection\n\n"));
    rc = RfcRemoveThroughputFromConnection(connection, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(rc, cU("Error while removing throughput"), &errorInfo);


    //throughput does no longer collect data
    printfU(cU("Let's do one last RFC_PING\n"));
    exampleWithSimplePing();


    rc = RfcDestroyThroughput(throughput, &errorInfo);
    rc = RfcCloseConnection(connection, &errorInfo);

    return 0;
}