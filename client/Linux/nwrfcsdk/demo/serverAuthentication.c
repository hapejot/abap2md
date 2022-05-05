#include <stdlib.h>
#include <stdio.h>
#ifdef SAPonNT
#include <windows.h>
#else
#include <unistd.h>
#endif

#include "sapnwrfc.h"

void RfcSleep(unsigned milliSec)
{

#ifdef SAPonNT
    Sleep((DWORD)(milliSec));
#else
    usleep(static_cast<unsigned>(1000 * (milliSec)));
#endif
}


static int running = 1;

void errorHandling(const SAP_UC* description, RFC_ERROR_INFO* errorInfo)
{
    printfU(cU("%s : (%d) %s\n"), description, errorInfo->code, errorInfo->message);
    exit(1);
}

void loadCryptolib(const SAP_UC* const pathTolib)
{
    RFC_ERROR_INFO errorInfo;
    RFC_RC rc = RfcLoadCryptoLibrary(pathTolib, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(cU("Could not load cryptolib"), &errorInfo);
}

RFC_RC SAP_API ShutDownImplementation(RFC_CONNECTION_HANDLE rfcHandle, RFC_FUNCTION_HANDLE funcHandle, RFC_ERROR_INFO* errorInfo)
{
    running = 0;
    printfU(cU("Server will shutdown\n"));
    return RFC_OK;
}

void installShutdownModule(void)
{
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;

    RFC_FUNCTION_DESC_HANDLE funcDescHandle = RfcCreateFunctionDesc(cU("RFC_SERVER_SHUTDOWN"), &errorInfo);
    if (funcDescHandle == NULL)
        errorHandling(cU("Creation of shutdown module failed"), &errorInfo);

    rc = RfcInstallServerFunction(NULL, funcDescHandle, ShutDownImplementation, &errorInfo); // install globally as there is no SYSID with websocket
    if (rc != RFC_OK)
        errorHandling(cU("Error installing server function"), &errorInfo);
}

RFC_RC SAP_API OnAuthenticationCheck(RFC_ATTRIBUTES attributes, RFC_AUTHENTICATION_HANDLE handle, RFC_ERROR_INFO* errorInfo)
{
    RFC_RC rc = RFC_OK;
    RFC_AUTHENTICATION_TYPE type;
    rc = RfcGetAuthenticationType(handle, &type, errorInfo);
    unsigned length = 0;

    printfU(cU("Partner %s (system %s) "), attributes.partnerHost, attributes.sysId);
    switch (type)
    {
        case RFC_AUTH_NONE:
        {
            printfU(cU("did not provide authentication data.\n\n"));
            rc = RFC_AUTHENTICATION_FAILURE;
            break;
        }
        case RFC_AUTH_BASIC:
        {
            const SAP_UC* user = NULL;
            const SAP_UC* pw = NULL;
            rc = RfcGetAuthenticationUser(handle, &user, &length, errorInfo);
            rc = RfcGetAuthenticationPassword(handle, &pw, &length, errorInfo);
            printfU(cU("with basic authentication:\n"));
            printfU(cU("user %s, password %s\n\n"), user, pw);
            /*
            check basic auth credentials in user database
            if (credentialsInvalid)
            {
                fill error info with details
                rc = RFC_AUTHENTICATION_FAILURE;
            }
            */
            break;
        }
        case RFC_AUTH_X509:
        {
            const RFC_CERTIFICATE_DATA* data = NULL;
            rc = RfcGetAuthenticationCertificateData(handle, &data, errorInfo);
            printfU(cU("with x509 authentication:\n"));
            printfU(cU("cert: subject   %s\n"), data->subject);
            printfU(cU("cert: issuer    %s\n"), data->issuer);
            printfU(cU("cert: validTo   %") SAP_Fllu cU("\n"), data->validTo);
            printfU(cU("cert: validFrom %") SAP_Fllu cU("\n"), data->validFrom);
            printfU(cU("cert: signature %s\n\n"), data->signature);
            /*
            check certificate validity (expiration, trusted issuer, etc), check user mapping in user database, ...
            if (certificateInvalid || noUsermappingFound)
            {
                fill error info with details
                rc = RFC_AUTHENTICATION_FAILURE;
            }
            */
            break;
        }
        case RFC_AUTH_SSO:
        {
            const SAP_UC* ticket = NULL;
            rc = RfcGetAuthenticationAssertionTicket(handle, &ticket, &length, errorInfo);
            printfU(cU("with SSO authentication:\n"));
            printfU(cU("ticket %s\n\n"), ticket);
            /*
            check assertion ticket validity, extract user info
            if (ticketInvalid || userDoesNotExist)
            {
                fill error info with details
                rc = RFC_AUTHENTICATION_FAILURE;
            }
            */
            break;
        }
    }
    return rc;
}

void installAuthenticationHandler(void)
{
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    rc = RfcInstallAuthenticationCheckHandler(OnAuthenticationCheck, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(cU("Could not install authentication handler"), &errorInfo);
}

int mainU(int argc, SAP_UC** argv)
{
    RFC_RC rc = RFC_OK;
    RFC_ERROR_INFO errorInfo;
    if (argc > 1)
        loadCryptolib(argv[1]);

    RFC_CONNECTION_PARAMETER loginParams[1];
    // DEFAULT
    // TLS_SAPCRYPTOLIB=\absolute\path\to\sapcrypto.dll //libsapcrypto.so
    // DEST=SERVER_SAMPLE
    // WSPORT=44318
    // USE_TLS=1
    // TLS_SERVER_PSE=\absolute\path\to\my.pse
    // REG_COUNT=1
    // LANG=EN
    // TLS_SERVER_PARTNER_AUTH=REQUEST
    loginParams[0].name = cU("DEST"); loginParams[0].value = cU("SERVER_SAMPLE");

    installShutdownModule();
    installAuthenticationHandler();
    
    RFC_SERVER_HANDLE serverHandle = RfcCreateServer(loginParams, 1, &errorInfo);

    printfU(cU("Starting to listen...\n\n"));
    rc = RfcLaunchServer(serverHandle, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(cU("Error launching server"), &errorInfo);

    while (running != 0)
    {
        RfcSleep(1000 * 60);
    }

    rc = RfcShutdownServer(serverHandle, 0, &errorInfo);
    if (rc != RFC_OK)
        errorHandling(cU("Error shutting down server"), &errorInfo);

    RfcDestroyServer(serverHandle, &errorInfo);

    return 0;
}
