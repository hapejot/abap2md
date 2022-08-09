#find_package(SAPNWRFC REQUIRED)
find_path(SAPNWRFC_INCLUDE_DIR
  NAMES sapnwrfc.h
  PATHS ${CMAKE_CURRENT_SOURCE_DIR}/${CMAKE_HOST_SYSTEM_NAME}/nwrfcsdk/include
)

message( "SAP Include: ${SAPNWRFC_INCLUDE_DIR}" )
set( SAPNWRFC_LIB_ORIG ${CMAKE_CURRENT_SOURCE_DIR}/${CMAKE_HOST_SYSTEM_NAME}/nwrfcsdk/lib)
# Finally the library itself
#find_library(SAPNWRFC_LIBRARY_SAPNWRFC
#  NAMES sapnwrfc
#  PATHS ${CMAKE_CURRENT_SOURCE_DIR}/nwrfcsdk.${CMAKE_HOST_SYSTEM_NAME}/lib
#)

#find_library(SAPNWRFC_LIBRARY_SAPUCUM
#  NAMES sapucum libsapucum
#  PATHS ${CMAKE_CURRENT_SOURCE_DIR}/nwrfcsdk.${CMAKE_HOST_SYSTEM_NAME}/lib
#)

#file(GLOB SAPNWRFC_LIBRARY_ICU
#  PATHS ${CMAKE_CURRENT_SOURCE_DIR}/nwrfcsdk.${CMAKE_HOST_SYSTEM_NAME}/lib/libicu*
#)

set(SAPNWRFC_LIBDIR ${CMAKE_CURRENT_BINARY_DIR})


file(GLOB SAPNWRFC_LIBRARIES  ${SAPNWRFC_LIBDIR}/libsapnwrfc.*
                        ${SAPNWRFC_LIBDIR}/libsapucum.*
                        )


include_directories(${SAPNWRFC_INCLUDE_DIR})

#set(LIBS ${LIBS} ${SAPNWRFC_LIBRARIES})

#add_definitions(-DUSE_TCL_STUBS)
add_definitions(-DSAPwithUNICODE -DUNICODE -D_UNICODE -DSAPwithTHREADS)
#link_directories(${CMAKE_CURRENT_SOURCE_DIR}/nwrfcsdk.${CMAKE_HOST_SYSTEM_NAME}/lib)

file(GLOB SAPNWRFC_LIB_ORIG_FILES ${SAPNWRFC_LIB_ORIG}/*)


add_custom_command(
    OUTPUT SAPNW_INST
    COMMAND ${CMAKE_COMMAND} -E copy ${SAPNWRFC_LIB_ORIG_FILES}
                                    "${SAPNWRFC_LIBDIR}"
  )

add_custom_target( CopyPatterns ALL DEPENDS SAPNW_INST )

