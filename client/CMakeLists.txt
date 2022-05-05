cmake_minimum_required(VERSION 3.7)

project(sap_favs VERSION 1.0.0)
# set( CMAKE_EXPORT_COMPILE_COMMANDS 1 )
list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake/modules")
# set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
include_directories(${PROJECT_SOURCE_DIR}
                    ${CMAKE_CURRENT_BINARY_DIR})

add_compile_definitions( __STDC_WANT_LIB_EXT1__=1 )

find_package(sapnwrfc  REQUIRED)

#add_library(saprfc SHARED lib.c talloc/talloc.c)

add_compile_options( -g -save-temps )

add_executable(z_abap2md_generate_single z_abap2md_generate_single.c)
target_link_libraries(z_abap2md_generate_single   ${SAPNWRFC_LIBRARIES})


