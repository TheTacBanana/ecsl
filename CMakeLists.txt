# Minimum Version
cmake_minimum_required(VERSION 3.15)

project(ecslvm VERSION 1.0 LANGUAGES C)

set(PROJECT_INCLUDE "${CMAKE_CURRENT_LIST_DIR}/ecsl_vm/")
file(GLOB_RECURSE PROJECT_SOURCES CONFIGURE_DEPENDS "${CMAKE_CURRENT_LIST_DIR}/ecsl_vm/*.c")

add_executable(${PROJECT_NAME})
target_sources(${PROJECT_NAME} PRIVATE ${PROJECT_SOURCES})
target_include_directories(${PROJECT_NAME} PRIVATE ${PROJECT_INCLUDE})