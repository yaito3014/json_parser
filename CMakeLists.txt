cmake_minimum_required(VERSION 3.30)

project(cpp_json_parser LANGUAGES CXX)

add_library(json_parser INTERFACE)

target_compile_features(json_parser INTERFACE cxx_std_26)

file(GLOB YK_JSON_PARSER_HEADERS include/**)
target_sources(json_parser PUBLIC FILE_SET HEADERS BASE_DIRS include FILES ${YK_JSON_PARSER_HEADERS})

add_subdirectory(tests)
