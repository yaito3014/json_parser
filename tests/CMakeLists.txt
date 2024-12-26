include(CTest)

if(BUILD_TESTING)
    find_package(Boost CONFIG REQUIRED COMPONENTS unit_test_framework)
    
    add_executable(yk_json_parser_test test.cpp)
    target_link_libraries(
        yk_json_parser_test
        PRIVATE json_parser
        PRIVATE Boost::unit_test_framework
    )
endif()
