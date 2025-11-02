cmake_minimum_required(VERSION 3.20)
project(GladosVM VERSION 1.0.0 LANGUAGES CXX)

# C++23 Standard
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wpedantic")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0 -DDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -march=native -mtune=native -DNDEBUG -flto")

# Source files
set(CORE_SOURCES
    src/core/RuntimeValue.cpp
    src/core/InstructionStack.cpp
)

set(LOADER_SOURCES
    src/loader/BytecodeLoader.cpp
    src/loader/InstructionDecoder.cpp
)

set(PROCESS_SOURCES
    src/process/ProcessTemplate.cpp
    src/process/Message.cpp
    src/process/ProcessMailbox.cpp
    src/process/Process.cpp
)

set(VM_SOURCES
    src/vm/VirtualMachine.cpp
    src/vm/VariableStore.cpp
    src/vm/ExecutionEngine.cpp
    src/vm/FunctionManager.cpp
    src/vm/ProcessManager.cpp
    src/vm/VMContext.cpp
    src/vm/InstructionDispatcher.cpp
)

# Pattern matcher only used in benchmarks
set(BENCHMARK_ONLY_SOURCES
    src/vm/PatternMatcher.cpp
)

set(ALL_SOURCES
    ${CORE_SOURCES}
    ${LOADER_SOURCES}
    ${PROCESS_SOURCES}
    ${VM_SOURCES}
    src/main.cpp
)

# Main executable
add_executable(glados_vm ${ALL_SOURCES})

target_include_directories(glados_vm PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

# Enable LTO for release builds
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    set_property(TARGET glados_vm PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

# Tests (optional - requires Google Test)
option(BUILD_TESTS "Build test suite" ON)

if(BUILD_TESTS)
    find_package(GTest QUIET)
    
    if(GTest_FOUND)
        enable_testing()
        
        add_executable(vm_tests
            tests/VMInstructionTests.cpp
            ${CORE_SOURCES}
            ${LOADER_SOURCES}
            ${PROCESS_SOURCES}
            ${VM_SOURCES}
        )
        
        target_include_directories(vm_tests PRIVATE
            ${CMAKE_CURRENT_SOURCE_DIR}/src
            ${CMAKE_CURRENT_SOURCE_DIR}/include
        )
        
        target_link_libraries(vm_tests
            GTest::gtest
            GTest::gtest_main
        )
        
        add_test(NAME VMInstructionTests COMMAND vm_tests)
        
        message(STATUS "Building with tests enabled")
    else()
        message(STATUS "Google Test not found - tests disabled")
    endif()
endif()

# Benchmarks (optional - requires Google Benchmark)
option(BUILD_BENCHMARKS "Build benchmark suite" OFF)

if(BUILD_BENCHMARKS)
    find_package(benchmark QUIET)
    
    if(benchmark_FOUND)
        add_executable(vm_benchmarks
            benchmarks/InstructionBenchmarks.cpp
            ${CORE_SOURCES}
            ${LOADER_SOURCES}
            ${PROCESS_SOURCES}
            ${VM_SOURCES}
            ${BENCHMARK_ONLY_SOURCES}
        )
        
        target_include_directories(vm_benchmarks PRIVATE
            ${CMAKE_CURRENT_SOURCE_DIR}/src
            ${CMAKE_CURRENT_SOURCE_DIR}/include
        )
        
        target_link_libraries(vm_benchmarks
            benchmark::benchmark
        )
        
        message(STATUS "Building with benchmarks enabled")
    else()
        message(STATUS "Google Benchmark not found - benchmarks disabled")
    endif()
endif()

# Install targets
install(TARGETS glados_vm
    RUNTIME DESTINATION bin
)

# Print configuration summary
message(STATUS "")
message(STATUS "Glados VM Configuration:")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C++ standard: C++${CMAKE_CXX_STANDARD}")
message(STATUS "  Compiler: ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "  Tests: ${BUILD_TESTS}")
message(STATUS "  Benchmarks: ${BUILD_BENCHMARKS}")
message(STATUS "")
