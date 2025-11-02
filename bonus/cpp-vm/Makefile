# GLaDOS Ratatouille VM Makefile
# C++23 Build System with Optimization Support

CXX := g++
CXXFLAGS := -std=c++23 -Wall -Wextra -Wpedantic -Iinclude -Isrc
LDFLAGS :=

# Build modes
DEBUG_FLAGS := -g -O0 -DDEBUG -fsanitize=address -fsanitize=undefined
RELEASE_FLAGS := -O3 -march=native -mtune=native -DNDEBUG -flto
PROFILE_FLAGS := -O2 -g -fprofile-generate

# Directories
SRC_DIR := src
BUILD_DIR := build
BIN_DIR := bin
TEST_DIR := tests
BENCH_DIR := benchmarks

# Source files
CORE_SRC := $(wildcard $(SRC_DIR)/core/*.cpp)
LOADER_SRC := $(wildcard $(SRC_DIR)/loader/*.cpp)
VM_SRC := $(wildcard $(SRC_DIR)/vm/*.cpp)
HANDLER_SRC := $(wildcard $(SRC_DIR)/vm/handlers/*.cpp)
PROCESS_SRC := $(wildcard $(SRC_DIR)/process/*.cpp)
MAIN_SRC := $(SRC_DIR)/main.cpp

ALL_SRC := $(CORE_SRC) $(LOADER_SRC) $(VM_SRC) $(HANDLER_SRC) $(PROCESS_SRC)
OBJECTS := $(ALL_SRC:$(SRC_DIR)/%.cpp=$(BUILD_DIR)/%.o)

# Targets
TARGET := $(BIN_DIR)/glados_vm
TEST_TARGET := $(BIN_DIR)/vm_tests
BENCH_TARGET := $(BIN_DIR)/vm_benchmarks

# Default build mode
BUILD_MODE ?= release

ifeq ($(BUILD_MODE),debug)
    CXXFLAGS += $(DEBUG_FLAGS)
else ifeq ($(BUILD_MODE),profile)
    CXXFLAGS += $(PROFILE_FLAGS)
else
    CXXFLAGS += $(RELEASE_FLAGS)
endif

# Phony targets
.PHONY: all clean tests benchmarks run help install format

all: $(TARGET)

$(TARGET): $(OBJECTS) $(BUILD_DIR)/main.o | $(BIN_DIR)
	@echo "Linking $@..."
	@$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)
	@echo "Build complete: $@"

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp | $(BUILD_DIR)
	@mkdir -p $(dir $@)
	@echo "Compiling $<..."
	@$(CXX) $(CXXFLAGS) -c $< -o $@

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)/core $(BUILD_DIR)/loader $(BUILD_DIR)/vm $(BUILD_DIR)/vm/handlers $(BUILD_DIR)/process

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

clean:
	@echo "Cleaning..."
	@rm -rf $(BUILD_DIR) $(BIN_DIR)
	@echo "Clean complete"

tests: $(TEST_TARGET)

$(TEST_TARGET): $(OBJECTS) $(TEST_DIR)/VMInstructionTests.cpp | $(BIN_DIR)
	@echo "Building tests..."
	@$(CXX) $(CXXFLAGS) $(OBJECTS) $(TEST_DIR)/VMInstructionTests.cpp -o $@ $(LDFLAGS) -lgtest -lgtest_main -lpthread

benchmarks: $(BENCH_TARGET)

$(BENCH_TARGET): $(OBJECTS) $(BENCH_DIR)/InstructionBenchmarks.cpp | $(BIN_DIR)
	@echo "Building benchmarks..."
	@$(CXX) $(CXXFLAGS) $(OBJECTS) $(BENCH_DIR)/InstructionBenchmarks.cpp -o $@ $(LDFLAGS) -lbenchmark -lpthread

run: $(TARGET)
	@$(TARGET)

test: tests
	@$(TEST_TARGET)

bench: benchmarks
	@$(BENCH_TARGET)

debug:
	@$(MAKE) BUILD_MODE=debug

release:
	@$(MAKE) BUILD_MODE=release

help:
	@echo "Targets: all clean tests benchmarks run test bench debug release"
