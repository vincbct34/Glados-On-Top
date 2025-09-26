#!/bin/bash

# Test runner for Glados LISP interpreter
# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

GLADOS_EXEC="./glados"
TEST_DIR="test"
PASSED=0
FAILED=0

echo "======================================"
echo "Running Glados LISP Interpreter Tests"
echo "======================================"

# Function to run a test file
run_test() {
    local test_file=$1
    local test_name=$2
    local expect_success=${3:-true}
    
    echo -n "Testing $test_name... "
    
    if [ ! -f "$test_file" ]; then
        echo -e "${RED}FAIL${NC} (file not found)"
        ((FAILED++))
        return
    fi
    
    # Run the test and capture output
    output=$($GLADOS_EXEC < "$test_file" 2>&1)
    exit_code=$?
    
    if [ "$expect_success" = true ]; then
        if [ $exit_code -eq 0 ]; then
            echo -e "${GREEN}PASS${NC}"
            ((PASSED++))
            echo "Output:"
            echo "$output" | sed 's/^/  /'
        else
            echo -e "${RED}FAIL${NC} (exit code: $exit_code)"
            ((FAILED++))
            echo "Error output:"
            echo "$output" | sed 's/^/  /'
        fi
    else
        # For error tests, we expect non-zero exit code or error messages
        if [ $exit_code -ne 0 ] || echo "$output" | grep -q -i "error"; then
            echo -e "${GREEN}PASS${NC} (expected failure)"
            ((PASSED++))
        else
            echo -e "${RED}FAIL${NC} (should have failed)"
            ((FAILED++))
            echo "Unexpected success output:"
            echo "$output" | sed 's/^/  /'
        fi
    fi
    echo ""
}

# Check if glados executable exists
if [ ! -f "$GLADOS_EXEC" ]; then
    echo -e "${RED}Error: glados executable not found${NC}"
    echo "Please build the project first with: stack build"
    exit 1
fi

# Run all test suites
echo -e "${YELLOW}Arithmetic Tests:${NC}"
run_test "$TEST_DIR/arithmetic.scm" "Basic Arithmetic"

echo -e "${YELLOW}List Tests:${NC}"
run_test "$TEST_DIR/lists.scm" "List Operations"

echo -e "${YELLOW}Function Tests:${NC}"
run_test "$TEST_DIR/functions.scm" "Functions and Lambdas"

echo -e "${YELLOW}Integration Tests:${NC}"
run_test "$TEST_DIR/integration.scm" "Complex Integration"

echo -e "${YELLOW}Error Handling Tests:${NC}"
run_test "$TEST_DIR/errors.scm" "Error Cases" false

# Summary
echo "======================================"
echo "Test Results:"
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"
echo -e "Total:  $(($PASSED + $FAILED))"

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
