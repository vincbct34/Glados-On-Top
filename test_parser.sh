#!/bin/bash
# Comprehensive test script for Ratatouille parser
# Tests all example files and reports results

set -e  # Exit on error

echo "================================"
echo "RATATOUILLE PARSER TEST SUITE"
echo "================================"
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0

# Test function
test_file() {
    local file=$1
    local name=$(basename "$file")
    TOTAL=$((TOTAL + 1))

    echo -n "Testing $name... "

    if ./glados "$file" > /dev/null 2>&1; then
        echo -e "${GREEN}✓ PASS${NC}"
        PASSED=$((PASSED + 1))
        return 0
    else
        echo -e "${RED}✗ FAIL${NC}"
        FAILED=$((FAILED + 1))
        echo "  Error details:"
        ./glados "$file" 2>&1 | grep -A 3 "Parse Error" || true
        return 1
    fi
}

# Test basic examples
echo -e "${BLUE}=== BASIC EXAMPLES ===${NC}"
test_file "examples/basics/helloWorld.rat"
test_file "examples/basics/counter.rat"
echo ""

# Test advanced examples
echo -e "${BLUE}=== ADVANCED EXAMPLES ===${NC}"
test_file "examples/advanced/errorHandling.rat"
test_file "examples/advanced/asynchroneCalc.rat"
test_file "examples/advanced/recursiveCounter.rat"
test_file "examples/advanced/triangularComm.rat"
echo ""

# Summary
echo "================================"
echo "TEST SUMMARY"
echo "================================"
echo "Total tests:  $TOTAL"
echo -e "Passed:       ${GREEN}$PASSED${NC}"
echo -e "Failed:       ${RED}$FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed! ✓${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed! ✗${NC}"
    exit 1
fi
