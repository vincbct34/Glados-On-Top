#!/bin/bash

# Test script for all Ratatouille examples
# Usage: ./test_all_examples.sh

COMPILER="./glados"
VM="./glados-vm"
TEMP_BYTECODE="/tmp/test.rtbc"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0
TIMEOUT=0
TOTAL=0

# Find all .rat files in examples directory
EXAMPLES=$(find examples -name "*.rat" | sort)

echo "======================================"
echo "Testing All Ratatouille Examples"
echo "======================================"
echo ""

for example in $EXAMPLES; do
  TOTAL=$((TOTAL + 1))
  echo -n "Testing $example ... "

  # Compile the example
  if ! $COMPILER "$example" -o "$TEMP_BYTECODE" > /dev/null 2>&1; then
    echo -e "${RED}FAIL${NC} (compilation error)"
    FAILED=$((FAILED + 1))
    # Show error details
    $COMPILER "$example" -o "$TEMP_BYTECODE" 2>&1 | head -10
    continue
  fi

  # Run the compiled bytecode with timeout
  if timeout 5 $VM "$TEMP_BYTECODE" > /dev/null 2>&1; then
    echo -e "${GREEN}PASS${NC}"
    PASSED=$((PASSED + 1))
  else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 124 ]; then
      echo -e "${YELLOW}TIMEOUT${NC}"
      TIMEOUT=$((TIMEOUT + 1))
    else
      echo -e "${RED}FAIL${NC} (runtime error)"
      FAILED=$((FAILED + 1))
      # Show error details
      $VM "$TEMP_BYTECODE" 2>&1 | head -10
    fi
  fi
done

# Clean up
rm -f "$TEMP_BYTECODE"

echo ""
echo "======================================"
echo "Test Results Summary"
echo "======================================"
echo "PASSED:  $PASSED"
echo "FAILED:  $FAILED"
echo "TIMEOUT: $TIMEOUT"
echo "TOTAL:   $TOTAL"
echo "======================================"

if [ $FAILED -eq 0 ] && [ $TIMEOUT -eq 0 ]; then
  echo -e "${GREEN}✓ All tests passed${NC}"
  exit 0
else
  echo -e "${RED}✗ Some tests failed${NC}"
  exit 1
fi
