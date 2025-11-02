#!/bin/bash

# Test script for all Ratatouille examples
# Usage: ./test_all_examples.sh

# Detect project root (where glados executable is located)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -f "$SCRIPT_DIR/../glados" ]; then
  PROJECT_ROOT="$SCRIPT_DIR/.."
elif [ -f "$SCRIPT_DIR/glados" ]; then
  PROJECT_ROOT="$SCRIPT_DIR"
else
  echo "Error: Cannot find glados compiler. Please run from project root or examples directory."
  exit 1
fi

COMPILER="$PROJECT_ROOT/glados"
VM="$PROJECT_ROOT/glados-vm"
EXAMPLES_DIR="$PROJECT_ROOT/examples"
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
EXAMPLES=$(find "$EXAMPLES_DIR" -name "*.rat" | sort)

echo ""
echo "  ____       _        _              _ _ _      "
echo " |  _ \ __ _| |_ __ _| |_ ___  _   _(_) | | ___ "
echo " | |_) / _\` | __/ _\` | __/ _ \| | | | | | |/ _ \\"
echo " |  _ < (_| | || (_| | || (_) | |_| | | | |  __/"
echo " |_| \_\__,_|\__\__,_|\__\___/ \__,_|_|_|_|\___|"
echo ""
echo "======================================"
echo "Testing All Ratatouille Examples"
echo "======================================"
echo ""

for example in $EXAMPLES; do
  TOTAL=$((TOTAL + 1))
  echo -n "Testing $example ... "

  # Check if the file should fail (contains [SHOULD_FAIL] in filename)
  SHOULD_FAIL=false
  if [[ "$example" == *"[SHOULD_FAIL]"* ]]; then
    SHOULD_FAIL=true
  fi

  # Compile the example
  if ! $COMPILER "$example" -o "$TEMP_BYTECODE" > /dev/null 2>&1; then
    if [ "$SHOULD_FAIL" = true ]; then
      echo -e "${GREEN}PASS${NC} (expected compilation error)"
      PASSED=$((PASSED + 1))
    else
      echo -e "${RED}FAIL${NC} (compilation error)"
      FAILED=$((FAILED + 1))
      # Show error details
      $COMPILER "$example" -o "$TEMP_BYTECODE" 2>&1 | head -10
    fi
    continue
  fi

  # Run the compiled bytecode with timeout
  if timeout 5 $VM "$TEMP_BYTECODE" > /dev/null 2>&1; then
    if [ "$SHOULD_FAIL" = true ]; then
      echo -e "${RED}FAIL${NC} (expected to fail but passed)"
      FAILED=$((FAILED + 1))
    else
      echo -e "${GREEN}PASS${NC}"
      PASSED=$((PASSED + 1))
    fi
  else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 124 ]; then
      echo -e "${YELLOW}TIMEOUT${NC}"
      TIMEOUT=$((TIMEOUT + 1))
    else
      if [ "$SHOULD_FAIL" = true ]; then
        echo -e "${GREEN}PASS${NC} (expected runtime error)"
        PASSED=$((PASSED + 1))
      else
        echo -e "${RED}FAIL${NC} (runtime error)"
        FAILED=$((FAILED + 1))
        # Show error details
        $VM "$TEMP_BYTECODE" 2>&1 | head -10
      fi
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
