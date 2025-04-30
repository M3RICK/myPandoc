#!/bin/bash

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test directory
TEST_DIR="test_files"
mkdir -p $TEST_DIR

# Counter for tests
PASSED=0
FAILED=0
TOTAL=0

# Function to run a test
run_test() {
    local test_name="$1"
    local command="$2"
    local expected_exit="$3"
    local compare_output="$4"
    local expected_output="$5"

    echo -e "${YELLOW}Running test: ${test_name}${NC}"
    ((TOTAL++))

    # Run the command and capture output and exit code
    output=$(eval "$command" 2>&1)
    exit_code=$?

    # Check exit code
    if [ $exit_code -eq $expected_exit ]; then
        # If we need to compare output
        if [ "$compare_output" = "true" ]; then
            if [ "$output" = "$expected_output" ]; then
                echo -e "${GREEN}✓ Test passed${NC}"
                ((PASSED++))
            else
                echo -e "${RED}✗ Test failed: Output does not match expected${NC}"
                echo -e "Expected: $expected_output"
                echo -e "Got: $output"
                ((FAILED++))
            fi
        else
            echo -e "${GREEN}✓ Test passed${NC}"
            ((PASSED++))
        fi
    else
        echo -e "${RED}✗ Test failed: Expected exit code $expected_exit, got $exit_code${NC}"
        echo -e "Output: $output"
        ((FAILED++))
    fi
    echo ""
}

# Create test files
echo '<document>
  <header title="Test Document"></header>
  <body>
    <paragraph>This is a test paragraph.</paragraph>
  </body>
</document>' > $TEST_DIR/test.xml

echo '{
  "header": {
    "title": "Test Document"
  },
  "body": [
    "This is a test paragraph."
  ]
}' > $TEST_DIR/test.json

echo '---
title: Test Document
---

This is a test paragraph.' > $TEST_DIR/test.md

# Basic tests
run_test "No arguments" "./mypandoc" 84 false ""
run_test "Missing input file" "./mypandoc -f xml" 84 false ""
run_test "Missing output format" "./mypandoc -i $TEST_DIR/test.xml" 84 false ""
run_test "Invalid input file" "./mypandoc -i nonexistent.xml -f json" 84 false ""

# Format conversion tests
run_test "XML to JSON" "./mypandoc -i $TEST_DIR/test.xml -f json -o $TEST_DIR/output.json" 0 false ""
if [ -f "$TEST_DIR/output.json" ]; then
    echo -e "${GREEN}✓ Output file created${NC}"
else
    echo -e "${RED}✗ Output file not created${NC}"
fi

run_test "JSON to XML" "./mypandoc -i $TEST_DIR/test.json -f xml -o $TEST_DIR/output.xml" 0 false ""
if [ -f "$TEST_DIR/output.xml" ]; then
    echo -e "${GREEN}✓ Output file created${NC}"
else
    echo -e "${RED}✗ Output file not created${NC}"
fi

run_test "XML to Markdown" "./mypandoc -i $TEST_DIR/test.xml -f markdown -o $TEST_DIR/output.md" 0 false ""
if [ -f "$TEST_DIR/output.md" ]; then
    echo -e "${GREEN}✓ Output file created${NC}"
else
    echo -e "${RED}✗ Output file not created${NC}"
fi

# Format detection test
run_test "Format detection (XML)" "./mypandoc -i $TEST_DIR/test.xml -f json -o $TEST_DIR/detected.json" 0 false ""

# Explicit format specification
run_test "Explicit format (XML)" "./mypandoc -i $TEST_DIR/test.xml -f json -e xml -o $TEST_DIR/explicit.json" 0 false ""

# Print to stdout
run_test "Print to stdout" "./mypandoc -i $TEST_DIR/test.xml -f xml" 0 false ""

# Clean up
# rm -rf $TEST_DIR

# Summary
echo -e "${YELLOW}Test Summary:${NC}"
echo -e "Total tests: $TOTAL"
echo -e "${GREEN}Passed: $PASSED${NC}"
echo -e "${RED}Failed: $FAILED${NC}"

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
