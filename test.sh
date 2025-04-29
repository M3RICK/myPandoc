#!/bin/bash
# Test script for mypandoc

# Build the project
echo "Building mypandoc..."
make re

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Function to run a test
run_test() {
    local test_name="$1"
    local cmd="$2"
    
    echo -n "Running test: $test_name... "
    if eval "$cmd" > /dev/null 2>&1; then
        echo -e "${GREEN}PASSED${NC}"
        return 0
    else
        echo -e "${RED}FAILED${NC}"
        echo "Command failed: $cmd"
        return 1
    fi
}

# Test XML parsing and output
echo "Testing XML parsing and output..."

# Test 1: XML to XML
run_test "XML to XML" "./mypandoc -i test.xml -f xml -o output.xml"

# Test 2: XML to JSON
run_test "XML to JSON" "./mypandoc -i test.xml -f json -o output.json"

# Test 3: XML to Markdown
run_test "XML to Markdown" "./mypandoc -i test.xml -f markdown -o output.md"

# Test JSON parsing and output
echo "Testing JSON parsing and output..."

# Test 4: JSON to XML
run_test "JSON to XML" "./mypandoc -i test.json -f xml -o output_from_json.xml"

# Test 5: JSON to JSON
run_test "JSON to JSON" "./mypandoc -i test.json -f json -o output_from_json.json"

# Test 6: JSON to Markdown
run_test "JSON to Markdown" "./mypandoc -i test.json -f markdown -o output_from_json.md"

# Test for auto-detection of format
echo "Testing format auto-detection..."

# Test 7: Auto-detect XML
run_test "Auto-detect XML" "./mypandoc -i test.xml -f markdown -o auto_xml.md"

# Test 8: Auto-detect JSON
run_test "Auto-detect JSON" "./mypandoc -i test.json -f xml -o auto_json.xml"

echo "All tests completed."