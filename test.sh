#!/bin/bash
# Improved test script for mypandoc that compares output with the original Pandoc

clear

# Build the project
echo "Building mypandoc..."
make re

# Check if pandoc is installed
if ! command -v pandoc &> /dev/null; then
    echo "Warning: pandoc is not installed. Output validation will be skipped."
    PANDOC_INSTALLED=false
else
    PANDOC_INSTALLED=true
fi

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Create test directory
TEST_DIR="test_outputs"
mkdir -p $TEST_DIR

# Function to run a test
run_test() {
    local test_name="$1"
    local cmd="$2"
    local output_file="$3"
    local expected_file="$4"

    echo -n "Running test: $test_name... "

    # Run the command and capture both output and exit code
    eval "$cmd" > /dev/null 2> "$TEST_DIR/error.log"
    local exit_code=$?

    if [ $exit_code -eq 0 ]; then
        echo -e "${GREEN}COMMAND SUCCEEDED${NC}"

        # Check if file was created
        if [ -f "$output_file" ]; then
            echo -e "  - Output file created: ${GREEN}YES${NC}"

            # Validate with pandoc if it's installed
            if [ "$PANDOC_INSTALLED" = true ] && [ -n "$expected_file" ]; then
                # Compare with Pandoc output (will likely have differences)
                echo -e "  - Comparing with Pandoc output (differences expected):"
                echo -e "${YELLOW}---------- PANDOC DIFF START ----------${NC}"
                diff -u "$output_file" "$expected_file" | head -n 20
                if [ $(diff -u "$output_file" "$expected_file" | wc -l) -gt 20 ]; then
                    echo -e "${YELLOW}... (diff truncated, showing first 20 lines only) ...${NC}"
                fi
                echo -e "${YELLOW}----------- PANDOC DIFF END -----------${NC}"

                # Compare with expected structure for this project
                expected_structure="${expected_file}.expected_structure"
                if [ -f "$expected_structure" ]; then
                    echo -e "  - Comparing with expected structure for this project:"
                    echo -e "${YELLOW}---------- STRUCTURE DIFF START ----------${NC}"
                    diff -u "$output_file" "$expected_structure" | head -n 20
                    if [ $(diff -u "$output_file" "$expected_structure" | wc -l) -gt 20 ]; then
                        echo -e "${YELLOW}... (diff truncated, showing first 20 lines only) ...${NC}"
                    fi
                    echo -e "${YELLOW}----------- STRUCTURE DIFF END -----------${NC}"

                    if diff -q "$output_file" "$expected_structure" > /dev/null; then
                        echo -e "  - Structure matches expected: ${GREEN}YES${NC}"
                    else
                        echo -e "  - Structure matches expected: ${YELLOW}NO${NC}"
                    fi
                fi
            fi
        else
            echo -e "  - Output file created: ${RED}NO${NC}"
            return 1
        fi
        return 0
    else
        echo -e "${RED}FAILED${NC}"
        echo -e "Command failed with exit code $exit_code."
        echo -e "Error output:"
        cat "$TEST_DIR/error.log"
        return 1
    fi
}

# Function to generate expected output with pandoc
generate_expected_output() {
    local input_file="$1"
    local output_format="$2"
    local output_file="$3"

    if [ "$PANDOC_INSTALLED" = true ]; then
        echo "Generating expected output with pandoc: $output_file"
        if [ "$output_format" = "xml" ]; then
            # Note: Pandoc doesn't have direct XML output that matches your format,
            # so we use HTML as a close approximation
            pandoc -f markdown -t html "$input_file" -o "$output_file" 2>/dev/null

            # For a better comparison, we'll also generate your format directly
            echo '<document>
<header title="Simple Test"></header>
<body>
<paragraph>This is a very simple test.</paragraph>
</body>
</document>' > "${output_file}.expected_structure"

        elif [ "$output_format" = "json" ]; then
            # Pandoc's JSON structure is different from yours
            pandoc -f markdown -t json "$input_file" -o "$output_file" 2>/dev/null

            # Generate your expected JSON structure
            echo '{
  "header": {
    "title": "Simple Test"
  },
  "body": [
    {
      "paragraph": ["This is a very simple test."]
    }
  ]
}' > "${output_file}.expected_structure"

        elif [ "$output_format" = "markdown" ]; then
            # This should be closer to your format
            pandoc -f html -t markdown "$input_file" -o "$output_file" 2>/dev/null

            # Generate your expected Markdown structure
            echo '---
title: Simple Test
---

This is a very simple test.' > "${output_file}.expected_structure"
        fi
    fi
}

# Create simple test files
echo "Creating test files for comparison..."

echo '<document>
<header title="Simple Test"></header>
<body>
<paragraph>This is a very simple test.</paragraph>
</body>
</document>' > "$TEST_DIR/simple.xml"

echo '{
    "header": {
        "title": "Simple Test"
    },
    "body": [
        {
            "paragraph": ["This is a very simple test."]
        }
    ]
}' > "$TEST_DIR/simple.json"

echo '---
title: Simple Test
---

This is a very simple test.' > "$TEST_DIR/simple.md"

# Generate expected outputs if pandoc is installed
if [ "$PANDOC_INSTALLED" = true ]; then
    generate_expected_output "$TEST_DIR/simple.md" "xml" "$TEST_DIR/expected_simple.xml"
    generate_expected_output "$TEST_DIR/simple.md" "json" "$TEST_DIR/expected_simple.json"
    generate_expected_output "$TEST_DIR/simple.xml" "markdown" "$TEST_DIR/expected_simple.md"

    # Show expected output contents for reference
    echo -e "\n${YELLOW}Expected Pandoc XML output:${NC}"
    head -n 10 "$TEST_DIR/expected_simple.xml"
    if [ $(wc -l < "$TEST_DIR/expected_simple.xml") -gt 10 ]; then
        echo "... (truncated)"
    fi

    echo -e "\n${YELLOW}Expected Pandoc JSON output:${NC}"
    head -n 10 "$TEST_DIR/expected_simple.json"
    if [ $(wc -l < "$TEST_DIR/expected_simple.json") -gt 10 ]; then
        echo "... (truncated)"
    fi

    echo -e "\n${YELLOW}Expected Pandoc Markdown output:${NC}"
    cat "$TEST_DIR/expected_simple.md"
fi

echo "Running basic functionality tests..."

# Test basic functionality
run_test "XML to XML" "./mypandoc -i $TEST_DIR/simple.xml -f xml -o $TEST_DIR/out_xml_to_xml.xml" "$TEST_DIR/out_xml_to_xml.xml" "$TEST_DIR/expected_simple.xml"

run_test "XML to JSON" "./mypandoc -i $TEST_DIR/simple.xml -f json -o $TEST_DIR/out_xml_to_json.json" "$TEST_DIR/out_xml_to_json.json" "$TEST_DIR/expected_simple.json"

run_test "XML to Markdown" "./mypandoc -i $TEST_DIR/simple.xml -f markdown -o $TEST_DIR/out_xml_to_md.md" "$TEST_DIR/out_xml_to_md.md" "$TEST_DIR/expected_simple.md"

run_test "JSON to XML" "./mypandoc -i $TEST_DIR/simple.json -f xml -o $TEST_DIR/out_json_to_xml.xml" "$TEST_DIR/out_json_to_xml.xml" "$TEST_DIR/expected_simple.xml"

run_test "JSON to JSON" "./mypandoc -i $TEST_DIR/simple.json -f json -o $TEST_DIR/out_json_to_json.json" "$TEST_DIR/out_json_to_json.json" "$TEST_DIR/expected_simple.json"

run_test "JSON to Markdown" "./mypandoc -i $TEST_DIR/simple.json -f markdown -o $TEST_DIR/out_json_to_md.md" "$TEST_DIR/out_json_to_md.md" "$TEST_DIR/expected_simple.md"

# Test auto-detection
echo "Testing format auto-detection..."

run_test "Auto-detect XML" "./mypandoc -i $TEST_DIR/simple.xml -f markdown -o $TEST_DIR/auto_xml.md" "$TEST_DIR/auto_xml.md" ""
run_test "Auto-detect JSON" "./mypandoc -i $TEST_DIR/simple.json -f xml -o $TEST_DIR/auto_json.xml" "$TEST_DIR/auto_json.xml" ""

# Test handling errors
echo "Testing error handling..."

run_test "Non-existent file" "./mypandoc -i nonexistent.xml -f xml -o $TEST_DIR/error.xml" "$TEST_DIR/error.xml" ""
run_test "Invalid format specification" "./mypandoc -i $TEST_DIR/simple.xml -f invalid -o $TEST_DIR/error.xml" "$TEST_DIR/error.xml" ""

# Test stdin/stdout
echo "Testing stdin/stdout..."

run_test "Output to stdout" "./mypandoc -i $TEST_DIR/simple.xml -f json > $TEST_DIR/stdout.json" "$TEST_DIR/stdout.json" ""

# Test complex documents
echo "Testing complex documents..."

# Create complex test files if they don't exist
if [ ! -f "test.xml" ] || [ ! -f "test.json" ]; then
    echo "Warning: Complex test files (test.xml or test.json) not found in current directory."
else
    run_test "Complex XML to JSON" "./mypandoc -i test.xml -f json -o $TEST_DIR/complex_xml_to_json.json" "$TEST_DIR/complex_xml_to_json.json" ""
    run_test "Complex JSON to XML" "./mypandoc -i test.json -f xml -o $TEST_DIR/complex_json_to_xml.xml" "$TEST_DIR/complex_json_to_xml.xml" ""
fi

echo "All tests completed."
echo "Test outputs are available in the $TEST_DIR directory."

# Uncomment to clean up
echo "Cleaning up..."
make fclean
# rm -rf $TEST_DIR
