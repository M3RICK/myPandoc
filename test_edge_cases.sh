#!/bin/bash
# Test script focusing on edge cases that might challenge your parser

clear

# Build the project
echo "Building mypandoc..."
make re

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Create test directory
TEST_DIR="test_edge_cases"
mkdir -p $TEST_DIR

# Function to run a test
run_test() {
    local test_name="$1"
    local cmd="$2"

    echo -n "Running test: $test_name... "

    # Run the command and capture both output and exit code
    eval "$cmd" > "$TEST_DIR/output.log" 2> "$TEST_DIR/error.log"
    local exit_code=$?

    if [ $exit_code -eq 0 ]; then
        echo -e "${GREEN}PASSED${NC}"
        cat "$TEST_DIR/output.log"
        return 0
    else
        echo -e "${RED}FAILED (Exit code: $exit_code)${NC}"
        echo -e "Error output:"
        cat "$TEST_DIR/error.log"
        return 1
    fi
}

# ============= GENERATE EDGE CASE FILES =============

echo "Generating edge case files that might challenge your parser..."

# Edge Case 1: XML with whitespace before document tag
echo "
<document>
<header title=\"XML with whitespace\"></header>
<body>
<paragraph>This XML file has whitespace before the document tag.</paragraph>
</body>
</document>" > "$TEST_DIR/whitespace_xml.xml"

# Edge Case 2: XML with XML declaration
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<document>
<header title=\"XML with declaration\"></header>
<body>
<paragraph>This XML file has an XML declaration at the top.</paragraph>
</body>
</document>" > "$TEST_DIR/declaration_xml.xml"

# Edge Case 3: XML with different root element
echo "<root>
<header title=\"Different root element\"></header>
<body>
<paragraph>This XML file has a different root element.</paragraph>
</body>
</root>" > "$TEST_DIR/different_root.xml"

# Edge Case 4: JSON with whitespace before opening brace
echo "
{
  \"header\": {
    \"title\": \"JSON with whitespace\"
  },
  \"body\": [
    {
      \"paragraph\": [\"This JSON file has whitespace before the opening brace.\"]
    }
  ]
}" > "$TEST_DIR/whitespace_json.json"

# Edge Case 5: JSON with comments (not standard JSON but sometimes used)
echo "{
  /* This is a comment */
  \"header\": {
    \"title\": \"JSON with comments\"
  },
  \"body\": [
    {
      \"paragraph\": [\"This JSON file has comments.\"]
    }
  ]
}" > "$TEST_DIR/comments_json.json"

# Edge Case 6: JSON with different structure
echo "{
  \"title\": \"Different JSON structure\",
  \"content\": [
    {
      \"type\": \"paragraph\",
      \"text\": \"This JSON file has a different structure than expected.\"
    }
  ]
}" > "$TEST_DIR/different_structure.json"

# Edge Case 7: Markdown without YAML frontmatter
echo "# Markdown without frontmatter

This Markdown file doesn't have YAML frontmatter.

## Section

Some content in a section." > "$TEST_DIR/no_frontmatter.md"

# Edge Case 8: Markdown with alternative frontmatter format
echo "+++
title = \"Alternative frontmatter\"
+++

This Markdown file uses TOML frontmatter instead of YAML." > "$TEST_DIR/toml_frontmatter.md"

# Edge Case 9: XML with nested elements
echo "<document>
<header title=\"Nested XML\"></header>
<body>
<paragraph>This paragraph <bold>has <italic>nested</italic> formatting</bold> elements.</paragraph>
<section title=\"Nested Section\">
  <paragraph>This is inside a section.</paragraph>
  <list>
    <item>List item 1</item>
    <item>List item 2</item>
  </list>
</section>
</body>
</document>" > "$TEST_DIR/nested_xml.xml"

# Edge Case 10: JSON with nested structures
echo "{
  \"header\": {
    \"title\": \"Nested JSON\"
  },
  \"body\": [
    {
      \"paragraph\": [
        \"This paragraph has \",
        {
          \"bold\": [
            \"bold \",
            {
              \"italic\": [\"nested\"]
            },
            \" formatting\"
          ]
        },
        \" elements.\"
      ]
    },
    {
      \"section\": {
        \"title\": \"Nested Section\",
        \"contents\": [
          {
            \"paragraph\": [\"This is inside a section.\"]
          },
          {
            \"list\": [
              {
                \"item\": [\"List item 1\"]
              },
              {
                \"item\": [\"List item 2\"]
              }
            ]
          }
        ]
      }
    }
  ]
}" > "$TEST_DIR/nested_json.json"

# ============= RUN TESTS =============

echo "Running tests with edge case files..."
echo "Note: These tests are designed to challenge your parser."
echo "If they fail, it indicates areas where your parser might be too strict."
echo "------------------------------------------------------"

# Test XML edge cases
echo -e "\n${YELLOW}Testing XML edge cases:${NC}"
run_test "XML with whitespace" "./mypandoc -i $TEST_DIR/whitespace_xml.xml -f json -o $TEST_DIR/whitespace_xml.json"
run_test "XML with declaration" "./mypandoc -i $TEST_DIR/declaration_xml.xml -f json -o $TEST_DIR/declaration_xml.json"
run_test "XML with different root" "./mypandoc -i $TEST_DIR/different_root.xml -f json -o $TEST_DIR/different_root.json"
run_test "XML with nested elements" "./mypandoc -i $TEST_DIR/nested_xml.xml -f json -o $TEST_DIR/nested_xml.json"

# Test JSON edge cases
echo -e "\n${YELLOW}Testing JSON edge cases:${NC}"
run_test "JSON with whitespace" "./mypandoc -i $TEST_DIR/whitespace_json.json -f xml -o $TEST_DIR/whitespace_json.xml"
run_test "JSON with comments" "./mypandoc -i $TEST_DIR/comments_json.json -f xml -o $TEST_DIR/comments_json.xml"
run_test "JSON with different structure" "./mypandoc -i $TEST_DIR/different_structure.json -f xml -o $TEST_DIR/different_structure.xml"
run_test "JSON with nested structures" "./mypandoc -i $TEST_DIR/nested_json.json -f xml -o $TEST_DIR/nested_json.xml"

# Test Markdown edge cases
echo -e "\n${YELLOW}Testing Markdown edge cases:${NC}"
run_test "Markdown without frontmatter" "./mypandoc -i $TEST_DIR/no_frontmatter.md -f xml -o $TEST_DIR/no_frontmatter.xml"
run_test "Markdown with alternative frontmatter" "./mypandoc -i $TEST_DIR/toml_frontmatter.md -f xml -o $TEST_DIR/toml_frontmatter.xml"

echo "All tests completed."
echo "Test files and outputs are available in the $TEST_DIR directory."

# Create a summary of failures
echo -e "\n${YELLOW}Summary of failing edge cases:${NC}"
find "$TEST_DIR" -name "error.log" -not -size 0 | while read file; do
    test_name=$(basename "$file" .log | sed 's/error_//')
    echo -e "${RED}✗ $test_name${NC}"
    echo "  Error: $(head -n 1 "$file")"
done

# Create a summary of successes
echo -e "\n${YELLOW}Summary of passing edge cases:${NC}"
for test_file in "$TEST_DIR"/*.xml "$TEST_DIR"/*.json "$TEST_DIR"/*.md; do
    base_name=$(basename "$test_file")
    if [[ $base_name != whitespace* && $base_name != declaration* && $base_name != different* &&
          $base_name != comments* && $base_name != nested* && $base_name != no_frontmatter* &&
          $base_name != toml_frontmatter* && $base_name != output* && $base_name != error* ]]; then
        continue
    fi

    output_file=""
    if [[ $test_file == *".xml" ]]; then
        output_file="${test_file%.xml}.json"
    elif [[ $test_file == *".json" ]]; then
        output_file="${test_file%.json}.xml"
    elif [[ $test_file == *".md" ]]; then
        output_file="${test_file%.md}.xml"
    fi

    if [[ -f "$output_file" && -s "$output_file" ]]; then
        echo -e "${GREEN}✓ $(basename "$test_file")${NC}"
    fi
done

echo -e "\n${YELLOW}Suggested fixes:${NC}"
echo "1. Modify validateXml() in FileStatus.hs to allow whitespace before tags"
echo "2. Modify validateJson() in FileStatus.hs to allow whitespace before {"
echo "3. Enhance parseXml in XmlParser.hs to handle different root elements"
echo "4. Add support for Markdown without frontmatter"
echo "5. Implement more robust error reporting"
