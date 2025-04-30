#!/usr/bin/env python3

import os
import sys
import json
import subprocess
import xml.etree.ElementTree as ET
from tempfile import NamedTemporaryFile

# ANSI colors for output
GREEN = '\033[0;32m'
RED = '\033[0;31m'
YELLOW = '\033[1;33m'
BLUE = '\033[0;34m'
NC = '\033[0m'  # No Color

def print_color(color, message):
    print(f"{color}{message}{NC}")

def run_command(cmd):
    """Run a shell command and return stdout, stderr, and exit code"""
    process = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, text=True
    )
    stdout, stderr = process.communicate()
    return stdout, stderr, process.returncode

def validate_xml(xml_content):
    """Validate XML structure"""
    try:
        root = ET.fromstring(xml_content)
        return True, "Valid XML"
    except Exception as e:
        return False, f"Invalid XML: {str(e)}"

def validate_json(json_content):
    """Validate JSON structure"""
    try:
        parsed = json.loads(json_content)
        return True, "Valid JSON"
    except json.JSONDecodeError as e:
        return False, f"Invalid JSON: {str(e)}"

def validate_markdown(md_content):
    """Basic validation for Markdown (checks for YAML frontmatter)"""
    if md_content.startswith('---'):
        parts = md_content.split('---', 2)
        if len(parts) >= 3:
            return True, "Valid Markdown with frontmatter"
    return False, "Invalid Markdown structure (missing proper frontmatter)"

def create_test_files():
    """Create test files with various document elements"""
    test_dir = "test_complex"
    os.makedirs(test_dir, exist_ok=True)

    # Complex XML test file
    complex_xml = '''<document>
  <header title="Complex Document" author="Test Author" date="2024-04-30"></header>
  <body>
    <paragraph>This is a <bold>paragraph</bold> with <italic>formatting</italic> and <code>inline code</code>.</paragraph>
    <section title="Introduction">
      <paragraph>This is the introduction with a <link href="https://example.com">link</link>.</paragraph>
      <codeblock>function test() {
  console.log("Hello world");
}</codeblock>
    </section>
    <list>
      <item>First item</item>
      <item>Second <bold>item</bold> with formatting</item>
    </list>
  </body>
</document>'''

    # Complex JSON test file
    complex_json = '''{
  "header": {
    "title": "Complex Document",
    "author": "Test Author",
    "date": "2024-04-30"
  },
  "body": [
    {
      "paragraph": [
        "This is a ",
        {
          "bold": ["paragraph"]
        },
        " with ",
        {
          "italic": ["formatting"]
        },
        " and ",
        {
          "code": "inline code"
        },
        "."
      ]
    },
    {
      "section": {
        "title": "Introduction",
        "contents": [
          {
            "paragraph": [
              "This is the introduction with a ",
              {
                "link": {
                  "text": "link",
                  "url": "https://example.com"
                }
              },
              "."
            ]
          },
          {
            "codeblock": "function test() {\\n  console.log(\\"Hello world\\");\\n}"
          }
        ]
      }
    },
    {
      "list": [
        {
          "item": ["First item"]
        },
        {
          "item": [
            "Second ",
            {
              "bold": ["item"]
            },
            " with formatting"
          ]
        }
      ]
    }
  ]
}'''

    # Complex Markdown test file
    complex_md = '''---
title: Complex Document
author: Test Author
date: 2024-04-30
---

This is a **paragraph** with *formatting* and `inline code`.

## Introduction

This is the introduction with a [link](https://example.com).

```
function test() {
  console.log("Hello world");
}
```

- First item
- Second **item** with formatting
'''

    with open(f"{test_dir}/complex.xml", "w") as f:
        f.write(complex_xml)

    with open(f"{test_dir}/complex.json", "w") as f:
        f.write(complex_json)

    with open(f"{test_dir}/complex.md", "w") as f:
        f.write(complex_md)

    return test_dir

def test_conversion(input_file, output_format, exec_path="./mypandoc"):
    """Test conversion and validate output"""
    print_color(YELLOW, f"Testing conversion: {input_file} -> {output_format}")

    # Create temporary file for output
    with NamedTemporaryFile(delete=False, suffix=f".{output_format}") as tmp:
        output_file = tmp.name

    # Run conversion
    cmd = f"{exec_path} -i {input_file} -f {output_format} -o {output_file}"
    stdout, stderr, exit_code = run_command(cmd)

    if exit_code != 0:
        print_color(RED, f"Conversion failed with exit code {exit_code}")
        print(f"STDOUT: {stdout}")
        print(f"STDERR: {stderr}")
        return False

    # Read output file
    try:
        with open(output_file, 'r') as f:
            content = f.read()

        # Validate based on format
        if output_format == 'xml':
            valid, message = validate_xml(content)
        elif output_format == 'json':
            valid, message = validate_json(content)
        elif output_format == 'markdown':
            valid, message = validate_markdown(content)
        else:
            valid, message = False, f"Unknown format: {output_format}"

        if valid:
            print_color(GREEN, f"✓ Valid {output_format} content: {message}")
            print_color(BLUE, f"Output preview: {content[:100]}...")
        else:
            print_color(RED, f"✗ {message}")
            print(f"Content: {content}")

        # Clean up temporary file
        os.unlink(output_file)
        return valid

    except Exception as e:
        print_color(RED, f"Error reading/validating output file: {str(e)}")
        return False

def test_round_trip(test_dir, exec_path="./mypandoc"):
    """Test round-trip conversions (XML->JSON->XML, etc.)"""
    print_color(YELLOW, "\n=== Testing Round-Trip Conversions ===")

    # XML -> JSON -> XML
    with NamedTemporaryFile(delete=False, suffix=".json") as tmp_json:
        json_file = tmp_json.name

    with NamedTemporaryFile(delete=False, suffix=".xml") as tmp_xml:
        xml_file = tmp_xml.name

    # First conversion: XML -> JSON
    cmd1 = f"{exec_path} -i {test_dir}/complex.xml -f json -o {json_file}"
    _, _, exit_code1 = run_command(cmd1)

    if exit_code1 != 0:
        print_color(RED, "Round-trip test failed at XML -> JSON step")
        return False

    # Second conversion: JSON -> XML
    cmd2 = f"{exec_path} -i {json_file} -f xml -o {xml_file}"
    _, _, exit_code2 = run_command(cmd2)

    if exit_code2 != 0:
        print_color(RED, "Round-trip test failed at JSON -> XML step")
        os.unlink(json_file)
        return False

    # Compare original and round-trip XML (simplified check)
    try:
        with open(f"{test_dir}/complex.xml", 'r') as f:
            original = f.read()

        with open(xml_file, 'r') as f:
            roundtrip = f.read()

        # Very basic comparison - just check if documents have similar structure
        # A more thorough test would parse both XMLs and compare the structure
        if "<document>" in roundtrip and "<header" in roundtrip and "<body>" in roundtrip:
            print_color(GREEN, "✓ Round-trip test passed (XML -> JSON -> XML)")
            success = True
        else:
            print_color(RED, "✗ Round-trip test failed - output doesn't match expected structure")
            print("Original:\n", original[:200], "...")
            print("Round-trip:\n", roundtrip[:200], "...")
            success = False

        # Clean up
        os.unlink(json_file)
        os.unlink(xml_file)
        return success

    except Exception as e:
        print_color(RED, f"Error in round-trip test: {str(e)}")
        return False

def main():
    # Check if executable exists
    exec_path = "./mypandoc"
    if not os.path.isfile(exec_path):
        print_color(RED, f"Error: {exec_path} not found")
        sys.exit(1)

    # Create test files
    test_dir = create_test_files()
    print_color(GREEN, f"Created test files in {test_dir}/")

    # Test different conversions
    tests = [
        (f"{test_dir}/complex.xml", "json"),
        (f"{test_dir}/complex.xml", "markdown"),
        (f"{test_dir}/complex.json", "xml"),
        (f"{test_dir}/complex.json", "markdown"),
        # Uncomment if markdown input is supported
        # (f"{test_dir}/complex.md", "xml"),
        # (f"{test_dir}/complex.md", "json"),
    ]

    results = []
    for input_file, output_format in tests:
        result = test_conversion(input_file, output_format, exec_path)
        results.append(result)
        print("")  # Add spacing between tests

    # Test round-trip conversion
    round_trip_result = test_round_trip(test_dir, exec_path)
    results.append(round_trip_result)

    # Print summary
    passed = results.count(True)
    failed = results.count(False)
    print_color(YELLOW, "\n=== Test Summary ===")
    print(f"Total tests: {len(results)}")
    print_color(GREEN, f"Passed: {passed}")
    print_color(RED, f"Failed: {failed}")

    if failed == 0:
        print_color(GREEN, "\nAll tests passed!")
        return 0
    else:
        print_color(RED, "\nSome tests failed!")
        return 1

if __name__ == "__main__":
    sys.exit(main())
