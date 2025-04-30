#!/usr/bin/env python3

import os
import sys
import subprocess
import tempfile
import xml.etree.ElementTree as ET
from pathlib import Path

# ANSI colors for output
GREEN = '\033[0;32m'
RED = '\033[0;31m'
YELLOW = '\033[1;33m'
BLUE = '\033[0;34m'
CYAN = '\033[0;36m'
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

def create_test_case(filename, content):
    """Create a test XML file with the given content"""
    with open(filename, 'w') as f:
        f.write(content)
    return filename

def validate_xml_output(content):
    """Validate that output is well-formed XML"""
    try:
        root = ET.fromstring(content)
        return True, ""
    except Exception as e:
        return False, str(e)

def test_parser(test_name, xml_content, parser_path="./mypandoc", expected_success=True):
    """Test the XML parser with a specific test case"""
    print_color(YELLOW, f"\n=== Testing XML Parser: {test_name} ===")

    # Create temporary test file
    test_dir = Path("xml_parser_tests")
    test_dir.mkdir(exist_ok=True)

    test_file = test_dir / f"{test_name.lower().replace(' ', '_')}.xml"
    create_test_case(test_file, xml_content)

    # Create temporary output file
    output_file = test_dir / f"{test_name.lower().replace(' ', '_')}_output.json"

    # Run the parser
    cmd = f"{parser_path} -i {test_file} -f json -o {output_file} -e xml"
    stdout, stderr, exit_code = run_command(cmd)

    # Check if result matches expectations
    success = (exit_code == 0) if expected_success else (exit_code != 0)

    if success and expected_success:
        print_color(GREEN, f"✓ Parser successfully handled the test case")

        # Check if output file exists and is valid
        if output_file.exists():
            with open(output_file, 'r') as f:
                output_content = f.read()

            print_color(CYAN, f"Output (first 150 chars):")
            print(output_content[:150] + "..." if len(output_content) > 150 else output_content)

            # Try converting back to XML to test round-trip
            round_trip_file = test_dir / f"{test_name.lower().replace(' ', '_')}_roundtrip.xml"
            cmd_roundtrip = f"{parser_path} -i {output_file} -f xml -o {round_trip_file} -e json"
            rt_stdout, rt_stderr, rt_exit_code = run_command(cmd_roundtrip)

            if rt_exit_code == 0:
                print_color(GREEN, f"✓ Round-trip conversion successful")

                # Validate the XML structure
                with open(round_trip_file, 'r') as f:
                    rt_content = f.read()

                valid, error = validate_xml_output(rt_content)
                if valid:
                    print_color(GREEN, f"✓ Round-trip XML is well-formed")
                else:
                    print_color(RED, f"✗ Round-trip XML is not well-formed: {error}")
            else:
                print_color(RED, f"✗ Round-trip conversion failed with exit code {rt_exit_code}")
                print(f"STDERR: {rt_stderr}")
        else:
            print_color(RED, f"✗ Output file was not created")
    elif not success and not expected_success:
        print_color(GREEN, f"✓ Parser correctly rejected invalid input as expected")
        print_color(CYAN, f"Error message: {stderr}")
    else:
        print_color(RED, f"✗ Test failed - {'Expected success but got failure' if expected_success else 'Expected failure but got success'}")
        print(f"STDOUT: {stdout}")
        print(f"STDERR: {stderr}")
        print(f"Exit code: {exit_code}")

    return success

def run_all_tests(parser_path="./mypandoc"):
    """Run all XML parser tests"""
    test_cases = [
        # Basic valid documents
        {
            "name": "Basic Document",
            "content": '''<document>
  <header title="Basic Test"></header>
  <body>
    <paragraph>Simple paragraph.</paragraph>
  </body>
</document>''',
            "expected_success": True
        },

        # Document with all header attributes
        {
            "name": "Full Header",
            "content": '''<document>
  <header title="Full Header" author="Test Author" date="2024-04-30"></header>
  <body>
    <paragraph>Test paragraph.</paragraph>
  </body>
</document>''',
            "expected_success": True
        },

        # Document with all formatting options
        {
            "name": "Formatting Elements",
            "content": '''<document>
  <header title="Formatting Test"></header>
  <body>
    <paragraph>This has <bold>bold</bold>, <italic>italic</italic>, and <code>code</code> elements.</paragraph>
  </body>
</document>''',
            "expected_success": True
        },

        # Document with nested elements
        {
            "name": "Nested Elements",
            "content": '''<document>
  <header title="Nested Elements"></header>
  <body>
    <paragraph>This has <bold><italic>nested formatting</italic></bold> elements.</paragraph>
  </body>
</document>''',
            "expected_success": True
        },

        # Document with link and image
        {
            "name": "Links and Images",
            "content": '''<document>
  <header title="Links and Images"></header>
  <body>
    <paragraph>This has a <link href="https://example.com">link</link> and an <image src="image.jpg" alt="test image"></image>.</paragraph>
  </body>
</document>''',
            "expected_success": True
        },

        # Document with section
        {
            "name": "Sections",
            "content": '''<document>
  <header title="Sections Test"></header>
  <body>
    <section title="First Section">
      <paragraph>Section content.</paragraph>
      <section title="Nested Section">
        <paragraph>Nested section content.</paragraph>
      </section>
    </section>
  </body>
</document>''',
            "expected_success": True
        },

        # Document with code block
        {
            "name": "Code Block",
            "content": '''<document>
  <header title="Code Block Test"></header>
  <body>
    <codeblock>
function test() {
  return "This is a test";
}
    </codeblock>
  </body>
</document>''',
            "expected_success": True
        },

        # Document with list
        {
            "name": "List Elements",
            "content": '''<document>
  <header title="List Test"></header>
  <body>
    <list>
      <item>First item</item>
      <item>Item with <bold>formatting</bold></item>
      <item>Third item</item>
    </list>
  </body>
</document>''',
            "expected_success": True
        },

        # Document with special characters
        {
            "name": "Special Characters",
            "content": '''<document>
  <header title="Special &quot;Characters&quot;"></header>
  <body>
    <paragraph>Text with &lt;, &gt;, &amp;, &quot;, and &apos; characters.</paragraph>
  </body>
</document>''',
            "expected_success": True
        },

        # Very complex document with many elements
        {
            "name": "Complex Document",
            "content": '''<document>
  <header title="Complex Document" author="Test Author" date="2024-04-30"></header>
  <body>
    <paragraph>This is a <bold>test</bold> paragraph with <italic>multiple</italic> formatting.</paragraph>
    <section title="First Section">
      <paragraph>This section has a <link href="https://example.com">link</link>.</paragraph>
      <codeblock>
// Code block inside section
function example() {
  console.log("Hello world");
}
      </codeblock>
      <list>
        <item>First list item</item>
        <item>Second <italic>formatted</italic> item</item>
      </list>
    </section>
    <section title="Second Section">
      <paragraph>This section has an <image src="test.png" alt="test"></image> element.</paragraph>
      <section title="Nested Section">
        <paragraph>This is <bold><italic>deeply</italic></bold> nested.</paragraph>
      </section>
    </section>
  </body>
</document>''',
            "expected_success": True
        },

        # Invalid documents to test error handling

        # Missing document root
        {
            "name": "Missing Document Root",
            "content": '''<header title="Test"></header>
<body>
  <paragraph>Test</paragraph>
</body>''',
            "expected_success": False
        },

        # Missing header
        {
            "name": "Missing Header",
            "content": '''<document>
  <body>
    <paragraph>Test</paragraph>
  </body>
</document>''',
            "expected_success": False
        },

        # Missing body
        {
            "name": "Missing Body",
            "content": '''<document>
  <header title="Test"></header>
</document>''',
            "expected_success": False
        },

        # Missing title attribute
        {
            "name": "Missing Title",
            "content": '''<document>
  <header></header>
  <body>
    <paragraph>Test</paragraph>
  </body>
</document>''',
            "expected_success": False
        },

        # Malformed XML
        {
            "name": "Malformed XML",
            "content": '''<document>
  <header title="Test"</header>
  <body>
    <paragraph>Test</paragraph>
  </body>
</document>''',
            "expected_success": False
        },

        # Unclosed tags
        {
            "name": "Unclosed Tags",
            "content": '''<document>
  <header title="Test"></header>
  <body>
    <paragraph>Test
  </body>
</document>''',
            "expected_success": False
        }
    ]

    results = []
    for test_case in test_cases:
        result = test_parser(
            test_case["name"],
            test_case["content"],
            parser_path,
            test_case["expected_success"]
        )
        results.append(result)

    # Print summary
    print_color(YELLOW, "\n=== XML Parser Test Summary ===")
    total = len(results)
    passed = results.count(True)
    failed = results.count(False)

    print(f"Total tests: {total}")
    print_color(GREEN, f"Passed: {passed}")
    print_color(RED, f"Failed: {failed}")

    success_rate = (passed / total) * 100
    print(f"Success rate: {success_rate:.1f}%")

    if failed == 0:
        print_color(GREEN, "\nAll XML parser tests passed!")
        return 0
    else:
        print_color(RED, f"\n{failed} XML parser tests failed!")
        return 1

if __name__ == "__main__":
    # Check if executable exists
    parser_path = "./mypandoc"
    if not os.path.isfile(parser_path):
        print_color(RED, f"Error: {parser_path} not found")
        sys.exit(1)

    sys.exit(run_all_tests(parser_path))
