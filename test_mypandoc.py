#!/usr/bin/env python3
import os
import subprocess
import tempfile
import re
import sys
import json
import shutil
from xml.dom import minidom
from xml.parsers.expat import ExpatError

# ANSI colors
GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
BLUE = "\033[94m"
RESET = "\033[0m"
BOLD = "\033[1m"

# Test document values in different formats
TEST_DOCS = {
    "simple": {
        "xml": '''<document>
  <header title="Simple Test" author="Test User" date="2024-01-01"></header>
  <body>
    <paragraph>This is a simple test document.</paragraph>
  </body>
</document>''',
        "json": '''{
  "header": {
    "title": "Simple Test",
    "author": "Test User",
    "date": "2024-01-01"
  },
  "body": [
    {
      "paragraph": ["This is a simple test document."]
    }
  ]
}''',
        "markdown": '''---
title: Simple Test
author: Test User
date: 2024-01-01
---

This is a simple test document.
'''
    },
    "complex": {
        "xml": '''<document>
  <header title="Complex Test" author="Test Expert" date="2024-05-01"></header>
  <body>
    <section title="Introduction">
      <paragraph>This is a <bold>complex</bold> document with <italic>various</italic> formatting.</paragraph>
      <section title="Subsection">
        <paragraph>This is a subsection with <code>inline code</code>.</paragraph>
        <codeblock>function test() {
  return "Hello World";
}</codeblock>
      </section>
    </section>
    <section title="Conclusion">
      <paragraph>This is the <bold>conclusion</bold> section.</paragraph>
      <list>
        <item>First item</item>
        <item>Second item</item>
      </list>
    </section>
  </body>
</document>''',
        "json": '''{
  "header": {
    "title": "Complex Test",
    "author": "Test Expert",
    "date": "2024-05-01"
  },
  "body": [
    {
      "section": {
        "title": "Introduction",
        "content": [
          {
            "paragraph": ["This is a ", {"bold": ["complex"]}, " document with ", {"italic": ["various"]}, " formatting."]
          },
          {
            "section": {
              "title": "Subsection",
              "content": [
                {
                  "paragraph": ["This is a subsection with ", {"code": "inline code"}, "."]
                },
                {
                  "codeblock": "function test() {\n  return \"Hello World\";\n}"
                }
              ]
            }
          }
        ]
      }
    },
    {
      "section": {
        "title": "Conclusion",
        "content": [
          {
            "paragraph": ["This is the ", {"bold": ["conclusion"]}, " section."]
          },
          {
            "list": [
              {
                "item": ["First item"]
              },
              {
                "item": ["Second item"]
              }
            ]
          }
        ]
      }
    }
  ]
}'''
    }
}

# Create test directory
def setup_test_environment():
    """Set up test environment with example files"""
    print(f"{BLUE}Setting up test environment...{RESET}")
    
    if os.path.exists("test_mypandoc"):
        shutil.rmtree("test_mypandoc")
    
    os.makedirs("test_mypandoc", exist_ok=True)
    os.makedirs("test_mypandoc/inputs", exist_ok=True)
    os.makedirs("test_mypandoc/outputs", exist_ok=True)
    os.makedirs("test_mypandoc/expected", exist_ok=True)
    
    # Create test files
    for doc_type, formats in TEST_DOCS.items():
        for fmt, content in formats.items():
            with open(f"test_mypandoc/inputs/{doc_type}.{fmt}", "w") as f:
                f.write(content)
    
    # Create an empty file for error testing
    with open("test_mypandoc/inputs/empty.xml", "w") as f:
        pass
    
    # Create a malformed XML file
    with open("test_mypandoc/inputs/malformed.xml", "w") as f:
        f.write("<document><header title=\"Malformed\"></header><body><unclosed>")
    
    # Create a malformed JSON file
    with open("test_mypandoc/inputs/malformed.json", "w") as f:
        f.write("{\"header\": {\"title\": \"Malformed\"}, \"body\": [}")
    
    print(f"{GREEN}Test environment set up successfully!{RESET}")

def run_command(cmd, expect_error=False):
    """Run a command and return the result"""
    result = subprocess.run(cmd, capture_output=True, text=True)
    
    if expect_error:
        if result.returncode != 84:
            print(f"{RED}Expected error code 84, got {result.returncode}{RESET}")
            return False, result.stdout, result.stderr
        return True, result.stdout, result.stderr
    else:
        if result.returncode != 0:
            print(f"{RED}Command failed with code {result.returncode}{RESET}")
            print(f"Error: {result.stderr}")
            return False, result.stdout, result.stderr
        return True, result.stdout, result.stderr

def test_basic_conversion(input_file, input_format, output_format):
    """Test basic conversion from one format to another"""
    print(f"Testing conversion from {input_format} to {output_format}...")
    
    output_file = f"test_mypandoc/outputs/{os.path.basename(input_file).split('.')[0]}_{input_format}_to_{output_format}.{output_format}"
    
    cmd = ["./mypandoc", "-i", input_file, "-f", output_format, "-o", output_file]
    if input_format:
        cmd.extend(["-e", input_format])
    
    success, stdout, stderr = run_command(cmd)
    
    if success and os.path.exists(output_file):
        print(f"{GREEN}✓ Conversion successful{RESET}")
        return True, output_file
    else:
        print(f"{RED}✗ Conversion failed{RESET}")
        return False, None

def test_format_detection():
    """Test automatic format detection"""
    print(f"{BOLD}Testing format detection...{RESET}")
    
    formats = ["xml", "json", "markdown"]
    success_count = 0
    
    for fmt in formats:
        input_file = f"test_mypandoc/inputs/simple.{fmt}"
        output_file = f"test_mypandoc/outputs/detection_from_{fmt}.xml"
        
        print(f"Testing detection of {fmt} format...")
        cmd = ["./mypandoc", "-i", input_file, "-f", "xml", "-o", output_file]
        success, stdout, stderr = run_command(cmd)
        
        if success:
            success_count += 1
            print(f"{GREEN}✓ Successfully detected {fmt} format{RESET}")
        else:
            print(f"{RED}✗ Failed to detect {fmt} format{RESET}")
    
    print(f"{BOLD}Format detection: {success_count}/{len(formats)} tests passed{RESET}")
    return success_count == len(formats)

def test_error_handling():
    """Test error handling for various scenarios"""
    print(f"{BOLD}Testing error handling...{RESET}")
    
    tests = [
        {
            "name": "No arguments",
            "cmd": ["./mypandoc"],
            "expect_error": True
        },
        {
            "name": "Missing input file",
            "cmd": ["./mypandoc", "-f", "xml"],
            "expect_error": True
        },
        {
            "name": "Missing output format",
            "cmd": ["./mypandoc", "-i", "test_mypandoc/inputs/simple.xml"],
            "expect_error": True
        },
        {
            "name": "Non-existent input file",
            "cmd": ["./mypandoc", "-i", "nonexistent.xml", "-f", "json"],
            "expect_error": True
        },
        {
            "name": "Empty input file",
            "cmd": ["./mypandoc", "-i", "test_mypandoc/inputs/empty.xml", "-f", "json"],
            "expect_error": True
        },
        {
            "name": "Malformed XML",
            "cmd": ["./mypandoc", "-i", "test_mypandoc/inputs/malformed.xml", "-f", "json"],
            "expect_error": True
        },
        {
            "name": "Malformed JSON",
            "cmd": ["./mypandoc", "-i", "test_mypandoc/inputs/malformed.json", "-f", "xml"],
            "expect_error": True
        },
        {
            "name": "Invalid output format",
            "cmd": ["./mypandoc", "-i", "test_mypandoc/inputs/simple.xml", "-f", "invalid"],
            "expect_error": True
        },
        {
            "name": "Invalid input format",
            "cmd": ["./mypandoc", "-i", "test_mypandoc/inputs/simple.xml", "-f", "json", "-e", "invalid"],
            "expect_error": True
        }
    ]
    
    success_count = 0
    
    for test in tests:
        print(f"Testing error handling: {test['name']}...")
        success, stdout, stderr = run_command(test["cmd"], test["expect_error"])
        
        if success:
            success_count += 1
            print(f"{GREEN}✓ Error handling for {test['name']} works correctly{RESET}")
        else:
            print(f"{RED}✗ Error handling for {test['name']} failed{RESET}")
    
    print(f"{BOLD}Error handling: {success_count}/{len(tests)} tests passed{RESET}")
    return success_count == len(tests)

def test_stdin_stdout():
    """Test using standard input and output"""
    print(f"{BOLD}Testing standard input/output...{RESET}")
    
    input_file = "test_mypandoc/inputs/simple.xml"
    
    with open(input_file, "r") as f:
        content = f.read()
    
    cmd = ["./mypandoc", "-f", "json"]
    process = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    stdout, stderr = process.communicate(input=content)
    
    if process.returncode == 0 and stdout.strip():
        print(f"{GREEN}✓ Standard input/output test passed{RESET}")
        return True
    else:
        print(f"{RED}✗ Standard input/output test failed{RESET}")
        print(f"Error: {stderr}")
        return False

def test_all_conversions():
    """Test all possible format conversions"""
    print(f"{BOLD}Testing all format conversions...{RESET}")
    
    formats = ["xml", "json", "markdown"]
    success_count = 0
    total_tests = 0
    
    for input_format in formats:
        for output_format in formats:
            total_tests += 1
            input_file = f"test_mypandoc/inputs/simple.{input_format}"
            
            print(f"Converting {input_format} → {output_format}...")
            success, output_file = test_basic_conversion(input_file, input_format, output_format)
            
            if success:
                success_count += 1
    
    print(f"{BOLD}Format conversions: {success_count}/{total_tests} tests passed{RESET}")
    return success_count == total_tests

def test_complex_document():
    """Test handling of a complex document with nested structures"""
    print(f"{BOLD}Testing complex document handling...{RESET}")
    
    formats = ["xml", "json"]
    success_count = 0
    
    for input_format in formats:
        input_file = f"test_mypandoc/inputs/complex.{input_format}"
        
        for output_format in formats:
            if input_format != output_format:
                print(f"Converting complex document from {input_format} to {output_format}...")
                success, output_file = test_basic_conversion(input_file, input_format, output_format)
                
                if success:
                    success_count += 1
    
    print(f"{BOLD}Complex document handling: {success_count}/2 tests passed{RESET}")
    return success_count == 2

def test_roundtrip():
    """Test roundtrip conversion (XML to JSON to XML and vice versa)"""
    print(f"{BOLD}Testing roundtrip conversion...{RESET}")
    
    # XML -> JSON -> XML
    print("Testing XML -> JSON -> XML...")
    input_file = "test_mypandoc/inputs/simple.xml"
    json_output = "test_mypandoc/outputs/roundtrip_xml_to_json.json"
    xml_output = "test_mypandoc/outputs/roundtrip_json_to_xml.xml"
    
    cmd1 = ["./mypandoc", "-i", input_file, "-f", "json", "-o", json_output, "-e", "xml"]
    success1, stdout1, stderr1 = run_command(cmd1)
    
    if not success1:
        print(f"{RED}✗ First step of roundtrip test failed{RESET}")
        return False
    
    cmd2 = ["./mypandoc", "-i", json_output, "-f", "xml", "-o", xml_output, "-e", "json"]
    success2, stdout2, stderr2 = run_command(cmd2)
    
    if not success2:
        print(f"{RED}✗ Second step of roundtrip test failed{RESET}")
        return False
    
    # Compare input and output XML 
    with open(input_file, "r") as f:
        original_xml = f.read()
    
    with open(xml_output, "r") as f:
        final_xml = f.read()
    
    if normalize_xml(original_xml) == normalize_xml(final_xml):
        print(f"{GREEN}✓ XML -> JSON -> XML roundtrip test passed{RESET}")
        xml_success = True
    else:
        print(f"{RED}✗ XML -> JSON -> XML roundtrip test failed{RESET}")
        xml_success = False
    
    # JSON -> XML -> JSON
    print("Testing JSON -> XML -> JSON...")
    input_file = "test_mypandoc/inputs/simple.json"
    xml_output = "test_mypandoc/outputs/roundtrip_json_to_xml.xml"
    json_output = "test_mypandoc/outputs/roundtrip_xml_to_json.json"
    
    cmd1 = ["./mypandoc", "-i", input_file, "-f", "xml", "-o", xml_output, "-e", "json"]
    success1, stdout1, stderr1 = run_command(cmd1)
    
    if not success1:
        print(f"{RED}✗ First step of roundtrip test failed{RESET}")
        return False
    
    cmd2 = ["./mypandoc", "-i", xml_output, "-f", "json", "-o", json_output, "-e", "xml"]
    success2, stdout2, stderr2 = run_command(cmd2)
    
    if not success2:
        print(f"{RED}✗ Second step of roundtrip test failed{RESET}")
        return False
    
    # Compare input and output JSON
    with open(input_file, "r") as f:
        original_json = f.read()
    
    with open(json_output, "r") as f:
        final_json = f.read()
    
    if normalize_json(original_json) == normalize_json(final_json):
        print(f"{GREEN}✓ JSON -> XML -> JSON roundtrip test passed{RESET}")
        json_success = True
    else:
        print(f"{RED}✗ JSON -> XML -> JSON roundtrip test failed{RESET}")
        json_success = False
    
    return xml_success and json_success

def normalize_xml(xml_str):
    """Normalize XML by removing whitespace and standardizing format"""
    try:
        dom = minidom.parseString(xml_str)
        # Extract and normalize content - stripping whitespace and newlines
        normalized = re.sub(r'\s+', ' ', dom.toxml())
        return re.sub(r'> <', '><', normalized).strip()
    except ExpatError:
        return xml_str

def normalize_json(json_str):
    """Normalize JSON by parsing and re-stringifying to standardize format"""
    try:
        obj = json.loads(json_str)
        return json.dumps(obj, sort_keys=True)
    except json.JSONDecodeError:
        return json_str

def print_summary(results):
    """Print a summary of all test results"""
    print("\n" + "="*60)
    print(f"{BOLD}TEST SUMMARY{RESET}")
    print("="*60)
    
    total_passed = sum(1 for result in results.values() if result)
    total_tests = len(results)
    
    for test_name, passed in results.items():
        status = f"{GREEN}PASSED{RESET}" if passed else f"{RED}FAILED{RESET}"
        print(f"{test_name:30} {status}")
    
    print("-"*60)
    print(f"Total: {total_passed}/{total_tests} tests passed")
    
    if total_passed == total_tests:
        print(f"\n{GREEN}{BOLD}ALL TESTS PASSED! Your mypandoc implementation is working correctly.{RESET}")
    else:
        print(f"\n{RED}{BOLD}SOME TESTS FAILED. Review the failures and fix your implementation.{RESET}")

def main():
    """Run all tests"""
    print(f"{BOLD}{BLUE}MYPANDOC COMPREHENSIVE TEST SUITE{RESET}")
    print(f"{BLUE}==============================={RESET}\n")
    
    # Check if mypandoc binary exists
    if not os.path.exists("./mypandoc"):
        print(f"{RED}Error: mypandoc binary not found!{RESET}")
        print("Please run 'make' to build your project first.")
        return False
    
    # Setup test environment
    setup_test_environment()
    
    # Run all tests and collect results
    results = {
        "Basic Conversions": test_all_conversions(),
        "Format Detection": test_format_detection(),
        "Error Handling": test_error_handling(),
        "Complex Document Handling": test_complex_document(),
        "Roundtrip Conversion": test_roundtrip()
    }
    
    # Print summary
    print_summary(results)
    
    # Return success if all tests passed
    return all(results.values())

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)