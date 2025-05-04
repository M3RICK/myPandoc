#!/usr/bin/env python3
import os
import subprocess
import json
import xml.dom.minidom as minidom
import tempfile
import sys
from xml.parsers.expat import ExpatError

# ANSI colors for better output
GREEN = "\033[92m"
RED = "\033[91m"
YELLOW = "\033[93m"
BLUE = "\033[94m"
RESET = "\033[0m"
BOLD = "\033[1m"

def run_command(cmd, input_data=None):
    """Run a command and return stdout, stderr, and return code"""
    process = subprocess.Popen(
        cmd, 
        stdin=subprocess.PIPE if input_data else None,
        stdout=subprocess.PIPE, 
        stderr=subprocess.PIPE,
        text=True
    )
    
    stdout, stderr = process.communicate(input=input_data)
    return stdout, stderr, process.returncode

def create_test_files():
    """Create temporary test files for each format"""
    print(f"{BLUE}Creating test files...{RESET}")
    
    # Simple document for testing
    test_files = {}
    
    # XML test file
    xml_content = """<document>
  <header title="Test Document" author="Test Author" date="2024-05-04"></header>
  <body>
    <paragraph>This is a <bold>test</bold> document with <italic>formatting</italic>.</paragraph>
    <section title="Section 1">
      <paragraph>This is section 1 content.</paragraph>
      <codeblock>def hello():
    print("Hello, world!")
</codeblock>
    </section>
    <section title="Section 2">
      <paragraph>This is <code>section 2</code> content.</paragraph>
      <list>
        <item>Item 1</item>
        <item>Item 2</item>
      </list>
    </section>
  </body>
</document>"""
    
    xml_file = "test_xml.xml"
    with open(xml_file, "w") as f:
        f.write(xml_content)
    test_files["xml"] = xml_file
    
    # JSON test file
    json_content = """{
  "header": {
    "title": "Test Document",
    "author": "Test Author",
    "date": "2024-05-04"
  },
  "body": [
    {
      "paragraph": ["This is a ", {"bold": ["test"]}, " document with ", {"italic": ["formatting"]}, "."]
    },
    {
      "section": {
        "title": "Section 1",
        "content": [
          {
            "paragraph": ["This is section 1 content."]
          },
          {
            "codeblock": "def hello():\\n    print(\\"Hello, world!\\")"
          }
        ]
      }
    },
    {
      "section": {
        "title": "Section 2",
        "content": [
          {
            "paragraph": ["This is ", {"code": "section 2"}, " content."]
          },
          {
            "list": [
              {"item": ["Item 1"]},
              {"item": ["Item 2"]}
            ]
          }
        ]
      }
    }
  ]
}"""
    
    json_file = "test_json.json"
    with open(json_file, "w") as f:
        f.write(json_content)
    test_files["json"] = json_file
    
    # Markdown test file
    md_content = """---
title: Test Document
author: Test Author
date: 2024-05-04
---

This is a **test** document with *formatting*.

## Section 1

This is section 1 content.

```
def hello():
    print("Hello, world!")
```

## Section 2

This is `section 2` content.

- Item 1
- Item 2
"""
    
    md_file = "test_markdown.md"
    with open(md_file, "w") as f:
        f.write(md_content)
    test_files["markdown"] = md_file
    
    return test_files

def test_format_conversion(binary, src_format, dest_format, test_files):
    """Test conversion from one format to another"""
    input_file = test_files[src_format]
    output_file = f"output_{src_format}_to_{dest_format}.{dest_format}"
    
    print(f"\nTesting {src_format} → {dest_format}:")
    
    cmd = [binary, "-i", input_file, "-f", dest_format, "-o", output_file, "-e", src_format]
    stdout, stderr, return_code = run_command(cmd)
    
    if return_code != 0:
        print(f"{RED}✗ Conversion failed with code {return_code}{RESET}")
        print(f"{RED}Error: {stderr}{RESET}")
        return False
    
    if not os.path.exists(output_file):
        print(f"{RED}✗ Output file not created{RESET}")
        return False
    
    # Validate output format
    is_valid = validate_format(output_file, dest_format)
    if is_valid:
        print(f"{GREEN}✓ Conversion successful and valid{RESET}")
    else:
        print(f"{RED}✗ Output is not valid {dest_format.upper()}{RESET}")
    
    return is_valid

def validate_format(file_path, format_name):
    """Validate that the file is in the correct format"""
    try:
        with open(file_path, "r") as f:
            content = f.read()
            
        if format_name == "xml":
            # Try parsing as XML
            try:
                minidom.parseString(content)
                return True
            except ExpatError:
                return False
                
        elif format_name == "json":
            # Try parsing as JSON
            try:
                json.loads(content)
                return True
            except json.JSONDecodeError:
                return False
                
        elif format_name == "markdown":
            # Basic check for markdown (has title section)
            return content.startswith("---") and "title:" in content.split("---")[1]
            
        return True
    except Exception as e:
        print(f"{RED}Error validating format: {str(e)}{RESET}")
        return False

def test_roundtrip(binary, format1, format2, test_files):
    """Test roundtrip conversion: format1 -> format2 -> format1"""
    print(f"\n{BOLD}Testing roundtrip: {format1} → {format2} → {format1}{RESET}")
    
    # First conversion: format1 -> format2
    input_file = test_files[format1]
    intermediate_file = f"temp_{format1}_to_{format2}.{format2}"
    
    cmd1 = [binary, "-i", input_file, "-f", format2, "-o", intermediate_file, "-e", format1]
    stdout1, stderr1, return_code1 = run_command(cmd1)
    
    if return_code1 != 0:
        print(f"{RED}✗ First conversion failed with code {return_code1}{RESET}")
        print(f"{RED}Error: {stderr1}{RESET}")
        return False
    
    # Second conversion: format2 -> format1
    final_file = f"temp_{format2}_to_{format1}.{format1}"
    cmd2 = [binary, "-i", intermediate_file, "-f", format1, "-o", final_file, "-e", format2]
    stdout2, stderr2, return_code2 = run_command(cmd2)
    
    if return_code2 != 0:
        print(f"{RED}✗ Second conversion failed with code {return_code2}{RESET}")
        print(f"{RED}Error: {stderr2}{RESET}")
        return False
    
    # Validate both files
    is_valid1 = validate_format(intermediate_file, format2)
    is_valid2 = validate_format(final_file, format1)
    
    # Compare original and final content structurally (not exact match)
    structure_preserved = compare_structure(input_file, final_file, format1)
    
    if is_valid1 and is_valid2 and structure_preserved:
        print(f"{GREEN}✓ Roundtrip successful and structure preserved{RESET}")
        return True
    else:
        if not is_valid1:
            print(f"{RED}✗ Intermediate file is not valid {format2.upper()}{RESET}")
        if not is_valid2:
            print(f"{RED}✗ Final file is not valid {format1.upper()}{RESET}")
        if not structure_preserved:
            print(f"{RED}✗ Document structure not preserved in roundtrip{RESET}")
        return False

def compare_structure(file1, file2, format_name):
    """Compare structure of two files in the same format"""
    try:
        with open(file1, "r") as f:
            content1 = f.read()
        with open(file2, "r") as f:
            content2 = f.read()
            
        if format_name == "xml":
            # For XML, normalize and compare DOM structure
            dom1 = minidom.parseString(content1)
            dom2 = minidom.parseString(content2)
            # Simple structure comparison - count tags
            return len(dom1.getElementsByTagName("*")) == len(dom2.getElementsByTagName("*"))
            
        elif format_name == "json":
            # For JSON, compare structure by key presence
            obj1 = json.loads(content1)
            obj2 = json.loads(content2)
            # Check header and body
            if "header" not in obj2 or "body" not in obj2:
                return False
            # Check title
            if obj1["header"].get("title") != obj2["header"].get("title"):
                return False
            # Check body length is same
            return len(obj1["body"]) == len(obj2["body"])
            
        elif format_name == "markdown":
            # For markdown, check title and section count
            sections1 = content1.count("##")
            sections2 = content2.count("##")
            return sections1 == sections2
            
        return True
    except Exception as e:
        print(f"{RED}Error comparing structure: {str(e)}{RESET}")
        return False

def test_error_handling(binary):
    """Test various error conditions"""
    print(f"\n{BOLD}Testing error handling:{RESET}")
    
    test_cases = [
        {
            "name": "Missing input file",
            "cmd": [binary, "-i", "nonexistent.xml", "-f", "json"],
            "expected_code": 84
        },
        {
            "name": "Invalid input format",
            "cmd": [binary, "-i", "test_xml.xml", "-f", "json", "-e", "invalid"],
            "expected_code": 84
        },
        {
            "name": "Invalid output format",
            "cmd": [binary, "-i", "test_xml.xml", "-f", "invalid"],
            "expected_code": 84
        },
        {
            "name": "Missing required arguments",
            "cmd": [binary],
            "expected_code": 84
        },
        {
            "name": "Missing input file argument",
            "cmd": [binary, "-f", "json"],
            "expected_code": 84
        },
        {
            "name": "Missing format argument",
            "cmd": [binary, "-i", "test_xml.xml"],
            "expected_code": 84
        }
    ]
    
    success_count = 0
    for test in test_cases:
        stdout, stderr, return_code = run_command(test["cmd"])
        
        if return_code == test["expected_code"]:
            print(f"{GREEN}✓ {test['name']}: correct error code {return_code}{RESET}")
            success_count += 1
        else:
            print(f"{RED}✗ {test['name']}: expected code {test['expected_code']}, got {return_code}{RESET}")
    
    print(f"\n{BOLD}Error handling: {success_count}/{len(test_cases)} tests passed{RESET}")
    return success_count == len(test_cases)

def clean_up_test_files(test_files):
    """Clean up test files"""
    print(f"\n{BLUE}Cleaning up test files...{RESET}")
    
    for file_path in test_files.values():
        if os.path.exists(file_path):
            os.remove(file_path)
    
    # Clean up output files
    for file_name in os.listdir('.'):
        if file_name.startswith(('output_', 'temp_')):
            os.remove(file_name)

def main():
    """Main test function"""
    binary = "./mypandoc"
    
    # Check if binary exists
    if not os.path.exists(binary):
        print(f"{RED}Error: {binary} not found. Please build the project first.{RESET}")
        return 1
    
    print(f"{BOLD}{BLUE}MyPandoc Comprehensive Test Suite{RESET}")
    print(f"{BLUE}==============================={RESET}")
    
    # Create test files
    test_files = create_test_files()
    
    # Run tests
    results = {}
    
    # Test basic conversions
    formats = ["xml", "json", "markdown"]
    for src_format in formats:
        for dest_format in formats:
            test_name = f"{src_format}_to_{dest_format}"
            results[test_name] = test_format_conversion(binary, src_format, dest_format, test_files)
    
    # Test roundtrip conversions
    results["roundtrip_xml_json"] = test_roundtrip(binary, "xml", "json", test_files)
    results["roundtrip_json_xml"] = test_roundtrip(binary, "json", "xml", test_files)
    results["roundtrip_xml_markdown"] = test_roundtrip(binary, "xml", "markdown", test_files)
    results["roundtrip_json_markdown"] = test_roundtrip(binary, "json", "markdown", test_files)
    
    # Test error handling
    results["error_handling"] = test_error_handling(binary)
    
    # Print summary
    print("\n" + "="*60)
    print(f"{BOLD}TEST SUMMARY{RESET}")
    print("="*60)
    
    passed = 0
    total = len(results)
    
    for test_name, result in results.items():
        status = f"{GREEN}PASSED{RESET}" if result else f"{RED}FAILED{RESET}"
        print(f"{test_name:25} {status}")
        if result:
            passed += 1
    
    print("-"*60)
    print(f"Total: {passed}/{total} tests passed")
    
    if passed == total:
        print(f"\n{GREEN}{BOLD}ALL TESTS PASSED! Your mypandoc implementation is working correctly.{RESET}")
    else:
        print(f"\n{YELLOW}{BOLD}SOME TESTS FAILED. Your implementation might need improvements.{RESET}")
    
    # Clean up
    clean_up_test_files(test_files)
    
    return 0 if passed == total else 1

if __name__ == "__main__":
    sys.exit(main())
