#!/usr/bin/env python3

import os
import sys
import json
import subprocess
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
    """Create a test JSON file with the given content"""
    with open(filename, 'w') as f:
        f.write(content)
    return filename

def validate_json(content):
    """Validate that a string is valid JSON"""
    try:
        json.loads(content)
        return True, ""
    except json.JSONDecodeError as e:
        return False, str(e)

def test_parser(test_name, json_content, parser_path="./mypandoc", expected_success=True):
    """Test the JSON parser with a specific test case"""
    print_color(YELLOW, f"\n=== Testing JSON Parser: {test_name} ===")

    # Validate input JSON first
    is_valid_json, error = validate_json(json_content)
    if not is_valid_json and expected_success:
        print_color(RED, f"✗ Test input is not valid JSON: {error}")
        return False

    # Create temporary test file
    test_dir = Path("json_parser_tests")
    test_dir.mkdir(exist_ok=True)

    test_file = test_dir / f"{test_name.lower().replace(' ', '_')}.json"
    create_test_case(test_file, json_content)

    # Create temporary output file
    output_file = test_dir / f"{test_name.lower().replace(' ', '_')}_output.xml"

    # Run the parser
    cmd = f"{parser_path} -i {test_file} -f xml -o {output_file} -e json"
    stdout, stderr, exit_code = run_command(cmd)

    # Check if result matches expectations
    success = (exit_code == 0) if expected_success else (exit_code != 0)

    if success and expected_success:
        print_color(GREEN, f"✓ Parser successfully handled the test case")

        # Check if output file exists and has content
        if output_file.exists():
            with open(output_file, 'r') as f:
                output_content = f.read()

            print_color(CYAN, f"Output (first 150 chars):")
            print(output_content[:150] + "..." if len(output_content) > 150 else output_content)

            # Try converting back to JSON to test round-trip
            round_trip_file = test_dir / f"{test_name.lower().replace(' ', '_')}_roundtrip.json"
            cmd_roundtrip = f"{parser_path} -i {output_file} -f json -o {round_trip_file} -e xml"
            rt_stdout, rt_stderr, rt_exit_code = run_command(cmd_roundtrip)

            if rt_exit_code == 0:
                print_color(GREEN, f"✓ Round-trip conversion successful")

                # Validate the JSON structure
                with open(round_trip_file, 'r') as f:
                    rt_content = f.read()

                valid, error = validate_json(rt_content)
                if valid:
                    print_color(GREEN, f"✓ Round-trip JSON is valid")
                else:
                    print_color(RED, f"✗ Round-trip JSON is not valid: {error}")
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
    """Run all JSON parser tests"""
    test_cases = [
        # Basic valid documents
        {
            "name": "Basic Document",
            "content": '''{
  "header": {
    "title": "Basic Test"
  },
  "body": [
    "Simple paragraph."
  ]
}''',
            "expected_success": True
        },

        # Document with all header attributes
        {
            "name": "Full Header",
            "content": '''{
  "header": {
    "title": "Full Header",
    "author": "Test Author",
    "date": "2024-04-30"
  },
  "body": [
    "Test paragraph."
  ]
}''',
            "expected_success": True
        },

        # Document with paragraph formatting
        {
            "name": "Formatting Elements",
            "content": '''{
  "header": {
    "title": "Formatting Test"
  },
  "body": [
    {
      "paragraph": [
        "This has ",
        {"bold": ["bold"]},
        ", ",
        {"italic": ["italic"]},
        ", and ",
        {"code": "code"},
        " elements."
      ]
    }
  ]
}''',
            "expected_success": True
        },

        # Document with nested formatting
        {
            "name": "Nested Formatting",
            "content": '''{
  "header": {
    "title": "Nested Formatting"
  },
  "body": [
    {
      "paragraph": [
        "This has ",
        {"bold": [
          {"italic": ["nested formatting"]}
        ]},
        " elements."
      ]
    }
  ]
}''',
            "expected_success": True
        },

        # Document with links and images
        {
            "name": "Links and Images",
            "content": '''{
  "header": {
    "title": "Links and Images"
  },
  "body": [
    {
      "paragraph": [
        "This has a ",
        {
          "link": {
            "text": "link",
            "url": "https://example.com"
          }
        },
        " and an ",
        {
          "image": {
            "alt": "test image",
            "url": "image.jpg"
          }
        },
        "."
      ]
    }
  ]
}''',
            "expected_success": True
        },

        # Document with section
        {
            "name": "Sections",
            "content": '''{
  "header": {
    "title": "Sections Test"
  },
  "body": [
    {
      "section": {
        "title": "First Section",
        "contents": [
          {
            "paragraph": ["Section content."]
          },
          {
            "section": {
              "title": "Nested Section",
              "contents": [
                {
                  "paragraph": ["Nested section content."]
                }
              ]
            }
          }
        ]
      }
    }
  ]
}''',
            "expected_success": True
        },

        # Document with code block
        {
            "name": "Code Block",
            "content": '''{
  "header": {
    "title": "Code Block Test"
  },
  "body": [
    {
      "codeblock": "function test() {\\n  return \\"This is a test\\";\\n}"
    }
  ]
}''',
            "expected_success": True
        },

        # Document with list
        {
            "name": "List Elements",
            "content": '''{
  "header": {
    "title": "List Test"
  },
  "body": [
    {
      "list": [
        {
          "item": ["First item"]
        },
        {
          "item": ["Item with ", {"bold": ["formatting"]}]
        },
        {
          "item": ["Third item"]
        }
      ]
    }
  ]
}''',
            "expected_success": True
        },

        # Document with escaped characters
        {
            "name": "Escaped Characters",
            "content": '''{
  "header": {
    "title": "Escaped \\"Characters\\""
  },
  "body": [
    {
      "paragraph": ["Text with special characters: \\", \\\\, \\/, \\b, \\f, \\n, \\r, \\t"]
    }
  ]
}''',
            "expected_success": True
        },

        # Very complex document
        {
            "name": "Complex Document",
            "content": '''{
  "header": {
    "title": "Complex Document",
    "author": "Test Author",
    "date": "2024-04-30"
  },
  "body": [
    {
      "paragraph": [
        "This is a ",
        {"bold": ["test"]},
        " paragraph with ",
        {"italic": ["multiple"]},
        " formatting."
      ]
    },
    {
      "section": {
        "title": "First Section",
        "contents": [
          {
            "paragraph": [
              "This section has a ",
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
            "codeblock": "// Code block inside section\\nfunction example() {\\n  console.log(\\"Hello world\\");\\n}"
          },
          {
            "list": [
              {
                "item": ["First list item"]
              },
              {
                "item": ["Second ", {"italic": ["formatted"]}, " item"]
              }
            ]
          }
        ]
      }
    },
    {
      "section": {
        "title": "Second Section",
        "contents": [
          {
            "paragraph": [
              "This section has an ",
              {
                "image": {
                  "alt": "test",
                  "url": "test.png"
                }
              },
              " element."
            ]
          },
          {
            "section": {
              "title": "Nested Section",
              "contents": [
                {
                  "paragraph": [
                    "This is ",
                    {"bold": [{"italic": ["deeply"]}]},
                    " nested."
                  ]
                }
              ]
            }
          }
        ]
      }
    }
  ]
}''',
            "expected_success": True
        },

        # Invalid documents to test error handling

        # Missing header
        {
            "name": "Missing Header",
            "content": '''{
  "body": [
    "Test paragraph."
  ]
}''',
            "expected_success": False
        },

        # Missing body
        {
            "name": "Missing Body",
            "content": '''{
  "header": {
    "title": "Test"
  }
}''',
            "expected_success": False
        },

        # Missing title
        {
            "name": "Missing Title",
            "content": '''{
  "header": {
    "author": "Test Author"
  },
  "body": [
    "Test paragraph."
  ]
}''',
            "expected_success": False
        },

        # Invalid JSON
        {
            "name": "Invalid JSON",
            "content": '''{
  "header": {
    "title": "Test"
  },
  "body": [
    "Missing closing quote
  ]
}''',
            "expected_success": False
        },

        # Invalid array structure
        {
            "name": "Invalid Array",
            "content": '''{
  "header": {
    "title": "Test"
  },
  "body": [
    "First item",
    {
      "paragraph": ["Second item"
    }
  ]
}''',
            "expected_success": False
        },

        # Empty document
        {
            "name": "Empty JSON",
            "content": "{}",
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
    print_color(YELLOW, "\n=== JSON Parser Test Summary ===")
    total = len(results)
    passed = results.count(True)
    failed = results.count(False)

    print(f"Total tests: {total}")
    print_color(GREEN, f"Passed: {passed}")
    print_color(RED, f"Failed: {failed}")

    success_rate = (passed / total) * 100
    print(f"Success rate: {success_rate:.1f}%")

    if failed == 0:
        print_color(GREEN, "\nAll JSON parser tests passed!")
        return 0
    else:
        print_color(RED, f"\n{failed} JSON parser tests failed!")
        return 1

if __name__ == "__main__":
    # Check if executable exists
    parser_path = "./mypandoc"
    if not os.path.isfile(parser_path):
        print_color(RED, f"Error: {parser_path} not found")
        sys.exit(1)

    sys.exit(run_all_tests(parser_path))
