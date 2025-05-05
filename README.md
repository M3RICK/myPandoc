# myPandoc - Document Converter

## What's This?

Welcome to **myPandoc**, our creative take on the legendary Pandoc converter for the **B-FUN-400** project. It’s a lightweight tool that effortlessly converts documents between XML, JSON, and Markdown.

## Features

- Converts between **XML**, **JSON**, and **Markdown**
- Custom-built **parsing library**
- Handles headers, text, formatting, code blocks, lists, links, and more
- Friendly command-line interface
- Clear error reporting (with exit code `84`)

## 📄 Supported Formats

| Can Read   | Can Write  |
|------------|------------|
| XML        | XML        |
| JSON       | JSON       |
| Markdown   | Markdown   |

## Document Structure

Each document can contain:

- **Headers**: Title, Author, Date
- **Content**:
  - Plain and formatted text
  - Links, images
  - Lists (ordered/unordered)
  - Paragraphs, sections, code blocks

## Setup

You'll need:

- [GHC](https://www.haskell.org/ghc/)
- [Stack ≥ 2.1.3](https://docs.haskellstack.org/)
- `make`

### Clone and Build

```bash
git clone <your-repo-url>
cd myPandoc
make
```

## How To Use This

```
./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]
```

### The Command Decoder:
- `-i ifile`: Where's your document hiding? (mandatory)
- `-f oformat`: What format do you want? (xml, json, markdown) (mandatory)
- `-o ofile`: Where should I put the result? 
- `-e iformat`: What format is your document in?

### Example Time!

Turn XML into Markdown):
```bash
./mypandoc -i super-important-doc.xml -f markdown
```

JSON to XML conversion:
```bash
./mypandoc -i data-blob.json -f xml -o now-its-xml.xml
```

When you want to be super explicit about everything:
```bash
./mypandoc -i mystery-file.txt -e markdown -f json -o look-ma-its-json.json
```

## When Things Go Wrong
If I get confused (invalid options, can't find files, etc.), I'll exit with code 84 and complain loudly to stderr. Don't worry, my error messages are friendly... mostly.

