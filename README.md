#  myPandoc - Document Converter

## What's This All About?
Hey there! Welcome to myPandoc, our cool and hip and swag and crazy and fun and rad take on the famous Pandoc document converter for the B-FUN-400 project. This nifty tool transforms documents between formats like it's no big deal! XML to Markdown? JSON to XML? No sweat!

## Cool Stuff It Does
- Magically converts documents between different formats
- Has its very own homemade parsing library
- Handles documents with all sorts of fancy elements
- Super friendly command-line interface that won't bite

## 📄 Formats It Speaks
- **Can Read**: XML, JSON, and Markdown
- **Can Write**: XML, JSON, and Markdown

## 📝 What's In A Document?
Documents in myPandoc's world have:
1. **Headers**
   - Title
   - Author
   - Date
   
2. **Content**
   - Plain text
   - Fancy formatting 
   - Links and images 
   - Paragraphs, sections, and code blocks 
   - Lists 

## Setting Up Shop
You'll need:
- GHC (that Haskell compiler thing)
- Stack (version 2.1.3+ because.)
- Make (make)

Getting started is a piece of cake:
```bash
git clone <wherever-the-link-is>
cd myPandoc
make
```

## How To Use This Thing

```
./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]
```

### The Command Decoder Ring:
- `-i ifile`: Where's your document hiding? (mandatory)
- `-f oformat`: What format do you want? (xml, json, markdown) (mandatory)
- `-o ofile`: Where should I put the result? 
- `-e iformat`: What format is your document in?

### Example Time!

Turn XML into Markdown (the cool way):
```bash
./mypandoc -i super-important-doc.xml -f markdown
```

JSON to XML conversion (for the XML enthusiasts):
```bash
./mypandoc -i data-blob.json -f xml -o now-its-xml.xml
```

When you want to be super explicit about everything:
```bash
./mypandoc -i mystery-file.txt -e markdown -f json -o look-ma-its-json.json
```

## When Things Go Wrong
If I get confused (invalid options, can't find files, etc.), I'll exit with code 84 and complain loudly to stderr. Don't worry, my error messages are friendly... mostly.

