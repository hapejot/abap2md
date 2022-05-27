# Pandoc

The program pandoc can be used to convert the markdown files
into something other. For example into a word document.

## Create a word document

generating a template file:

        pandoc -o custom-reference.docx --print-default-data-file reference.docx

using an existing template with headers and footers:

        pandoc -i doc/main.md --reference-doc inwerken.docx  -o main.docx
