Ligo Syntax Highlighting
===
A tool to create syntax highlighting for CameLIGO and JsLIGO.

Usage
---
Change one of the definition files, CameLIGO.ml and JsLIGO.ml, as needed.

Run the following command:

```
dune exec tools/ligo-syntax-highlighting/LigoSyntaxHighlighting.exe -- --vim=tools/vim/ligo/start/ligo --emacs=tools/emacs --vscode=tools/lsp/vscode-plugin/syntaxes --textmate=tools/ligo-syntax-highlighting/textmate
```

This will place the files where needed.
