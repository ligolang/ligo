Ligo Syntax Highlighting
===
A tool to create syntax highlighting for ReasonLIGO, CameLIGO, JsLIGO and PascaLIGO. 

Usage
---
Change one of the definition files, CameLIGO.ml, ReasonLIGO.ml, JsLIGO.ml and PascaLIGO.ml, as needed.

Run the following command:

```
dune exec tools/ligo-syntax-highlighting/LigoSyntaxHighlighting.exe -- --vim=tools/vim/ligo/start/ligo/syntax --emacs=tools/emacs --vscode=tools/lsp/vscode-plugin/syntaxes --textmate=tools/ligo-syntax-highlighting/textmate
```

This will place the files where needed.
