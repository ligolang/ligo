Syntax Highlighting
===
A library to help with generating syntax highlighting. With one definition for 
a syntax, multiple editors can be targeted.

Currently the following editors are supported:
- Emacs
- VIM
- VSCode

Note that in the definition multiple regular expressions will need to be 
written, and currently regular expressions can't be shared among editors 
- which meansseparate regular expressions for EMacs, VIM, and VSCode. It's 
recommended to keep these regular expressions simple, as the idea is to 

Examples
---
See `tools/ligo-syntax-highlighting` for examples.

Developers
===
Some general information on syntax highlighting in editors.

Emacs syntax highlighting
---
Emacs uses [syntax tables](https://www.emacswiki.org/emacs/EmacsSyntaxTable) 
to determine the syntax class of a character. This can be a string delimiter, 
a comment start/end, a word, a symbol, among others. This is used before the 
actual syntax highlighting. 

For syntax highlighting, Emacs uses [font-lock](https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html). 

One rule can highlight different parts.

As Emacs has only a limited set of fonts, we also generate a few specific ones
for LIGO. 

Regexp limitations:
 - no lookahead/lookback
 - no recursion

Regexp information: https://www.emacswiki.org/emacs/RegularExpression

VIM syntax highlighting 
---
When different areas of a syntax highlighting rule need to be highlighted, 
the rule should be split into multiple rules. This is done with help of [nextgroup_](https://vimhelp.org/syntax.txt.html#%3Asyn-nextgroup) and [contained](https://vimhelp.org/syntax.txt.html#%3Asyn-contained). It's also the reason why the regexpes are split this way in the library.

See https://vimhelp.org/syntax.txt.html for more info on syntax highlighting.

Regexp:
- no recursion

See: https://vimhelp.org/syntax.txt.html

VS code syntax highlighting
---
Allows for nesting of syntax highlighting to be more precise, but we currently
don't use this due to the other editors.

To debug this, open a vs code instance with `tools/lsp/vscode-plugin` and start 
the debugger (F5).  

Regexp: 
- no recursion

See: https://macromates.com/manual/en/regular_expressions