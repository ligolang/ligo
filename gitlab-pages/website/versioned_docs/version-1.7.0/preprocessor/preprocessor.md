---
id: preprocessor
title: Preprocessor
---

import Syntax from '@theme/Syntax';

A preprocessor is a tool that reads a text file and, if some special
instructions, called _preprocessing directives_, are found in the
input, the output may not be an identical copy, for example some parts
can be skipped. We document here the preprocessor shipped with the
LIGO compiler.

This preprocessor features different kinds of directives:

- directives found in the standard preprocessor for the language
  `C#`;

- a directive from `cpp`, the `C` preprocessor, enabling the textual
  inclusion of files;

- a directive specific to LIGO to support a minimal module system.

Importantly, strings and comments are handled the way `cpp` does ---
not `C#`.

In the following subsections, we shall briefly present those
directives. Here, we state some properties which hold for all of
them.

- They must start with a `#` symbol at the beginning of a line.

- Wrongly spelled directives or unsupported ones are ignored without
  warning, and therefore will appear in the output.

- They can have arguments in the form of free text or
  strings. (Anything after the directive name is considered a
  potential argument.)

- String arguments must be enclosed between double quotes and
  cannot span over two or more lines.

- The valid preprocessing of a directive leaves in its place an
  empty line (that is, a newline character) or another directive, to
  be picked up by other tools, like lexers.

- Newline characters are never discarded, to preserve the line
  numbers of copied text.
