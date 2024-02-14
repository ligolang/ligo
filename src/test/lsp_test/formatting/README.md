# Writing formatter tests
Note, that all paths that you use in formatting tests should be relative to the `dune` file path (i.e. to `src/test/lsp_test/formatting`).

## File could be formatted
1. Provide 2 files: file to format and the formatted result.
2. Add to the `dune` file the next lines:
```lisp
(rule
 (targets formatted.lang_output)
 (action
  (run
   ./formatting_test.exe
   path/to/formatted/file.lang
   formatted.lang_output))
 (deps path/to/formatted/file.lang))

(rule
 (alias runtest)
 (action
  (diff path/to/formatted/file.lang formatted.lang_output)))
```
Don't forget to run `dune promote` after running formatter tests.

## File couldn't be formatted
1. Provide a file that couldn't be formatted.
2. Add to the `dune` file the next lines:
```lisp
(rule
 (alias runtest)
 (action
  (run ./formatting_test.exe path/to/non_formattable/file.lang))
 (deps path/to/non_formattable/file.lang))
```
