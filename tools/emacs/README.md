# Emacs plugin for LIGO

This plugin features syntax highlighting and `lsp-mode` support for PascaLIGO and CameLIGO.

For the LSP to work, you need to install `lsp-mode` and put `ligo-squirrel` executable in PATH.

## Automatic installation

Install the `ligo-mode` package from [MELPA](https://melpa.org). This is the recommended installation method.

## Manual installation

Put `ligo-mode.el` to the emacs load path, and add the following lines to your `init.el`:

```lisp
(add-to-list 'load-path "<LIGO_MODE_DIR>")
(add-to-list 'auto-mode-alist '("\\.mligo\\'" . ligo-caml-mode))
(autoload 'ligo-caml-mode "ligo-mode" "LIGO caml mode" t)
```

Alternatively, run `M-x update-directory-autoloads` against `<LIGO_MODE_DIR>`, outputting to `<LIGO_MODE_DIR>/ligo-mode-autoloads.el`, and then your config becomes:
```lisp
(add-to-list 'load-path "<LIGO_MODE_DIR>")
(load "<LIGO_MODE_DIR>/ligo-mode-autoloads.el")
```

# LSP support

For users of `lsp-mode`, setup can be performed automatically by using
`M-x ligo-setup-lsp`, or with the following snippet in an init file:

```lisp
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'ligo-mode
    (ligo-setup-lsp)))
```

# Development

## Tests

Tests can be run in two ways:

**Batch mode**
```bash
cd tools/emacs/
cask install # create virtual env and install test deps
cask emacs --batch -l tests/configuration-test.el -f ert-run-tests-batch-and-exit
```
Before it you should install [Cask](https://github.com/cask/cask/) utility that allows to setup isolated test environment.

**Interactive mode**

Unfortunately, not all tests work properly in batch mode (e.g. `functionality-test.el`) and for debugging of the package you may need to run tests manually from GUI e.g. interactively:

1. run Emacs editor
2. `M-x eval-buffer`
3. `M-x ert-run-tests-interactively` with `t` value in pop-up bar (that means run all tests)

NOTE: In interactive mode test runner just evaluate them in the state of your emacs editor so it is very fragile. If you want to be sure in testing results (e.g. before committing some changes) we recommend you restart Emacs editor and perform all steps again.
