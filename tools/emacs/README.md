# Emacs plugin for LIGO

This plugin features syntax highlighting and `lsp-mode` support for JsLIGO and CameLIGO.

For the LSP to work, you need to install `lsp-mode` and put `ligo` executable in PATH.

## Automatic installation

Install the `ligo-mode` package from [MELPA](https://melpa.org). This is the recommended installation method.

## Manual installation

Put `ligo-mode.el` to the emacs load path, and add the following lines to your `init.el`:

```lisp
(add-to-list 'load-path "<LIGO_MODE_DIR>")
(add-to-list 'auto-mode-alist '("\\.mligo\\'" . ligo-caml-mode))
(add-to-list 'auto-mode-alist '("\\.jsligo\\'" . ligo-javascript-mode))
(autoload 'ligo-caml-mode "ligo-mode" "CameLIGO mode" t)
(autoload 'ligo-javascript-mode "ligo-mode" "JsLIGO mode" t)
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

# Spacemacs configuration

For Spacemacs it's enough to add `ligo-mode` to `dotspacemacs-additional-packages` in `.spacemacs` file and enable `lsp` layer.

Also add following code into `dotspacemacs/user-config ()` to setup automatic load the plugin for `.mligo` files:
```lisp
(defun dotspacemacs/user-config ()
  ...
  (require 'lsp-mode)
  (ligo-setup-lsp)
  (add-hook 'ligo-caml-mode-hook #'lsp)
)
```

If you want load `ligo-mode.el` package from local path (for development or hacking) you can use `quelpa` (`ligo-mode` in `dotspacemacs-additional-packages` should be commented):

```lisp
(defun dotspacemacs/user-config ()
  ...
  (quelpa '(ligo-mode :fetcher file
                      :path "/home/user/ligo/tools/emacs/ligo-mode.el"))
  ...
)
```

# Development

## Tests

Tests can be run in two ways:

**Batch mode**
```bash
cd tools/emacs/
cask install # create virtual env and install test deps
cask emacs --batch -L . -l tests/configuration-test.el -f ert-run-tests-batch-and-exit
```
Before it you should install [Cask](https://github.com/cask/cask/) utility that allows to setup isolated test environment.

**Interactive mode**

Unfortunately, not all tests work properly in batch mode (e.g. `functionality-test.el`) and for debugging of the package you may need to run tests manually from GUI e.g. interactively:

1. run Emacs editor
2. `M-x eval-buffer`
3. `M-x ert-run-tests-interactively` with `t` value in pop-up bar (that means run all tests)

NOTE: In interactive mode test runner just evaluate them in the state of your emacs editor so it is very fragile. If you want to be sure in testing results (e.g. before committing some changes) we recommend you restart Emacs editor and perform all steps again.
