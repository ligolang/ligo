(require 'ert)
(require 'lsp-mode)
(require 'ligo-mode)

(ligo-setup-lsp)

;; for debug
;; (setq ert-batch-backtrace-right-margin 300)

(defun with-lsp-test-file (path &rest body)
  "Wrapper that open file, run lsp server, evaluate `code' and close file"
  (progn
     (find-file path)
     (lsp-mode)
     (ligo-caml-mode)
     (lsp-workspace-folders-add ".")
     (lsp)
     (lsp-test-wait body)
     ;; (lsp-test-wait (lsp-workspace-folders-remove "."))
     (kill-buffer)
    )
  )

;; (macroexpand '(with-lsp-test-file "a.txt" (goto-char 5) (lsp-rename "xx")))

;; Configuration tests

(ert-deftest ligo-auto-mode-alist-test ()
  "Check that file extensions are properly binded to according mode"
  (should (member '("\\.mligo\\'" . ligo-caml-mode) auto-mode-alist))
  )

(ert-deftest ligo-squirrel-bin-test ()
  "Check that LSP server is set"
  (should (file-executable-p (executable-find ligo-squirrel-bin)))
  )

(ert-deftest ligo-setup-lsp-test ()
  "Check that `setup-lsp-test' works properly"
  (should (member '(ligo-caml-mode . "ligo") lsp-language-id-configuration))
  (should (gethash 'ligo lsp-clients))
  )
