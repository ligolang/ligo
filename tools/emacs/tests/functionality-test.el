(require 'ert)
(require 'deferred)
(require 'lsp-mode)
(require 'ligo-mode)

(ligo-setup-lsp)

;; For debug

(setq ert-batch-backtrace-right-margin 300)

(defun print-all-buffers ()
  "Debug helper"
  (dolist (x (buffer-list))
    (with-current-buffer x
      (print x)
      (print (buffer-string))))
  )

;; Functionality tests
;; TODO: currently running in batch mode is broken. See experiments in the rest of file


(lsp-workspace-folders-add "./sample-project/")

(defmacro with-lsp (name &rest body)
  `(progn
     (with-current-buffer (find-file (concat "./sample-project/"
                                             ,name))
       (lsp-mode)
       (ligo-caml-mode)
       (lsp)
       ,@body
       (revert-buffer t t) ;; TODO: with-current-buffer kind of uselses
       )
     (kill-this-buffer)
     )
  )

(ert-deftest ligo-rename-test ()
  (with-lsp "simple.mligo"
    (goto-char 5)
    (lsp-rename "y")
    (should (string= (lsp--buffer-content)
                     (concat "let y : int = 10\n"
                             "let y = y\n")))
    (lsp-rename "x")
    (should (string= (lsp--buffer-content)
                     (concat "let x : int = 10\n"
                             "let y = x\n")))
    )
  )

(ert-deftest ligo-find-definition-test ()
  (with-lsp "simple.mligo"
    (goto-char 26)
    (lsp-find-definition)
    (should (= (point) 5))
    (lsp-find-definition)
    (should (= (point) 5))
    )
  )

;; TODO: we need async request for it
;; (ert-deftest ligo-hover-test ()
;;   (with-lsp "simple.mligo"
;;             (goto-char 5)
;;             (print (lsp-hover))))

(ert-deftest ligo-extract-type-definition-test ()
  (with-lsp "simple.mligo"
    (goto-char 9)
    (lsp-execute-code-action (lsp--select-action (lsp-code-actions-at-point)))
    (should (string= (lsp--buffer-content)
                     (concat "type extractedTypeName = int\n"
                             "let x : extractedTypeName = 10\n"
                             "let y = x\n")))
    )
  )

;; Experiments with running tests in batch mode

;; Tests can be run in batch mode with following command:
;; EMACS=emacs-29.2 cask emacs --batch -l tests/functionality-test.el -f ert-run-tests-batch-and-exit

;; Unfortunately for some reason it fails because lsp server is not initialized yet when the
;; request (like lsp-rename) is coming to it. And any attempts to wait for server doesn't succeed
;; The main complication is that `lsp-mode' is asynchronous (`deferred' library is used)
;; Below are some experiments

;; Maybe we should create an issue in `lsp-mode' github about this or ask help with configuration
;; their helper functions, but now we postpone it..

;; Loop until lsp server become `initialized'
;; Problem: sometimes it lock async tasks of lsp server and test process cannot quit the loop
;;          sometimes it completes loop but lsp server still fail with `(split-string nil "\\.")'
(defun loop-until-ready ()
  (while (not (eq 'initialized (lsp--workspace-status (cl-first (lsp-workspaces)))))
    ()
    )
  (print (lsp--workspace-status (cl-first (lsp-workspaces))))
  )

;; (ert-deftest ligo-rename-experiment-test ()
;;   "TODO"
;;   (with-current-buffer (find-file "./tests/sample-project/simple.mligo")
;;     (progn
;;     (lsp-mode)
;;     (ligo-caml-mode)
;;     (lsp-workspace-folders-add ".")
;;     (lsp)
;;     ;; (deferred:flush-queue!)
;;     (deferred:$
;;       ;; NB: we tried to use some test util from `lsp-mode' package:
;;       ;; https://github.com/emacs-lsp/lsp-mode/blob/master/test/lsp-integration-test.el#L54
;;       ;; (lsp-test-wait ;; error: (eval form)
;;       ;;  (eq 'initialized (lsp--workspace-status
;;       ;;                    (cl-first (lsp-workspaces)))))
;;       (deferred:next (lambda () (print "Message!")))
;;       ;; (deferred:nextc it (lambda () (loop-until-ready))) ;; see commend for `loop-until-ready'
;;       ;; NB: For some reason any attempts to wait for sometimes cause errors in `lsp-mode'
;;       ;; (deferred:nextc it (lambda () (sleep-for 3)))
;;       ;; (deferred:nextc it (lambda () (deferred:wait 3000)))
;;       (deferred:nextc it
;;         (lambda ()
;;           (goto-char 5)
;;           (lsp-rename "xx")
;;           (should (equal (lsp--buffer-content)
;;                          (concat "let xx : int = 10\n"
;;                                  "let y = xx\n")))))
;;       (deferred:sync! it)
;;       )
;;     )
;;     )
;;    )
