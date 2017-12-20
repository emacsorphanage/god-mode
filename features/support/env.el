(require 'f)
(require 'cl-lib)

;; From the espuds readme
(when (and (= emacs-major-version 25))
  (require 'cl-preloaded)
  (setf (symbol-function 'cl--assertion-failed)
        (lambda (form &optional string sargs args)
          "This function has been modified by ecukes to fix problems with cl-assert in emacs 25.
           The modified version should only be used for running espuds tests."
          (if string
              (apply #'error string (append sargs args))
            (signal 'cl-assertion-failed `(,form ,@sargs))))))

(defvar god-mode-support-path
  (f-dirname load-file-name))

(defvar god-mode-features-path
  (f-parent god-mode-support-path))

(defvar god-mode-root-path
  (f-parent god-mode-features-path))

(add-to-list 'load-path god-mode-root-path)

(require 'god-mode)
(require 'espuds)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
