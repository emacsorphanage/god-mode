(require 'f)
(require 'cl-lib)

(defvar god-mode-support-path
  (f-dirname load-file-name))

(defvar god-mode-features-path
  (f-parent god-mode-support-path))

(defvar god-mode-root-path
  (f-parent god-mode-features-path))

(add-to-list 'load-path god-mode-root-path)

(define-derived-mode test-special-mode fundamental-mode "Test"
  "This mode is marked special via its mode class rather than
through inheritance.")

(put 'test-special-mode 'mode-class 'special)

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
