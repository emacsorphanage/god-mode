(require 'f)

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
