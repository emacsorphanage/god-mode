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

;; Patch god-mode fallback logic
(with-eval-after-load 'god-mode
  (defun god-mode-lookup-command (key-string)
    "Look up and return the command for KEY-STRING.
Tries fallback for the last key by removing Control modifier. If no command is found,
returns a keymap to allow further key input, or nil if completely unbound."
    (when key-string
      (let* ((key-vector (read-kbd-macro key-string t))
             (binding (key-binding key-vector)))
        (cond
         ;; Case 1: found an actual command
         ((commandp binding)
          (setq last-command-event (aref key-vector (1- (length key-vector))))
          binding)

         ;; Case 2: it's a keymap (partial key sequence)
         ((keymapp binding)
          (god-mode-lookup-key-sequence nil key-string))

         ;; Case 3: try fallback by removing C- from the last key
         ((string-match "\\(.*\\) C-\\(.\\)$" key-string)
          (let* ((prefix (match-string 1 key-string))
                 (last (match-string 2 key-string))
                 (fallback-key (string-trim (format "%s %s" prefix last)))
                 (fallback-vector (read-kbd-macro fallback-key t))
                 (fallback-binding (key-binding fallback-vector)))
            (cond
             ((commandp fallback-binding)
              (message "Using fallback command: %s" fallback-key)
              (setq last-command-event (aref fallback-vector (1- (length fallback-vector))))
              fallback-binding)

             ((keymapp fallback-binding)
              (message "Using fallback keymap: %s" fallback-key)
              (god-mode-lookup-key-sequence nil fallback-key))

             ;; Not a command nor a map — wait for more input, don't fail
             ))))))))

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
