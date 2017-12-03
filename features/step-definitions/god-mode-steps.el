;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I bind a named keyboard macro which kills line to C-c C-r$"
  (lambda ()
    (fset 'god-mode-test-keyboard-macro
          "\C-a\C-k\C-k")
    (global-set-key (kbd "C-c C-r") 'god-mode-test-keyboard-macro)))

(Given "^god-mode is enabled for all buffers$"
  (lambda ()
    (when (not god-global-mode)
      (god-mode))))

(Given "^I describe function \"\\(.+\\)\"$"
       (lambda (fn)
         (describe-function (intern fn))))

(Given "^I grep current directory"
       (lambda ()
         (set-process-query-on-exit-flag
          (get-buffer-process (grep "grep -Rin god ."))
          nil)))

(Given "^I open a view-mode buffer"
       (lambda ()
         (set-buffer (get-buffer-create "*view-mode-buffer*"))
         (view-mode)
         (god-local-mode 0)
         (god-mode-maybe-activate)))

(Given "^I open a test-special-mode buffer"
       (lambda ()
         (set-buffer (get-buffer-create "*test-special-mode*"))
         (test-special-mode)))

(Given "^I start ielm$"
       (lambda ()
         (require 'ielm)
         (ielm)))

(When "I switch to buffer \"\\(.+\\)\""
      (lambda (buffer)
        (switch-to-buffer buffer)))

(When "I send the key sequence \"\\(.+\\)\""
      (lambda (keys)
        (execute-kbd-macro (kbd keys))))

(Then "^god-mode is enabled$"
      (lambda ()
        (cl-assert (not (null god-local-mode)))))

(Then "^god-mode is disabled$"
      (lambda ()
        (cl-assert (null god-local-mode))))

(Then "^I have god-mode on$"
      "Turn god-mode on."
      (lambda ()
        (god-local-mode 1)))

(Then "^I am in insertion mode$"
      "Turn god-mode off."
      (lambda ()
        (god-local-mode -1)))

(Then "^the buffer's contents should be\\(?: \"\\(.+\\)\"\\|:\\)$"
      "Asserts that the current buffer includes some text.

Examples:
 - Then the buffer's contents should be  \"CONTENTS\"
 - Then the buffer's contents should be:
     \"\"\"
     CONTENTS
     \"\"\""
      (lambda (expected)
        (let ((actual (buffer-string))
              (message "Expected buffer's contents to be '%s', but was '%s'"))
          (cl-assert (s-equals? expected actual) nil message expected actual))))
