;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

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

(Given "^I view the units table$"
       (lambda ()
         (require 'calc-units)
         (math-build-units-table-buffer nil)))

(Given "^I start ielm$"
       (lambda ()
         (require 'ielm)
         (ielm)))

(When "I switch to buffer \"\\(.+\\)\""
      (lambda (buffer)
        (switch-to-buffer buffer)))

(Then "^god-mode is enabled$"
      (lambda ()
        (assert (not (null god-local-mode)))))

(Then "^god-mode is disabled$"
      (lambda ()
        (assert (null god-local-mode))))

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
          (assert (s-equals? expected actual) nil message expected actual))))
