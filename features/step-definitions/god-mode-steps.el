;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

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
