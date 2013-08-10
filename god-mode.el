;;; god-mode.el --- God-like command entering minor mode

;; Copyright (C) 2013 Chris Done

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/god-mode
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library defines the following mapping:
;;
;; 1. All commands are assumed to be C-<something> unless otherwise
;;    indicated. Examples:
;;    * a    -> C-a
;;    * s    -> C-s
;;    * akny -> C-a C-k C-n C-y
;;
;; 2. `g' is a special key to indicate M-<something>. This means that
;;    there is no way to write `C-g' in this mode, you must therefore
;;    type `C-g' directly. Examples:
;;    * gf -> M-f
;;    * gx -> M-f
;;
;; 3. `x' is a special key to indicate C-x <something>. Examples:
;;    * xb -> C-x b
;;    * xh -> C-x h
;;
;; 4. `c' is a special key to indicate C-c <something>. Examples:
;;    * ca -> C-c a
;;    * cc -> C-c c
;;
;; 5. `h' is a special key to indicate C-h <something>. Examples:
;;    * hh -> C-h h
;;    * hf -> C-h f
;;
;; 6. There is a convention of uppercase special keys to indicate
;;    two modifier keys in action. Those are:
;;    * Gx -> C-M-x
;;    * Xs -> C-x C-s
;;    * Xx -> C-x C-x
;;    * Cc -> C-c C-c

;;; Code:

(defvar god-global-mode nil
  "Activate God mode on all buffers?")

(defun god-mode ()
  (interactive)
  "Activate/deactivate God mode."
  (setq god-global-mode (not god-global-mode))
  (if god-global-mode
      (god-local-mode 1)
    (god-local-mode -1)))

(defvar god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'god-mode-self-insert)
    map))

(define-minor-mode god-local-mode
  "Minor mode for running commands."
  :lighter " God")

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let ((key (this-command-keys)))
    (god-mode-interpret-key key)))

(defun god-mode-interpret-key (key)
  "Interpret the given key. This function sometimes recurses."
  (cond
   ((string-match "^[0-9]$" key)
    (let ((current-prefix-arg
           (if (numberp current-prefix-arg)
               (string-to-number (concat (number-to-string current-prefix-arg)
                                         key))
             (string-to-number key))))
      (god-mode-interpret-key (char-to-string (read-event (format "%d" current-prefix-arg))))))
   ((string= key "u")
    (let ((current-prefix-arg t))
      (god-mode-interpret-key (char-to-string (read-event "u")))))
   ((string= key " ") (god-mode-interpret-key "SPC"))
   ((string= key "g") (god-mode-try-command "g" "M-%s"))
   ((string= key "x") (god-mode-try-command "x" "C-x %s"))
   ((string= key "X") (god-mode-try-command "X" "C-x C-%s"))
   ((string= key "h") (god-mode-try-command "h" "C-h %s"))
   ((string= key "c") (god-mode-try-command "c" "C-c %s"))
   ((string= key "C") (god-mode-try-command "C" "C-c C-%s"))
   ((string= key "G") (god-mode-try-command "G" "C-M-%s"))
   (t
    (let* ((formatted (format "C-%s" key))
           (command (read-kbd-macro formatted))
           (binding (key-binding command)))
      (if (commandp binding)
          (call-interactively binding)
        (error "God: Unknown key binding for `%s`" formatted))))))

(defun god-mode-try-command (prompt format)
  "Try to run a command that takes additional key presses."
  (let* ((key (read-event prompt))
         (formatted (format format (char-to-string key)))
         (command (read-kbd-macro formatted))
         (binding (key-binding command)))
    (if (commandp binding)
        (call-interactively binding)
      (error "God: Unknown extended key binding for `%s`" formatted))))

(defadvice display-buffer (before god activate)
  (god-mode-activate))

(defadvice switch-to-buffer (before god activate)
  (god-mode-activate))

(defun god-mode-activate ()
  "Activate God mode locally on individual buffers when they are
somehow activated."
  (cond (god-global-mode
         (unless god-local-mode
           (god-local-mode 1)))
        (god-local-mode
         (god-local-mode -1))))

(ad-activate 'display-buffer)
(ad-activate 'switch-to-buffer)

(global-set-key (kbd "<escape>") 'god-local-mode)

(provide 'god-mode)

;;; god-mode.el ends here
