;;; god-mode.el --- God-like command entering minor mode

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2013 Magnar Sveen
;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/god-mode
;; Version: 2.7.2

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
;;   * `x s`  → `C-x s`
;;
;;   Note the use of space to produce `C-x s`.
;;
;; 2. `g' is a special key to indicate M-<something>. This means that
;;    there is no way to write `C-g' in this mode, you must therefore
;;    type `C-g' directly. Examples:
;;    * gf -> M-f
;;    * gx -> M-f
;;
;;    This key can be configured.
;;
;; 6. There is a convention of uppercase special keys to indicate
;;    two modifier keys in action. Those are:
;;    * Gx -> C-M-x
;;
;; 7. There is a literal interpretation key as `l' that can
;;    be used on chained commands, e.g.
;;
;;    * xs -> C-x C-s
;;    * xlb -> C-x b
;;    * xlh -> C-x h
;;    * xp -> C-x C-p
;;    * xx -> C-x C-x
;;
;;    This key can be configured.
;;
;; 8. There is a key (default `i') to disable God mode, similar to
;;    Vim's i.

;;; Code:

(defcustom god-literal-key
  " "
  "The key used for literal interpretation."
  :group 'god
  :type 'string)

(defcustom god-exempt-major-modes
  '(dired-mode
    help-mode
    grep-mode
    magit-log-edit-mode
    magit-status-mode
    vc-annotate-mode
    package-menu-mode
    Buffer-menu-mode)
  "List of major modes that should not start in god-local-mode."
  :group 'god
  :type '(function))

(defvar god-global-mode nil
  "Activate God mode on all buffers?")

(defvar god-mode-last-command nil
  "Last command ran. Used for repetition.")

(defvar god-mode-last-prefix-arg nil
  "Last previous arg, for repetition.")

;;;###autoload
(defun god-mode ()
  "Toggle global God mode."
  (interactive)
  (setq god-global-mode (not god-global-mode))
  (if god-global-mode
      (god-local-mode 1)
    (god-local-mode -1)))

(defvar god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'god-mode-self-insert)
    (define-key map (kbd "g") 'god-mode-meta)
    (define-key map (kbd "G") 'god-mode-control-meta)
    (define-key map (kbd "z") 'god-mode-repeat)
    (define-key map (kbd "i") 'god-local-mode)
    map))

;;;###autoload
(define-minor-mode god-local-mode
  "Minor mode for running commands."
  nil " God" god-local-mode-map
  (if god-local-mode
      (run-hooks 'god-mode-enabled-hook)
    (run-hooks 'god-mode-disabled-hook)))

(defun god-mode-meta ()
  "The command for M-."
  (interactive)
  (god-mode-try-command "M-" "M-%s"))

(defun god-mode-control-meta ()
  "The command for C-M-."
  (interactive)
  (god-mode-try-command "C-M-" "C-M-%s"))

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let ((key (char-to-string (aref (this-command-keys-vector) 0))))
    (god-mode-interpret-key key)))

(defun god-mode-repeat ()
  "Repeat the last command."
  (interactive)
  (when god-mode-last-command
    (let ((current-prefix-arg god-mode-last-prefix-arg))
      (call-interactively god-mode-last-command))))

(defun god-mode-interpret-key (key)
  "Interpret the given key. This function sometimes recurses."
  (cond
   ;; Digit argument
   ((string-match "^[0-9]$" key)
    (let ((current-prefix-arg
           (if (numberp current-prefix-arg)
               (string-to-number (concat (number-to-string current-prefix-arg)
                                         key))
             (string-to-number key))))
      (god-mode-interpret-key (char-to-string (read-event (format "%d" current-prefix-arg))))))
   ;; Boolean prefix arguments
   ((string= key "u")
    (let ((current-prefix-arg t))
      (god-mode-interpret-key (char-to-string (read-event "u")))))
   ;; For better keyboard macro interpretation.
   ((string= key " ") (god-mode-interpret-key "SPC"))
   ;; By default all other things are C-*///
   (t
    (let* ((formatted (format "C-%s" key))
           (command (read-kbd-macro formatted))
           (binding (key-binding command)))
      (god-mode-execute-binding formatted binding)))))

(defun god-mode-try-command (prompt format &optional keymapp control)
  "Try to run a command that takes additional key presses."
  (let* ((event (read-event prompt))
         (key. (if (eq event 'backspace)
                   "DEL"
                 (char-to-string event))))
    (let* ((control (if (string= key. god-literal-key) nil control))
           (key (if (string= key. god-literal-key)
                    (char-to-string (read-event prompt))
                  key.))
           (formatted (format (if keymapp
                                  (if control
                                      (concat format " C-%s")
                                    (concat format " %s"))
                                format)
                              key))
           (command (read-kbd-macro formatted))
           (binding (key-binding command)))
      (god-mode-execute-binding formatted binding))))

(defun god-mode-execute-binding (formatted binding)
  "Execute extended keymaps such as C-c, or if it is a command,
call it."
  (cond ((commandp binding)
         (setq this-original-command binding)
         (setq this-command binding)
         (setq god-mode-last-command binding)
         (setq god-mode-last-prefix-arg current-prefix-arg)
         (call-interactively binding))
        ((keymapp binding)
         (god-mode-try-command formatted formatted t t))
        (:else
         (error "God: Unknown key binding for `%s`" formatted))))

(add-hook 'after-change-major-mode-hook 'god-mode-maybe-activate)

(defun god-mode-maybe-activate ()
  "Activate God mode locally on individual buffers when appropriate."
  (when (and god-global-mode
             (not (minibufferp))
             (not (memq major-mode god-exempt-major-modes)))
    (god-local-mode 1)))

(provide 'god-mode)

;;; god-mode.el ends here
