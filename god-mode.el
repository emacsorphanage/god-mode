;;; god-mode.el --- God-like command entering minor mode

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2013 Magnar Sveen
;; Copyright (C) 2013 Rüdiger Sonderfeld
;; Copyright (C) 2013 Dillon Kearns

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/god-mode
;; Version: 2.9.1

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

;; See README.md.

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
    Buffer-menu-mode
    git-rebase-mode)
  "List of major modes that should not start in god-local-mode."
  :group 'god
  :type '(function))

(defvar god-global-mode nil
  "Activate God mode on all buffers?")

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
    map))

(defvar god-mode-universal-argument-map
  (let ((map (copy-keymap universal-argument-map)))
    (define-key map (kbd "u") 'universal-argument-more)
    map)
  "Keymap used while processing \\[universal-argument] with god-mode on.")

(defadvice save&set-overriding-map
  (before god-mode-add-to-universal-argument-map (map) activate compile)
  "This is used to set special keybindings after C-u is
pressed. When god-mode is active, intercept the call to add in
our own keybindings."
  (if (and god-local-mode (equal universal-argument-map map))
      (setq map god-mode-universal-argument-map)))

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
  (let ((key (char-to-string
              (aref (this-command-keys-vector)
                    (- (length (this-command-keys-vector)) 1)))))
    (god-mode-interpret-key key)))

(defun god-mode-interpret-key (key)
  "Interpret the given key. This function sometimes recurses."
  (cond
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
      (god-mode-execute-binding formatted binding (not control)))))

(defun god-mode-execute-binding (formatted binding &optional literal)
  "Execute extended keymaps such as C-c, or if it is a command,
call it."
  (cond ((commandp binding)
         (setq this-original-command binding)
         (setq this-command binding)
         ;; `real-this-command' is used by emacs to populate
         ;; `last-repeatable-command', which is used by `repeat'.
         (setq real-this-command binding)
         (call-interactively binding))
        ((keymapp binding)
         (god-mode-try-command formatted formatted t (not literal)))
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
