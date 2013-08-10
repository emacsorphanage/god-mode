;;; ghc-core.el --- Syntax highlighting module for GHC Core

;; Copyright (C) 2010  Johan Tibell

;; Author: Chris Done <chrisdone@gmail.com>

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
  "Activate/deactivate  God mode."
  (setq god-global-mode (not god-global-mode))
  (if god-global-mode
      (god-local-mode 1)
    (god-local-mode -1)))

(define-minor-mode god-local-mode
  "Minor mode for running commands."
  :lighter " God")

(defvar god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [remap self-insert-command] 'god-mode-self-insert)
    map))

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let
      ((key (this-command-keys)))
    (god-mode-interpret-key key)))

(defun god-mode-interpret-key (key)
  "Interpret the given key. This function sometimes recurses."
  (cond
   ((string= key " ")
    (god-mode-interpret-key "SPC"))
   ((string= key "u")
    (let ((key (read-event "u-")))
      (error "God: Universal argument not supported yet.")))
   ((string= key "g")
    (let ((key (read-event "g-")))
      (let* ((formatted (format "M-%s" (char-to-string key)))
             (command (read-kbd-macro formatted))
             (binding (key-binding command)))
        (if (commandp binding)
            (call-interactively binding)
          (error "God: Unknown key binding for `%s`" formatted)))))
   ((string= key "x")
    (let ((key (read-event "C-x ")))
      (let* ((formatted (format "C-x %s" (char-to-string key)))
             (command (read-kbd-macro formatted))
             (binding (key-binding command)))
        (if (commandp binding)
            (call-interactively binding)
          (error "God: Unknown key binding for `%s`" formatted)))))
   ((string= key "X")
    (let ((key (read-event "C-x C-")))
      (let* ((formatted (format "C-x C-%s" (char-to-string key)))
             (command (read-kbd-macro formatted))
             (binding (key-binding command)))
        (if (commandp binding)
            (call-interactively binding)
          (error "God: Unknown key binding for `%s`" formatted)))))
   ((string= key "h")
    (let ((key (read-event "C-h ")))
      (let* ((formatted (format "C-h %s" (char-to-string key)))
             (command (read-kbd-macro formatted))
             (binding (key-binding command)))
        (if (commandp binding)
            (call-interactively binding)
          (error "God: Unknown key binding for `%s`" formatted)))))
   ((string= key "c")
    (let ((key (read-event "C-c ")))
      (let* ((formatted (format "C-c %s" (char-to-string key)))
             (command (read-kbd-macro formatted))
             (binding (key-binding command)))
        (if (commandp binding)
            (call-interactively binding)
          (error "God: Unknown key binding for `%s`" formatted)))))
   ((string= key "C")
    (let ((key (read-event "C-c C-")))
      (let* ((formatted (format "C-c C-%s" (char-to-string key)))
             (command (read-kbd-macro formatted))
             (binding (key-binding command)))
        (if (commandp binding)
            (call-interactively binding)
          (error "God: Unknown key binding for `%s`" formatted)))))
   ((string= key "G")
    (let ((key (read-event "G-")))
      (let* ((formatted (format "C-M-%s" (char-to-string key)))
             (command (read-kbd-macro formatted))
             (binding (key-binding command)))
        (if (commandp binding)
            (call-interactively binding)
          (error "God: Unknown key binding for `%s`" formatted)))))
   (t
    (let* ((formatted (format "C-%s" key))
           (command (read-kbd-macro formatted))
           (binding (key-binding command)))
      (if (commandp binding)
          (call-interactively binding)
        (error "Unknown key binding for `%s`" formatted))))))

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
