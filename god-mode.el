;;; god-mode.el --- God-like command entering minor mode

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2013 Magnar Sveen
;; Copyright (C) 2013 Rüdiger Sonderfeld
;; Copyright (C) 2013 Dillon Kearns
;; Copyright (C) 2013 Fabián Ezequiel Gallina

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/god-mode
;; Version: 2.15.0

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

(require 'cl-lib)

(add-hook 'after-change-major-mode-hook 'god-mode-maybe-activate)

(defvar god-local-mode-paused nil)
(make-variable-buffer-local 'god-local-mode-paused)

(defcustom god-mod-alist
  '((nil . "C-")
    ("g" . "M-")
    ("G" . "C-M-"))
  "List of keys and their associated modifer."
  :group 'god
  :type '(alist))

(defcustom god-literal-key
  " "
  "The key used for literal interpretation."
  :group 'god
  :type 'string)

(defcustom god-exempt-major-modes
  '(dired-mode
    grep-mode
    vc-annotate-mode)
  "List of major modes that should not start in god-local-mode."
  :group 'god
  :type '(function))

(defcustom god-exempt-predicates
  (list #'god-exempt-mode-p
        #'god-comint-mode-p
        #'god-magit-mode-p
        #'god-view-mode-p
        #'god-special-mode-p)
  "List of predicates checked before enabling god-local-mode.
All predicates must return nil for god-local-mode to start."
  :group 'god
  :type '(repeat function))

(defvar god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'god-mode-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'god-mode-self-insert)
        (setq i (1+ i)))
      (define-key map (kbd "DEL") nil))
    map))

(defvar god-mode-universal-argument-map
  (let ((map (copy-keymap universal-argument-map)))
    (define-key map (kbd "u") 'universal-argument-more)
    map)
  "Keymap used while processing \\[universal-argument] with god-mode on.")

;;;###autoload
(define-minor-mode god-local-mode
  "Minor mode for running commands."
  nil " God" god-local-mode-map
  (if god-local-mode
      (run-hooks 'god-mode-enabled-hook)
    (run-hooks 'god-mode-disabled-hook)))

(defun god-local-mode-pause ()
  "Pause god-mode local to the buffer, if it's
enabled. See also `god-local-mode-resume'."
  (when god-local-mode
    (god-local-mode -1)
    (setq god-local-mode-paused t)))

(defun god-local-mode-resume ()
  "Will re-enable god-mode, if it was active when
`god-local-mode-pause' was called. If not, nothing happens."
  (when (bound-and-true-p god-local-mode-paused)
    (setq god-local-mode-paused nil)
    (god-local-mode 1)))

(defvar god-global-mode nil
  "Activate God mode on all buffers?")

(defvar god-literal-sequence nil
  "Activated after space is pressed in a command sequence.")

;;;###autoload
(defun god-mode ()
  "Toggle global God mode."
  (interactive)
  (setq god-global-mode (not god-global-mode))
  (if god-global-mode
      (god-local-mode 1)
    (god-local-mode -1)))

;;;###autoload
(defun god-mode-all ()
  "Toggle God mode in all buffers."
  (interactive)
  (let ((new-status (if (bound-and-true-p god-local-mode) -1 1)))
    (setq god-global-mode t)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (god-mode-activate new-status)))
          (buffer-list))
    (setq god-global-mode (= new-status 1))))

(defadvice save&set-overriding-map
  (before god-mode-add-to-universal-argument-map (map) activate compile)
  "This is used to set special keybindings after C-u is
pressed. When god-mode is active, intercept the call to add in
our own keybindings."
  (if (and god-local-mode (equal universal-argument-map map))
      (setq map god-mode-universal-argument-map)))

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (binding (god-mode-lookup-key-sequence initial-key)))
    (when (god-mode-upper-p initial-key)
      (setq this-command-keys-shift-translated t))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (setq god-literal-sequence nil)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

(defun god-mode-upper-p (char)
  "Is the given char upper case?"
  (and (>= char ?A)
       (<= char ?Z)
       (/= char ?G)))

(defun god-mode-lookup-key-sequence (&optional key key-string-so-far)
  "Lookup the command for the given `key' (or the next keypress,
if `key' is nil). This function sometimes
recurses. `key-string-so-far' should be nil for the first call in
the sequence."
  (interactive)
  (let ((sanitized-key
         (if key-string-so-far (char-to-string (or key (read-event key-string-so-far)))
           (god-mode-sanitized-key-string (or key (read-event key-string-so-far))))))
    (god-mode-lookup-command
     (key-string-after-consuming-key sanitized-key key-string-so-far))))

(defun god-mode-sanitized-key-string (key)
  "Convert any special events to textual."
  (cl-case key
    (tab "TAB")
    (?\  "SPC")
    (left "<left>")
    (right "<right>")
    (prior "<prior>")
    (next "<next>")
    (backspace "DEL")
    (return "RET")
    (t (char-to-string key))))

(defun key-string-after-consuming-key (key key-string-so-far)
  "Interpret god-mode special keys for key (consumes more keys if
appropriate). Append to keysequence."
  (let ((key-consumed t) next-modifier next-key)
    (message key-string-so-far)
    (setq next-modifier
          (cond
           ((string= key god-literal-key)
            (setq god-literal-sequence t)
            "")
           (god-literal-sequence
            (setq key-consumed nil)
            "")
           ((and
             (stringp key)
             (not (eq nil (assoc key god-mod-alist)))
             (not (eq nil key)))
            (cdr (assoc key god-mod-alist)))
           (t
            (setq key-consumed nil)
            (cdr (assoc nil god-mod-alist))
            )))
    (setq next-key
          (if key-consumed
              (god-mode-sanitized-key-string (read-event key-string-so-far))
            key))
    (if key-string-so-far
        (concat key-string-so-far " " next-modifier next-key)
      (concat next-modifier next-key))))

(defun god-mode-lookup-command (key-string)
  "Execute extended keymaps such as C-c, or if it is a command,
call it."
  (let* ((key-vector (read-kbd-macro key-string t))
         (binding (key-binding key-vector)))
    (cond ((commandp binding)
           (setq last-command-event (aref key-vector (- (length key-vector) 1)))
           binding)
          ((keymapp binding)
           (god-mode-lookup-key-sequence nil key-string))
          (:else
           (error "God: Unknown key binding for `%s`" key-string)))))

;;;###autoload
(defun god-mode-maybe-activate (&optional status)
  "Activate God mode locally on individual buffers when appropriate."
  (when (not (minibufferp))
    (god-mode-activate status)))

(defun god-mode-activate (&optional status)
  "Activate God mode locally on individual buffers when appropriate."
  (when (and god-global-mode
             (god-passes-predicates-p))
    (god-local-mode (if status status 1))))

(defun god-exempt-mode-p ()
  "Return non-nil if major-mode is exempt.
Members of the `god-exempt-major-modes' list are exempt."
  (memq major-mode god-exempt-major-modes))

(defun god-mode-child-of-p (major-mode parent-mode)
  "Return non-nil if MAJOR-MODE is derived from PARENT-MODE."
  (let ((parent (get major-mode 'derived-mode-parent)))
    (cond ((eq parent parent-mode))
          ((not (null parent))
           (god-mode-child-of-p parent parent-mode))
          (t nil))))

(defun god-comint-mode-p ()
  "Return non-nil if major-mode is child of comint-mode."
  (god-mode-child-of-p major-mode 'comint-mode))

(defun god-special-mode-p ()
  "Return non-nil if major-mode is child of special-mode."
  (god-mode-child-of-p major-mode 'special-mode))

(defun god-view-mode-p ()
  "Return non-nil if view-mode is enabled in current buffer."
  view-mode)

(defun god-magit-mode-p ()
  "Return non-nil if a Magit-related mode is enabled."
  (or (bound-and-true-p global-git-commit-mode)
      (eq major-mode 'git-commit-mode)  ; For versions prior to Magit 2.1.0
      (god-mode-child-of-p major-mode 'magit-popup-mode)
      (god-mode-child-of-p major-mode 'magit-mode)))

(defun god-passes-predicates-p ()
  "Return non-nil if all `god-exempt-predicates' return nil."
  (not
   (catch 'disable
     (let ((preds god-exempt-predicates))
       (while preds
         (when (funcall (car preds))
           (throw 'disable t))
         (setq preds (cdr preds)))))))

(provide 'god-mode)

;;; god-mode.el ends here
