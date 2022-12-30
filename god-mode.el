;;; god-mode.el --- Minor mode for God-like command entering  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2013 Magnar Sveen
;; Copyright (C) 2013 Rüdiger Sonderfeld
;; Copyright (C) 2013 Dillon Kearns
;; Copyright (C) 2013 Fabián Ezequiel Gallina
;; Copyright (C) 2020 Akhil Wali

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/emacsorphanage/god-mode
;; Version: 2.18.0
;; Package-Requires: ((emacs "25.1"))

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

;; Global minor mode for entering Emacs commands without modifier keys.

;;; Code:

(require 'cl-lib)

(add-hook 'after-change-major-mode-hook 'god-mode-maybe-activate)

(defvar god-local-mode-paused nil)
(make-variable-buffer-local 'god-local-mode-paused)

;; DEPRECATED
(defvaralias 'god-mod-alist 'god-mode-alist
  "Alias of `god-mode-alist' for backward compatibility.
Use `god-mode-alist' instead.")
(make-obsolete 'god-mod-alist 'god-mode-alist "2.16.1")

(defcustom god-mode-alist
  '((nil . "C-")
    ("g" . "M-")
    ("G" . "C-M-"))
  "List of keys and their associated modifer."
  :group 'god
  :type '(alist))

(defcustom god-literal-key
  "SPC"
  "The key used for literal interpretation."
  :group 'god
  :type 'string)

(defcustom god-exempt-major-modes
  '(Custom-mode
    Info-mode
    ag-mode
    calculator-mode
    calendar-mode
    cider-test-report-mode
    compilation-mode
    debugger-mode
    dired-mode
    edebug-mode
    ediff-mode
    eww-mode
    geben-breakpoint-list-mode
    git-commit-mode                     ; For versions prior to Magit 2.1.0
    grep-mode
    ibuffer-mode
    magit-popup-mode
    org-agenda-mode
    pdf-outline-buffer-mode
    recentf-dialog-mode
    sldb-mode
    sly-db-mode
    vc-annotate-mode
    wdired-mode)
  "List of major modes that should not start with `god-local-mode' enabled."
  :group 'god
  :type '(repeat symbol))

(defcustom god-exempt-predicates
  (list #'god-exempt-mode-p
        #'god-comint-mode-p
        #'god-git-commit-mode-p
        #'god-view-mode-p
        #'god-special-mode-p)
  "List of predicates checked before enabling `god-local-mode'.
All predicates must return nil for `god-local-mode' to start."
  :group 'god
  :type '(repeat function))

(defcustom god-mode-enable-function-key-translation t
  "Enables translation of function keys to use modifier keys.
For example, if this variable is non-nil, then entering `<f5>'
in God mode will be translated to `C-<f5>'."
  :group 'god
  :type 'boolean)

(defcustom god-mode-lighter-string
  "God"
  "String displayed on the mode line when God mode is active.
Set it to nil if you don't want a mode line indicator."
  :group 'god
  :type '(choice string (const :tag "None" nil)))

(defface god-mode-lighter
  '((t))
  "Face for God mode's lighter."
  :group 'god)

(defun god-mode-make-f-key (n &optional shift)
  "Get the event for numbered function key N, with shift status SHIFT.
For example, calling with arguments 5 and t yields the symbol `S-f5'."
  (intern (format "%sf%d" (if shift "S-" "") n)))

(defvar god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'god-mode-self-insert)
    (let ((i ?\s))
      (while (< i 256)
        (define-key map (vector i) 'god-mode-self-insert)
        (setq i (1+ i))))
    (when god-mode-enable-function-key-translation
      (dotimes (i 35)
        (define-key map (vector (god-mode-make-f-key (1+ i))) 'god-mode-self-insert)
        (define-key map (vector (god-mode-make-f-key (1+ i) t)) 'god-mode-self-insert)))
    (define-key map (kbd "DEL") nil)
    (define-key map (kbd "C-h k") #'god-mode-describe-key)
    map))

;;;###autoload
(define-minor-mode god-local-mode
  "Minor mode for running commands."
  :init-value nil
  :lighter (god-mode-lighter-string
            ((" "
             (:propertize god-mode-lighter-string face god-mode-lighter))))
  :keymap god-local-mode-map
  (if god-local-mode
      (run-hooks 'god-mode-enabled-hook)
    (run-hooks 'god-mode-disabled-hook)))

(defun god-local-mode-pause ()
  "Pause `god-local-mode' if it is enabled.
See also `god-local-mode-resume'."
  (when god-local-mode
    (god-local-mode -1)
    (setq god-local-mode-paused t)))

(defun god-local-mode-resume ()
  "Re-enable `god-local-mode'.
If it was not active when `god-local-mode-pause' was called, nothing happens."
  (when (bound-and-true-p god-local-mode-paused)
    (setq god-local-mode-paused nil)
    (god-local-mode 1)))

(defvar god-global-mode nil
  "Enable `god-local-mode' on all buffers.")

(defvar god-literal-sequence nil
  "Activated after `god-literal-key' is pressed in a command sequence.")

;;;###autoload
(defun god-mode ()
  "Toggle global `god-local-mode'."
  (interactive)
  (setq god-global-mode (not god-global-mode))
  (if god-global-mode
      (god-local-mode 1)
    (god-local-mode -1)))

;;;###autoload
(defun god-mode-all (&optional arg)
  "Toggle `god-local-mode' in all buffers.

Toggle the mode if ARG is nil. If ARG is non-nil, enable the mode
if ARG is zero or a positive number, or disable the mode if ARG
is a negative number."
  (interactive)
  (let ((new-status
	 (cond
	  ((null arg) (if (bound-and-true-p god-local-mode) -1 1))
	  ((> 0 arg) -1)
	  (t 1))))
    (setq god-global-mode t)
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (god-mode-activate new-status)))
          (buffer-list))
    (setq god-global-mode (= new-status 1))))

(defun god-mode-maybe-universal-argument-more ()
  "If `god-local-mode' is enabled, call `universal-argument-more'."
  (interactive)
  (if god-local-mode
      (call-interactively #'universal-argument-more)
    (let ((binding (god-mode-lookup-command "u")))
      (if (commandp binding t)
          (call-interactively binding)
        (execute-kbd-macro binding)))))

(define-key universal-argument-map (kbd "u")
  #'god-mode-maybe-universal-argument-more)

(defun god-mode-self-insert ()
  "Handle self-insert keys."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (binding (god-mode-lookup-key-sequence initial-key)))
    (when binding
      ;; For now, set the shift-translation status only for alphabetic keys.
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
        (execute-kbd-macro binding)))))

(defun god-mode-upper-p (key)
  "Check if KEY is an upper case character not present in `god-mode-alist'."
  (and (characterp key)
       (not (member (char-to-string key) (mapcar #'car god-mode-alist)))
       (>= key ?A)
       (<= key ?Z)))

(defun god-mode-lookup-key-sequence (&optional key key-string-so-far)
  "Lookup the command for the given KEY (or the next keypress, if KEY is nil).
This function sometimes recurses.
KEY-STRING-SO-FAR should be nil for the first call in the sequence."
  (interactive)
  (let ((sanitized-key
         (god-mode-sanitized-key-string
          (or key (read-event key-string-so-far)))))
    (god-mode-lookup-command
     (god-key-string-after-consuming-key sanitized-key key-string-so-far))))

(defvar god-mode-sanitized-key-alist
  (append
   '((tab . "TAB")
     (?\  . "SPC")
     (left . "<left>")
     (right . "<right>")
     (up . "<up>")
     (down . "<down>")
     (prior . "<prior>")
     (next . "<next>")
     (backspace . "DEL")
     ;; FIXME delete key is not picked up `god-mode-self-insert'.
     (delete . "<delete>")
     (return . "RET")
     (S-iso-lefttab . "<S-iso-lefttab>")
     ;; Use key code for S-SPC
     (33554464 . "S-SPC")
     (S-left . "S-<left>")
     (S-right . "S-<right>")
     (S-up . "S-<up>")
     (S-down . "S-<down>")
     (S-backspace . "<S-backspace>")
     (S-delete . "<S-delete>")
     (S-return . "<S-return>"))
   ;; f1..f35 and S-f1..S-f35
   (cl-mapcan (lambda (i)
                (list (cons (god-mode-make-f-key i)   (format "<f%d>" i))
                      (cons (god-mode-make-f-key i t) (format "S-<f%d>" i))))
              (number-sequence 1 35)))
  "Association list mapping special events to their textual representations.")

(defun god-mode-sanitized-key-string (key)
  "Convert any special events in KEY to textual representation."
  (or (cdr (assq key god-mode-sanitized-key-alist))
      (char-to-string key)))

(defun god-mode-help-char-dispatch (_help-key key-string-so-far)
  "Invokes `prefix-help-command' by entering the key sequence KEY-STRING-SO-FAR
followed by the `help-char' key.
HELP-KEY contains the key that caused `god-mode-help-char-dispatch' to be called
from the command loop."
  ;; Adding an element (t . key) to `unread-command-events' will add key to
  ;; the current command's key sequence.
  (setq unread-command-events
        (cl-loop for key in (append (read-kbd-macro key-string-so-far t)
                                    (list help-char))
                 collect (cons t key)))
  ;; Return nil so that `god-mode-lookup-command' doesn't perform any action.
  nil)

(defun god-key-string-after-consuming-key (key key-string-so-far)
  "Interpret god-mode special keys for KEY.
Consumes more keys if appropriate.
Appends to key sequence KEY-STRING-SO-FAR."
  (let ((key-consumed t) (next-modifier "") next-key)
    (message key-string-so-far)
    (cond
     ;; Don't check for `god-literal-key' with the first key.
     ((and key-string-so-far (string= key god-literal-key))
      (setq god-literal-sequence t))
     (god-literal-sequence
      (setq key-consumed nil))
     ((and (stringp key) (assoc key god-mode-alist))
      (setq next-modifier (cdr (assoc key god-mode-alist))))
     (t
      (setq key-consumed nil
            next-modifier (cdr (assoc nil god-mode-alist)))))
    (setq next-key
          (if key-consumed
              (god-mode-sanitized-key-string (read-event key-string-so-far))
            key))
    (if (and key-string-so-far (string= next-key (format "%c" help-char)))
        (god-mode-help-char-dispatch next-key key-string-so-far)
      (when (and (= (length next-key) 1)
                 (string= (get-char-code-property (aref next-key 0) 'general-category) "Lu")
                 ;; If C- is part of the modifier, S- needs to be given
                 ;; in order to distinguish the uppercase from the
                 ;; lowercase bindings. If C- is not in the modifier,
                 ;; then emacs natively treats uppercase differently
                 ;; from lowercase, and the S- modifier should not be
                 ;; supplied.
                 (string-prefix-p "C-" next-modifier))
        (setq next-modifier (concat next-modifier "S-")))
      (if key-string-so-far
          (concat key-string-so-far " " next-modifier next-key)
        (concat next-modifier next-key)))))

(defun god-mode-lookup-command (key-string)
  "Execute extended keymaps in KEY-STRING, or call it if it is a command."
  (when key-string
    (let* ((key-vector (read-kbd-macro key-string t))
           (binding (key-binding key-vector)))
      (cond ((commandp binding)
             (setq last-command-event (aref key-vector (- (length key-vector) 1)))
             binding)
            ((keymapp binding)
             (god-mode-lookup-key-sequence nil key-string))
            (:else
             (error "God: Unknown key binding for `%s`" key-string))))))

;;;###autoload
(defun god-mode-maybe-activate (&optional status)
  "Activate `god-local-mode' on individual buffers when appropriate.
STATUS is passed as an argument to `god-mode-activate'."
  (when (not (minibufferp))
    (god-mode-activate status)))

(defun god-mode-activate (&optional status)
  "Activate `god-local-mode' on individual buffers when appropriate.
STATUS is passed as an argument to `god-local-mode'."
  (when (and god-global-mode
             (god-passes-predicates-p))
    (god-local-mode (if status status 1))))

(defun god-exempt-mode-p ()
  "Return non-nil if `major-mode' is exempt.
Members of the `god-exempt-major-modes' list are exempt."
  (memq major-mode god-exempt-major-modes))

(defun god-mode-child-of-p (mode parent-mode)
  "Return non-nil if MODE is derived from PARENT-MODE."
  (let ((parent (get mode 'derived-mode-parent)))
    (cond ((eq parent parent-mode))
          ((not (null parent))
           (god-mode-child-of-p parent parent-mode))
          (t nil))))

(defun god-comint-mode-p ()
  "Return non-nil if `major-mode' is derived from `comint-mode'."
  (god-mode-child-of-p major-mode 'comint-mode))

(defun god-special-mode-p ()
  "Return non-nil if `major-mode' is special or derived from `special-mode'."
  (eq (get major-mode 'mode-class) 'special))

(defun god-view-mode-p ()
  "Return non-nil if variable `view-mode' is non-nil in current buffer."
  view-mode)

(defun god-git-commit-mode-p ()
  "Return non-nil if a `git-commit-mode' will be enabled in this buffer."
  (and (bound-and-true-p global-git-commit-mode)
       ;; `git-commit-filename-regexp' defined in the same library as
       ;; `global-git-commit-mode'.  Expression above maybe evaluated
       ;; to true because of autoload cookie.  So we perform
       ;; additional check.
       (boundp 'git-commit-filename-regexp)
       buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)))

(defun god-passes-predicates-p ()
  "Return non-nil if all `god-exempt-predicates' return nil."
  (not
   (catch 'disable
     (let ((preds god-exempt-predicates))
       (while preds
         (when (funcall (car preds))
           (throw 'disable t))
         (setq preds (cdr preds)))))))

;;;###autoload
(defun god-execute-with-current-bindings (&optional called-interactively)
  "Execute a single command from God mode, preserving current keybindings.

This command activates God mode temporarily, and deactivates God
mode as soon as the next command is run.  Prefix arguments do not
count as commands for this purpose, and do not cause God mode to
exit.  Moreover, any prefix argument that exists at the time of
this command's invocation is passed along to the next command.

Unlike normal use of God mode, this command makes available all
keybindings that were active at the time of its invocation,
including keybindings that are normally invisible to God mode,
such as those in `emulation-mode-map-alists' or text overlay
properties.  This makes it suitable for use with packages like
Evil that utilize such higher-priority keymaps.  (See Info
node `(elisp)Active Keymaps' for technical details on keymap
precedence.  For an alternative to this command, check out the
evil-god-state package, available on MELPA.)

This command has no effect when called from within God mode.

For interactive use only.  CALLED-INTERACTIVELY is a dummy
parameter to help enforce this restriction."
  (interactive "d")
  (if called-interactively
      (unless god-local-mode
        (message "Switched to God mode for the next command ...")
        (letrec ((caller this-command)
                 (buffer (current-buffer))
                 (cleanup
                  (lambda ()
                    ;; Perform cleanup in original buffer even if the command
                    ;; switched buffers.
                    (if (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (unwind-protect (god-local-mode 0)
                            (remove-hook 'post-command-hook post-hook)))
                      (remove-hook 'post-command-hook post-hook))))
                 (kill-transient-map
                  (set-transient-map
                   god-local-mode-map 'god-prefix-command-p cleanup))
                 (post-hook
                  (lambda ()
                    (unless (and
                             (eq this-command caller)
                             ;; If we've entered the minibuffer, this implies
                             ;; a non-prefix command was run, even if
                             ;; `this-command' has not changed.  For example,
                             ;; `execute-extended-command' behaves this way.
                             (not (window-minibuffer-p)))
                      (funcall kill-transient-map)))))
          (add-hook 'post-command-hook post-hook)
          ;; Pass the current prefix argument along to the next command.
          (setq prefix-arg current-prefix-arg)
          ;; Technically we don't need to activate God mode since the
          ;; transient keymap is already in place, but it's useful to provide
          ;; a mode line lighter and run any hook functions the user has set
          ;; up.  This could be made configurable in the future.
          (god-local-mode 1)))
    (error "This function should only be called interactively")))

(defun god-prefix-command-p ()
  "Return non-nil if the current command is a \"prefix\" command.
This includes prefix arguments and any other command that should
be ignored by `god-execute-with-current-bindings'."
  (memq this-command '(god-mode-self-insert
                       digit-argument
                       negative-argument
                       universal-argument
                       universal-argument-more)))


(defvar god-latest-described-command nil
  "The latest command recorded by `god-mode-describe-key'.")

(defun god-mode--help-fn-describe-function (_arg)
  "Insert information about `god-mode' key-bindings for the described function.
The argument _ARG is ignored: it's only needed because all hooks in
`help-fns-describe-function-functions' take the function-name as argument.
But in our case it's redundant"
  (insert
   (format-message "\n  %s\n  %s%s\n  %s%s%s\n\n"
                   "(`god-local-mode' is enabled. "
                   " The given key-sequence: "
                   (god-mode-get-described-key-seq)
                   " corresponds to this key-binding: "
                   (if (> emacs-major-version 27)
                       (help--key-description-fontified
                        god-latest-described-command)
                     god-latest-described-command)
                   ")")))

(defun god-mode-get-described-key-seq ()
  "Return the keys that were pressed after `god-mode-describe-key' was called."
  (let* ((latest-keys (recent-keys 'include-cmds))
         (latest-describe-key-index
          (cl-position '(nil . god-mode-self-insert)
                       latest-keys :from-end t :test #'equal))
         (start-index (+ 3 latest-describe-key-index))
         (key-sequence (key-description (seq-subseq latest-keys start-index))))
    (if (> emacs-major-version 27)
        (propertize key-sequence
                    'font-lock-face 'help-key-binding
                    'face 'help-key-binding)
      key-sequence)))

(defun god-mode-describe-key ()
  "Describe a key-sequence as interpreted by `god-mode'.
Prompt for a key sequence, use `god-mode-lookup-key-sequence' to translate it
into the appropriate command, and use `describe-function' to describe it"
  (interactive)
  (message "Describe the following god-mode key: ")
  (advice-add #'god-mode-lookup-command :filter-args
              (lambda (key-string)
                (setq god-latest-described-command key-string)))
  ;; use unwind-protect, and remove the advice / hook in the unwind forms
  (unwind-protect
      (let ((command
             ;; if the key is not recognized by god-mode,
             ;; we will pass it to the regular `describe-key'
             (condition-case err
                 (god-mode-lookup-key-sequence)
               (wrong-type-argument
                ;; due to how errors are passed,
                ;; we do not have enough information
                ;; to pass menu items
                (if (not (equal (cddr err) '((menu-bar))))
                    (describe-key (vector (caddr err))))))))
        (if command
            (progn
              (add-hook 'help-fns-describe-function-functions
                        #'god-mode--help-fn-describe-function)
              (describe-function command))))
    ;; unwind forms:
    (advice-remove #'god-mode-lookup-command
                   (lambda (key-string)
                     (setq god-latest-described-command key-string)))
    (remove-hook 'help-fns-describe-function-functions
                 #'god-mode--help-fn-describe-function)))

(provide 'god-mode)

;;; god-mode.el ends here
