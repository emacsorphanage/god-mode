;;; god-mode-isearch.el --- God mode behaviour for isearch  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Chris Done
;; Copyright (C) 2020 Akhil Wali

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/emacsorphanage/god-mode
;; Version: 2.18.0
;; Package-Requires: ((emacs "25.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Global minor mode for entering Emacs commands without modifier keys.

;;; Code:

;; Recommended use:

;; (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
;; (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(defvar god-mode-isearch-map
  (let ((map (copy-keymap isearch-mode-map)))
    (define-key map (kbd "s") 'isearch-repeat-forward)
    (define-key map (kbd "r") 'isearch-repeat-backward)
    (define-key map (kbd "w") 'isearch-yank-word-or-char)
    map)
  "Keymap for modal isearch.")

(defun god-mode-isearch-activate ()
  "Activate God mode in the isearch buffer."
  (interactive)
  (setq overriding-terminal-local-map god-mode-isearch-map))

(defun god-mode-isearch-disable ()
  "Deactivate God mode in the isearch buffer."
  (interactive)
  (setq overriding-terminal-local-map isearch-mode-map))

(provide 'god-mode-isearch)

;;; god-mode-isearch.el ends here
