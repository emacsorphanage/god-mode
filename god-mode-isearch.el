;;; god-mode-isearch.el --- God mode behaviour for isearch  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Chris Done
;; Copyright (C) 2020 Akhil Wali

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/emacsorphanage/god-mode
;; Version: 2.19.0
;; Package-Requires: ((emacs "29"))

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

;; (keymap-set isearch-mode-map "<escape>" 'god-mode-isearch-activate)
;; (keymap-set god-mode-isearch-map "<escape>" 'god-mode-isearch-disable)

(defvar-keymap god-mode-isearch-map
  :parent isearch-mode-map
  :doc
  "Keymap for modal isearch."
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward
  "w" #'isearch-yank-word-or-char)

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
