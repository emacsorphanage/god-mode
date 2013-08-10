;; C-a → a
;; C-SPC → SPC
;; M-a → ga
;; C-M-a → Ga
;; C-x h → xh
;; C-x b → xb
;; C-x C-s → Xs
;; C-x C-x → Xx
;; C-c C-c → Cc

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
