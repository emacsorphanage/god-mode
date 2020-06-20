# God Mode — no more RSI

[![melpa-badge][melpa-badge]][melpa-link]
[![melpa-stable-badge][melpa-stable-badge]][melpa-stable-link]
[![gh-actions-badge][gh-actions-badge]][gh-actions-link]

_NOTE_: Emacs 25.1 is required for this package to work well.

This is a global minor mode for entering Emacs commands without
modifier keys. It's similar to Vim's separation of commands and
insertion mode.

All existing key bindings will work in God mode. It's only there to reduce your
usage of modifier keys.

## Example

In the example below, you can see how much effort is reduced:

```
Before: C-p C-k C-n M-^ ) C-j C-y M-r C-x z z M-2 M-f C-x C-s
After:    p   k   n g ^ )   j   y g r     . .   2 g f   x   s
```

(Regarding <kbd>.</kbd>, see the [useful key bindings section][useful-key-bindings].)

You'll find this mode surprisingly natural, as you would already know how to use
your existing Emacs commands. Whenever you feel like it, you can explicitly use
modifier keys too.

See the [key mapping section][key-mapping] for a complete walk-through of key
translations.

## Setup

You can load and activate God mode as follows:

```emacs-lisp
(require 'god-mode)
(god-mode)
```

This will activate God mode in all future buffers. However, activation of God
mode itself is buffer-local.

God mode can be toggled through `god-local-mode` using the escape key
(<kbd>ESC</kbd>) as follows:

```emacs-lisp
(global-set-key (kbd "<escape>") #'god-local-mode)
```

If you want to toggle God mode on _all active and future buffers_, use
`god-mode-all` as follows:

```emacs-lisp
(global-set-key (kbd "<escape>") #'god-mode-all)
```

If God mode is activated through `god-mode` or `god-mode-all`, you might want to
ensure that no buffers are skipped, as follows:

```emacs-lisp
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
```

This means that `magit-mode` or `dired-mode`, for example, will also enter God
mode when you activate it in all buffers. This means you can always reliably use
God mode commands in any buffer.

Also, you can add this to your `.xmodmap` to rebind the caps lock key to the
escape key:

```
remove Lock = Caps_Lock
keysym Caps_Lock = Escape
```

And run `xmodmap .xmodmap` for the changes to take effect immediately.

Or use `dconf`:

``` sh
dconf write /org/gnome/desktop/input-sources/xkb-options "['caps:escape']"
```

See [this link][switch-caps-lock-and-esc] for more details.

## Key mapping

This package defines the following key mappings:

* All commands are assumed to use the control modifier (<kbd>C-</kbd>) unless 
  otherwise indicated. Here are some examples:

   * <kbd>x</kbd> → <kbd>C-x</kbd>
   * <kbd>f</kbd> → <kbd>C-f</kbd>
   * <kbd>x</kbd> <kbd>s</kbd> → <kbd>C-x</kbd> <kbd>C-s</kbd>
   * <kbd>x</kbd> <kbd>SPC</kbd> <kbd>s</kbd> → <kbd>C-x</kbd> <kbd>s</kbd>

   Note the use of the space key (<kbd>SPC</kbd>)
   to produce <kbd>C-x</kbd> <kbd>s</kbd>.

* <kbd>g</kbd> is used to indicate the meta modifier (<kbd>M-</kbd>). This means
  that there is no way to enter <kbd>C-g</kbd> in God mode, and you must
  therefore type in <kbd>C-g</kbd> directly. This key can be changed through
  `god-mode-alist`. Here are some examples:

   * <kbd>g</kbd> <kbd>x</kbd> → <kbd>M-x</kbd>
   * <kbd>g</kbd> <kbd>f</kbd> → <kbd>M-f</kbd>

* <kbd>G</kbd> is used to indicate both the control and meta modifiers
  (<kbd>C-M-</kbd>). This key can also be changed through `god-mode-alist`. Here
  are some examples:

   * <kbd>G</kbd> <kbd>x</kbd> → <kbd>C-M-x</kbd>
   * <kbd>G</kbd> <kbd>f</kbd> → <kbd>C-M-f</kbd>

* Digit arguments can also be used:

  * <kbd>1</kbd> <kbd>2</kbd> <kbd>f</kbd> → <kbd>M-12</kbd> <kbd>C-f</kbd>

* If you use some of the [useful key bindings][useful-key-bindings],
  <kbd>z</kbd> or <kbd>.</kbd> can repeat the previous command:

  * <kbd>g</kbd> <kbd>f</kbd> <kbd>.</kbd> <kbd>.</kbd> → <kbd>M-f</kbd>
    <kbd>M-f</kbd> <kbd>M-f</kbd>

* Universal arguments can also be specified using <kbd>u</kbd>:

  * <kbd>u</kbd> <kbd>c</kbd> <kbd>o</kbd> → <kbd>C-u</kbd> <kbd>C-c</kbd>
    <kbd>C-o</kbd>

## Cursor style to indicate mode

You can change the cursor style to indicate whether God mode is active as
follows:

```emacs-lisp
(defun my-god-mode-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor)
```

## Change modeline color

You can change the entire modeline's foreground and background to indicate
whether God mode is active as follows:

```emacs-lisp
(defun my-god-mode-update-modeline ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))

(add-hook 'god-mode-enabled-hook #'my-god-mode-update-modeline)
(add-hook 'god-mode-disabled-hook #'my-god-mode-update-modeline)
```

## `overwrite-mode`

You can pause or resume God mode depending on whether `overwrite-mode` is
activated as follows:

```emacs-lisp
(defun my-god-mode-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(add-hook 'overwrite-mode-hook #'my-god-mode-toggle-on-overwrite)
```

## `isearch` integration

There is an optional feature for providing God mode behaviour in `isearch`. It
allows you to hit <kbd>ESC</kbd>, for example, while in `isearch` to enable God
mode. Here's a more complete example:

<kbd>s</kbd> <kbd>h</kbd> <kbd>e</kbd> <kbd>y</kbd> <kbd>ESC</kbd> 
<kbd>s</kbd> <kbd>s</kbd> <kbd>s</kbd> <kbd>RET</kbd> → <kbd>C-s</kbd>
<kbd>h</kbd> <kbd>e</kbd> <kbd>y</kbd> <kbd>C-s</kbd> <kbd>C-s</kbd>
<kbd>C-s</kbd> <kbd>RET</kbd>

You can load and activate this feature as follows:

```emacs-lisp
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") #'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") #'god-mode-isearch-disable)
```

You can also configure `god-mode-isearch-map` for additional keybindings.

## Rebinding `self-insert-command`

You can rebind `self-insert-command` as you prefer. For example, here's a
mapping that calls `org-self-insert-command` in `org-mode`:

```emacs-lisp
(defun my-god-mode-self-insert ()
  (interactive)
  (if (and (bolp)
           (eq major-mode 'org-mode))
      (call-interactively 'org-self-insert-command)
    (call-interactively 'god-mode-self-insert)))

(define-key god-local-mode-map [remap self-insert-command] #'my-god-mode-self-insert)
```

## Useful key bindings

The following key bindings are popular:

```emacs-lisp
(define-key god-local-mode-map (kbd "z") #'repeat)
(define-key god-local-mode-map (kbd "i") #'god-local-mode)
```

Although I personally prefer:

```emacs-lisp
(define-key god-local-mode-map (kbd ".") #'repeat)
```

These are also handy:

```emacs-lisp
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
```

So that you can run <kbd>x</kbd> <kbd>1</kbd>, <kbd>x</kbd>
<kbd>2</kbd>, <kbd>x</kbd> <kbd>3</kbd>, and <kbd>x</kbd> <kbd>0</kbd> in God mode.

## Exempt major modes

_NOTE_: This is less necessary in recent versions of God mode, as
it overrides all printable single byte keys and bindings in 
most major modes.

Sometimes, God mode is enabled in buffers where it makes no sense. In
such cases, you can add the major mode to `god-exempt-major-modes`:

```emacs-lisp
(add-to-list 'god-exempt-major-modes 'dired-mode)
```

Since `dired-mode` is already in the list, that's a no-op, but you get
the idea. Consider opening an issue or pull request if you find a
major mode that should be on the official list.

Another option to control the behavior of God mode in new buffers is to provide
a function with no arguments that must return non-nil if God mode should be
disabled in the current buffer. See the `god-exempt-predicates` variable and its
default members `god-exempt-mode-p`, `god-comint-mode-p`, `god-view-mode-p` and
`god-special-mode-p` for further details.

## Usage with Evil

[Evil][evil] is a popular Emacs package that provides modal editing in the style
of Vi. While Evil doesn't always work well with God mode, there are a few simple
customizations that enable them to be used together smoothly.

* For running occasional and single commands in God mode, the built-in
  `god-execute-with-current-bindings` command works well with Evil without
  additional customization. This is quite similar to Evil's
  `evil-execute-in-emacs-state` command. All Evil bindings remain available when
  using `god-execute-with-current-bindings`. For example, executing
  `god-execute-with-current-bindings` and entering <kbd>v</kbd> will execute
  `evil-visual-block`, which is bound to <kbd>C-v</kbd> in Evil's Normal state.

* For sustained usage of God mode, it's a bit trickier as keybindings in Evil
  states generally override God mode. For example, if God mode is activated in
  Normal state, entering <kbd>v</kbd> executes `evil-visual-char`, which is
  bound to <kbd>v</kbd> in Normal state, instead of executing
  `evil-visual-block`. A good option to use Evil's state-specific keybindings
  through God mode is to create an intercept keymap using
  `evil-make-intercept-map` and `god-local-mode-map`. For example, you can
  enable use of God mode in Normal state as follows:

  ```emacs-lisp
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap god-local-mode-map 'god t t) 'normal)
  ```

* Another option to use God mode with Evil is to use the
  [`evil-god-state`][evil-god-state] package, which provides a dedicated Evil
  state for using God mode. For running occasional, single commands through God
  mode, use the `evil-execute-in-god-state` command. This works similar to
  `god-execute-with-current-bindings`. For sustained use of God mode, use the
  `evil-god-state` command. `evil-god-state` is also useful for accessing
  default Emacs keybindings through God mode. However, a disadvantage of
  `evil-god-state` is that Evil's state-specific keybindings will not be
  available in God mode.

[useful-key-bindings]: #useful-key-bindings
[key-mapping]: #key-mapping
[switch-caps-lock-and-esc]: https://askubuntu.com/questions/363346/how-to-permanently-switch-caps-lock-and-esc
[melpa-link]: https://melpa.org/#/god-mode
[melpa-stable-link]: https://stable.melpa.org/#/god-mode
[melpa-badge]: https://melpa.org/packages/god-mode-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/god-mode-badge.svg
[gh-actions-link]: https://github.com/emacsorphanage/god-mode/actions?query=workflow%3ACI+branch%3Amaster
[gh-actions-badge]: https://github.com/emacsorphanage/god-mode/workflows/CI/badge.svg?branch=master
[evil]: https://github.com/emacs-evil/evil
[evil-god-state]: https://github.com/gridaphobe/evil-god-state
