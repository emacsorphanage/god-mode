# God Mode — no more RSI

[![melpa-badge][melpa-badge]][melpa-link]
[![gh-actions-badge][gh-actions-badge]][gh-actions-link]

***NOTE***: Emacs 24.4 is required for this package to work well!

This is a global minor mode for entering Emacs commands without
modifier keys. It's similar to Vim's separation of commands and
insertion mode.

## Example

In the example below you can see how much effort is reduced:

    Before: C-p C-k C-n M-^ ) C-j C-y M-r C-x z z M-2 M-g M-g C-x C-s
    After:    p   k   n g ^ )   j   y g r     . .   2   g   g   x   s

(Regarding `.` see [nice keybindings][nice-keybindings] section.)

You'll find that this mode comes surprisingly naturally and that you
already know how to run your existing Emacs commands.

See the Mapping section for a complete rundown of the transformations.

## Activation

Load it up:

``` lisp
(require 'god-mode)
```

Activate for all future buffers by running `M-x god-mode`. Although the
activation is buffer-local.

Toggle between God mode and non-God mode using `ESC`:

``` lisp
(global-set-key (kbd "<escape>") 'god-local-mode)
```

If you want to enable/disable on *all active and future buffers*, use
this:

``` lisp
(global-set-key (kbd "<escape>") 'god-mode-all)
```

If you are using the global mode, you might want to make no buffers
exempt:

``` lisp
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
```

This means that e.g. magit-mode or dired-mode will also enter god-mode
when you activate it globally, and vise-verse. It means you can always
reliably use god-mode commands in any buffer as long as it is globally
activated.

Also, you can add this to your `.xmodmap` to rebind Caps Lock to
Escape:

``` lisp
remove Lock = Caps_Lock
keysym Caps_Lock = Escape
```

And run `xmodmap .xmodmap` for the changes to take effect immediately.

Or use dconf:

    dconf write /org/gnome/desktop/input-sources/xkb-options "['caps:escape']"

See [here][switch-caps-lock-and-esc] for more details.

## Mapping

This library defines the following mapping:

* All commands are assumed to be `C-<something>` unless otherwise
   indicated. Examples:

   * `a`    → `C-a`
   * `s`    → `C-s`
   * `akny` → `C-a C-k C-n C-y`
   * `xs`   → `C-x C-s`
   * `x s`  → `C-x s`

   Note the use of space to produce `C-x s`.

* `g` is a special key to indicate `M-<something>`. This means that
   there is no way to write `C-g` in this mode, you must therefore
   type `C-g` directly. Examples:

   * `gf` → `M-f`
   * `gx` → `M-x`

* `G` is a special key to indicate `C-M-<something>`. Example:

   * `Gx` → `C-M-x`

* Digit arguments:

  * `12f` → `M-12 C-f`

* Repetition (with `.` keybinding):

  * `gf..` → `M-f M-f M-f`

* Universal boolean argument:

  * `uco` → `C-u C-c C-o`

## Cursor style to indicate mode

You can change the cursor style indicate whether you're in God mode or
not.

``` lisp
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
```

## Change modeline color

You can use the following function to switch the entire modeline's foreground and background:

``` lisp
(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
```

## Overwrite mode

You can pause `god-mode` when `overwrite-mode` is enabled and resume
when `overwrite-mode` is disabled.

``` lisp
(defun god-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

(add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)
```

## isearch integration

There is a small module for providing god-mode-like behaviour for
isearch: You can hit <escape> while in isearch, for example:

    s hello <escape> s s s RET

For

    C-s hello C-s C-s C-s RET

Activate and configure with the following:

``` lisp
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)
```

Configure `god-mode-isearch-map` for additional keybindings.

## Using with org-mode fast keys

If you want to use god-mode with fast-keys, you can use a rebinding of
self-insert like this:

``` lisp
(define-key god-local-mode-map [remap self-insert-command] 'my-god-mode-self-insert)

(defun my-god-mode-self-insert ()
  (interactive)
  (if (and (bolp)
           (eq major-mode 'org-mode))
      (call-interactively 'org-self-insert-command)
    (call-interactively 'god-mode-self-insert)))
```

## Nice keybindings

The following customizations are popular:

``` lisp
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
```

Although I personally prefer:

``` lisp
(define-key god-local-mode-map (kbd ".") 'repeat)
```

Feel free to alter and customize as you prefer.

Also handy are these:

``` lisp
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)
```

So that you can run `x1`/`x2`/`x3`/`x0` in god-mode.

## Global god-mode and exempt major modes

**Note:** This is less necessary in recent god-mode, as god-mode
  overrides all printable single byte keys, so it will override
  dired-mode or magit-mode.

If you do `M-x god-mode`, then all buffers will be started in God
mode. If you don't like that behavior, just use the `god-local-mode`
toggler with a keybinding.

Sometimes `god-mode` is enabled in buffers where it makes no sense. In
that case you can add the major mode to `god-exempt-major-modes`:

``` lisp
(add-to-list 'god-exempt-major-modes 'dired-mode)
```

Since `dired-mode` is already in the list, that's a noop, but you get
the idea. Consider opening an issue or pull request if you find a
major mode that should be on the official list.

Another option to control god-mode's global behavior is to provide a
function with no arguments that must return non-nil if god-mode should
be disabled for the current buffer. See the `god-exempt-predicates`
variable and its default members `god-exempt-mode-p`,
`god-comint-mode-p`, `god-view-mode-p` and `god-special-mode-p` for
further details.

[nice-keybindings]: https://github.com/emacsorphanage/god-mode#nice-keybindings
[switch-caps-lock-and-esc]: https://askubuntu.com/questions/363346/how-to-permanently-switch-caps-lock-and-esc
[melpa-link]: https://melpa.org/#/god-mode
[melpa-badge]: https://melpa.org/packages/god-mode-badge.svg
[gh-actions-link]: https://github.com/emacsorphanage/god-mode/actions
[gh-actions-badge]: https://github.com/emacsorphanage/god-mode/workflows/CI/badge.svg
