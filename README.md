# God Mode — no more RSI

***NOTE***: Emacs 24.3 is required for this package to work well!

This is a global minor mode for entering Emacs commands without
modifier keys. It's similar to Vim's separation of commands and
insertion mode.

## Example

In the example below you can see how much effort is reduced:

    Before: C-p C-k C-n M-^ ) C-j C-y M-r C-x z z M-2 C-g C-g C-x C-s
    After:    p   k   n g ^ )   j   y g r   x . .   2 C-g C-g   x   s

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

Also, you can add this to your `.xmodmap` to rebind Caps Lock to
Escape:

``` lisp
remove Lock = Caps_Lock
keysym Caps_Lock = Escape
```

And run `xmodmap .xmodmap` for the changes to take effect immediately.

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

## Not implemented yet

* C- with backspace and arrow keys don't quite work, not looked into
  it yet.
