# God Mode

This is a global minor mode for entering Emacs commands without
modifier keys. It's similar to Vim's separation of commands and
insertion mode. Activate by running `M-x god-mode`. Toggle between God
mode and non-God mode using `ESC`.

## Mapping

This library defines the following mapping:

* All commands are assumed to be `C-<something>` unless otherwise
   indicated. Examples:

   * `a`    → `C-a`
   * `s`    → `C-s`
   * `akny` → `C-a C-k C-n C-y`

* `g` is a special key to indicate `M-<something>`. This means that
   there is no way to write `C-g` in this mode, you must therefore
   type `C-g` directly. Examples:

   * `gf` → `M-f`
   * `gx` → `M-f`

* `x` is a special key to indicate `C-x <something>`. Examples:

   * `xb` → `C-x b`
   * `xh` → `C-x h`

* `c` is a special key to indicate `C-c <something>`. Examples:

   * `ca` → `C-c a`
   * `cc` → `C-c c`

* `h` is a special key to indicate `C-h <something>`. Examples:

   * `hh` → `C-h h`
   * `hf` → `C-h f`

* There is a convention of uppercase special keys to indicate
   two modifier keys in action. Those are:

   * `Gx` → `C-M-x`
   * `Xs` → `C-x C-s`
   * `Xx` → `C-x C-x`
   * `Cc` → `C-c C-c`
