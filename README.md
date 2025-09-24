# volatile-highlights.el - Transient visual feedback for edits

[![MELPA](https://melpa.org/packages/volatile-highlights-badge.svg)](https://melpa.org/#/volatile-highlights) [![MELPA Stable](https://stable.melpa.org/packages/volatile-highlights-badge.svg)](https://stable.melpa.org/#/volatile-highlights) [![License: GPLv3](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![Emacs](https://img.shields.io/badge/Emacs-24.4%2B-blue.svg)](https://www.gnu.org/software/emacs/)

## Overview

Highlights are volatile; they vanish on your next command.

This library provides the global minor mode `volatile-highlights-mode`, which gives
transient visual feedback for common editing operations. After an operation completes,
the affected text is briefly highlighted and cleared on the next user command.

For example, yanking highlights the inserted text, and undo highlights the restored text.

Explore the Extending Guide for integration patterns and developer-oriented examples: [docs/extending.md](docs/extending.md).
Review animation tuning and appearance tips in [docs/appearance-and-tuning.md](docs/appearance-and-tuning.md).
Check upcoming migrations and detailed release notes in [NEWS.md](NEWS.md).

## Installation

### From MELPA (Recommended)

`volatile-highlights.el` is available on [MELPA](https://melpa.org/#/volatile-highlights). If you have MELPA configured in your Emacs, you can install it with:

`M-x package-install RET volatile-highlights RET`

### Manual Installation

Alternatively, download `volatile-highlights.el` and place it in a directory on your `load-path`.

## Configuration

You can enable `volatile-highlights-mode` globally with the following in your Emacs configuration (`init.el` or `.emacs`):

```emacs-lisp
(volatile-highlights-mode 1)
```

### Using `use-package`

A more structured way to configure it, especially if you use the popular [`use-package`](https://github.com/jwiegley/use-package) macro, is as follows:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1))
```

This ensures the package is installed and enables the mode after it is loaded.

## Highlighted Operations

When `volatile-highlights-mode` is active, the following operations will be highlighted:

-   **`undo`:** Highlights the text affected by the undo operation.
-   **`yank` and `yank-pop`:** Highlights the newly pasted text.
-   **Killing commands (`kill-region`, `kill-line`, etc.):** Highlights the region where text was cut.
-   **Replacements (`query-replace`, `replace-string`, etc.):** Highlights the inserted text for each replacement.
-   **Transposition commands (`transpose-chars`, `transpose-words`, `transpose-regions`, etc.):** Highlights the swapped text.
-   **Definitions (`xref` on Emacs 25.1+, `find-tag` on older Emacs):** Highlights the destination at point.
-   **`occur` (Emacs < 28):** Highlights the selected occurrence when jumping from an `*Occur*` buffer.
    On Emacs 28 and newer, `occur` already provides built-in highlighting, so the vhl extension is not used.
-   **Non-incremental search:** Highlights the found text.

You can enable or disable highlighting for each specific operation through the customization interface.

## Customization

### Basic Customization

To customize the behavior and appearance of the highlights, you can use the customization group:

`M-x customize-group RET volatile-highlights RET`

This interface allows you to:
-   Toggle highlighting for specific commands (e.g., turn off for `yank` but keep for `undo`).
-   Change the highlight color by customizing the `vhl/default-face`.
-   Choose animation mode via `vhl/animation-style`:
    - `'static`: static highlight (default)
    - `'fade-in`: fade in, then stay until next command
    - `'pulse`: pulse (fade-out), then clear automatically
-   Adjust the prestart hint intensity of animated highlights with
    `vhl/animation-prestart-opacity` (auto per style by default).
    Animated styles wait for Emacs to become idle, so increasing this
    value shows a subtle hint immediately while the full animation
    still runs later.
-   Highlight zero-width ranges (`vhl/highlight-zero-width-ranges`, default: disabled): also mark deletion points as a 1-character highlight.

### Example Customizations with `use-package`

Here are some examples of how you might configure the package in your `init.el`:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :custom
  ;; Animation: choose one of 'static, 'fade-in, or 'pulse
  (vhl/animation-style 'fade-in)
  ;; Also mark deletion points (zero-width ranges)
  (vhl/highlight-zero-width-ranges t)
  :config
  (volatile-highlights-mode 1)
  ;; Prefer customize-set-variable (or setopt on Emacs 29.1+) so :set hooks run
  (customize-set-variable 'vhl/animation-mid-frames 4)
  (customize-set-variable 'vhl/animation-frame-interval 0.03)
  ;; On Emacs 29.1+ you can instead use:
  ;; (setopt vhl/animation-mid-frames 4
  ;;         vhl/animation-frame-interval 0.03)
  )
```

### xref (Emacs 25.1+)

On Emacs 25.1 and newer, use `xref` to navigate to definitions (instead of `find-tag`).

Volatile-highlights includes built-in xref integration (no custom extension needed). It can be toggled via `vhl/use-xref-extension-p` (default: disabled).

By default, xref shows a pulsing highlight after jumps (controlled by `xref-pulse-on-jump`). If you prefer a static highlight, use the configuration below.

Keep static (non-pulsing) highlights, including for xref jumps:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :custom
  ;; Keep static (non-animated) highlights (default)
  (vhl/animation-style 'static)
  ;; Ensure xref integration is on (definitions)
  (vhl/use-xref-extension-p t)
  :config
  (volatile-highlights-mode 1)
  (with-eval-after-load 'xref
    ;; Disable the built-in xref pulse to keep static highlights.
    ;; Use customize-set-variable (or setopt on Emacs 29.1+).
    (customize-set-variable 'xref-pulse-on-jump nil)
    ;; On Emacs 29.1+ you can instead use:
    ;; (setopt xref-pulse-on-jump nil)
    ))
```

You can toggle vhl's xref integration with the customization flag `vhl/use-xref-extension-p`.

## Visual and Performance

- Quick tips:
  - Face: Customize `vhl/default-face` to match your theme for clear highlights.
  - Large ranges: Static highlights can be easier to read than pulsing effects.
  - Avoid stacking: Do not layer multiple highlight systems for the same action (e.g., xref pulse + VHL) unless intentional.
  - Animation: Choose style via `vhl/animation-style` and tune iterations/delays.

For detailed guidance (face choices, theme-derived recipes, animation styles,
and tuning suggestions), see: [docs/appearance-and-tuning.md](docs/appearance-and-tuning.md)

## Extending with Other Packages

`volatile-highlights` can be configured to work with other packages that have their own yank/paste or insertion/deletion commands.

For a broader, task-oriented guide (user and developer examples), see the Extending Guide: [docs/extending.md](docs/extending.md).

### Evil (Extensible Vi Layer)

To make `evil-mode`'s pasting commands trigger highlights, a robust `use-package` setup would be:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1)
  (with-eval-after-load 'evil
    (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                          'evil-paste-pop)
    (vhl/install-extension 'evil)))
```

### Undo-tree

To integrate with `undo-tree`:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1)
  (with-eval-after-load 'undo-tree
    (vhl/define-extension 'undo-tree 'undo-tree-yank)
    (vhl/install-extension 'undo-tree)))
```

## Release Notes

For a chronological summary of releases, planned migrations, and recent maintenance work, see [NEWS.md](NEWS.md).

## Development: Compile & Test

- Byte-compile:
  - Makefile: `make compile`
  - Direct: `emacs -Q --batch -L . -f batch-byte-compile volatile-highlights.el`

- Run tests (ERT):
  - Makefile: `make test`
  - Direct: `emacs -Q --batch -L . -l volatile-highlights.el -l test-volatile-highlights.el -f ert-run-tests-batch-and-exit`

- macOS custom Emacs path example:
  - `make test EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"`
  - Direct: `"/Applications/Emacs.app/Contents/MacOS/Emacs" -Q --batch -L . -l volatile-highlights.el -l test-volatile-highlights.el -f ert-run-tests-batch-and-exit`

- Temporary directory note (batch test on restricted environments):
  - `mkdir -p .tmp && TMPDIR=$PWD/.tmp emacs -Q --batch -L . -l volatile-highlights.el -l test-volatile-highlights.el -f ert-run-tests-batch-and-exit`

- Lint (checkdoc and package-lint):
  - Makefile: `make checkdoc` (runs checkdoc on library .el files), `make package-lint` (runs package-lint on volatile-highlights.el), or `make lint` (both)
  - package-lint must be installed (M-x package-install RET package-lint RET)
  - If your package directory is customized, set `PKGDIR`:
    - `make package-lint PKGDIR="$HOME/.emacs.d/elpa"`
    - You can combine with a custom Emacs binary:
      - `make package-lint EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs" PKGDIR="$HOME/.emacs.d/elpa"`

## Reporting Issues

Please open issues on the GitHub tracker:

https://github.com/k-talo/volatile-highlights.el/issues

## License

GPLv3 or later. See the [LICENSE](LICENSE) file for the full text.

Copyright (c) 2001, 2010-2016, 2024-2025 K-talo Miyazaki
