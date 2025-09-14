# volatile-highlights.el - Transient visual feedback for edits

[![MELPA](https://melpa.org/packages/volatile-highlights-badge.svg)](https://melpa.org/#/volatile-highlights) [![MELPA Stable](https://stable.melpa.org/packages/volatile-highlights-badge.svg)](https://stable.melpa.org/#/volatile-highlights) [![License: GPLv3](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![Emacs](https://img.shields.io/badge/Emacs-24.4%2B-blue.svg)](https://www.gnu.org/software/emacs/)

## Overview

This library provides the global minor mode `volatile-highlights-mode`, which gives
transient visual feedback for common editing operations. After an operation completes,
the affected text is briefly highlighted and cleared on the next user command.

For example, yanking highlights the inserted text, and undo highlights the restored text.

For integration with other packages and developer-oriented examples, see the Extending Guide: [docs/extending.md](docs/extending.md).

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
  :hook (after-init . volatile-highlights-mode))
```

This ensures the package is installed and enables the mode when Emacs starts.

## Features

When `volatile-highlights-mode` is active, the following operations will be highlighted:

-   **`undo`:** Highlights the text affected by the undo operation.
-   **`yank` and `yank-pop`:** Highlights the newly pasted text.
-   **Killing commands (`kill-region`, `kill-line`, etc.):** Highlights the region where text was cut.
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
-   Enable or disable the "pulsing" effect (`vhl/use-pulsing-visual-effect-p`, default: disabled).
-   Highlight zero-width ranges (`Vhl/highlight-zero-width-ranges`, default: disabled): also mark deletion points as a 1-character highlight.

### Example Customizations with `use-package`

Here are some examples of how you might configure the package in your `init.el`:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode)
  :custom
  ;; Use a pulsing effect instead of a static highlight
  (vhl/use-pulsing-visual-effect-p t)
  ;; Also mark deletion points (zero-width ranges)
  (Vhl/highlight-zero-width-ranges t)
  :config
  ;; You can also set variables directly
  (setq vhl/pulse-iterations 5))
```

### xref (Emacs 25.1+)

On Emacs 25.1 and newer, use `xref` to navigate to definitions (instead of `find-tag`).

Volatile-highlights includes built-in xref integration (no custom extension needed). It can be toggled via `vhl/use-xref-extension-p` (default: disabled).

By default, xref shows a pulsing highlight after jumps (controlled by `xref-pulse-on-jump`). If you prefer a static highlight, use the configuration below.

Keep static (non-pulsing) highlights, including for xref jumps:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode)
  :custom
  ;; Keep static (non-pulsing) highlights (default)
  (vhl/use-pulsing-visual-effect-p nil)
  ;; Ensure xref integration is on (definitions)
  (vhl/use-xref-extension-p t)
  :config
  (with-eval-after-load 'xref
    ;; Disable the built-in xref pulse to keep static highlights
    (setq xref-pulse-on-jump nil)))
```

You can toggle vhl's xref integration with the customization flag `vhl/use-xref-extension-p`.

## Visual and Performance

- Face: Customize `vhl/default-face` to match your theme for clear highlights.
- Large ranges: Static (non-pulsing) highlights are often easier to read than pulsing for big changes.
- Avoid stacking: Do not layer multiple highlight systems for the same action (e.g., xref pulse + VHL) unless intentional.

## Extending with Other Packages

`volatile-highlights` can be configured to work with other packages that have their own yank/paste or insertion/deletion commands.

For a broader, task-oriented guide (user and developer examples), see the Extending Guide: [docs/extending.md](docs/extending.md).

### Evil (Extensible Vi Layer)

To make `evil-mode`'s pasting commands trigger highlights, a robust `use-package` setup would be:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode)
  :config
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
  :hook (after-init . volatile-highlights-mode)
  :config
  (with-eval-after-load 'undo-tree
    (vhl/define-extension 'undo-tree 'undo-tree-yank)
    (vhl/install-extension 'undo-tree)))
```

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
