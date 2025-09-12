# volatile-highlights.el - Minor mode for visual feedback on some operations.

## Overview

This library provides the minor mode `volatile-highlights-mode`, which gives visual feedback for some operations by temporarily highlighting the relevant text.

For example, when you `yank` (paste) text, the newly inserted text will be highlighted for a moment. When you `undo`, the undone text will be highlighted. This provides a clear, visual confirmation of what just happened.

All highlights are "volatile" and will disappear on the next user action.

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
-   **`occur`:** Highlights the selected occurrence when jumping from an `*Occur*` buffer.
-   **Non-incremental search:** Highlights the found text.

You can enable or disable highlighting for each specific operation through the customization interface.

## Customization

### Basic Customization

To customize the behavior and appearance of the highlights, you can use the customization group:

`M-x customize-group RET volatile-highlights RET`

This interface allows you to:
-   Toggle highlighting for specific commands (e.g., turn off for `yank` but keep for `undo`).
-   Change the highlight color by customizing the `vhl/default-face`.
-   Enable or disable the "pulsing" effect (`vhl/use-pulsing-visual-effect-p`).

### Example Customizations with `use-package`

Here are some examples of how you might configure the package in your `init.el`:

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode)
  :custom
  ;; Use a pulsing effect instead of a static highlight
  (vhl/use-pulsing-visual-effect-p t)
  :config
  ;; You can also set variables directly
  (setq vhl/pulse-iterations 5))
```

### xref (Emacs 25.1+)

On Emacs 25.1 and newer, use `xref` to navigate to definitions (instead of `find-tag`).

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

## Extending with Other Packages

`volatile-highlights` can be configured to work with other packages that have their own yank/paste or navigation commands.

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

## Testing

Run ERT tests in batch:

`emacs -Q --batch -L . -l volatile-highlights.el -l test-volatile-highlights.el -f ert-run-tests-batch-and-exit`

Byte-compile to catch compile-time issues:

`emacs -Q --batch -L . -f batch-byte-compile volatile-highlights.el`
