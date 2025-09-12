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
-   **`find-tag`:** Highlights the located tag.
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
  ;; Don't highlight when yanking
  (vhl/use-yank-extension-p nil)
  :config
  ;; You can also set variables directly
  (setq vhl/pulse-iterations 5))
```

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
