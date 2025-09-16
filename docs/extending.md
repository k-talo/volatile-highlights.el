# Extending volatile-highlights

This guide focuses on integrating volatile-highlights with other packages and your own commands.
For installation and basic configuration, see the README: [README.md](../README.md).

It includes:
- [Quick Start](#quick-start)
- [For Users](#for-users): enable highlights for third-party insertion/deletion commands or other actions
- [For Developers](#for-developers): call the API directly from your own commands
- [Internals: how extensions work](#internals-how-vhldefine-extension-works)
- [Reference: core API](#reference-core-api)

## Quick Start

- Insert/delete commands (most packages)

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1)
  (with-eval-after-load 'some-pkg
    (vhl/define-extension 'some-pkg 'some-insert-cmd 'some-delete-cmd)
    (vhl/install-extension 'some-pkg)))
```

- Other commands (compute a range yourself)

```emacs-lisp
(defun my/vhl/after-bookmark-jump (&rest _)
  (vhl/add-range (line-beginning-position) (line-end-position)))

(with-eval-after-load 'bookmark
  (advice-add 'bookmark-jump :after #'my/vhl/after-bookmark-jump))
```
## Concepts

volatile-highlights primarily serves two common use cases:
- Insert/delete operations: emphasize where the buffer content changed
- Jump operations: emphasize the jump destination or origin

You can also apply it to related actions such as rectangle edits, replacements, formatting, and error navigation.

## For Users

### Insertion/deletion commands

For commands that insert or delete text, define an extension and then install it.
`vhl/define-extension` sets up automatic highlighting for the commands you list,
and `vhl/install-extension` enables it and connects it to a user option.

```emacs-lisp
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode 1)
  (with-eval-after-load 'some-pkg
    ;; Replace with actual insert/delete commands from the package
    (vhl/define-extension 'some-pkg 'some-insert-cmd 'some-delete-cmd)
    (vhl/install-extension 'some-pkg)))
```

Notes:
- Installing an extension creates a customizable flag `vhl/use-some-pkg-extension-p`.
- Toggling this option enables or disables the integration for that package.
- The extension also defines interactive commands `vhl/ext/some-pkg/on` and `vhl/ext/some-pkg/off`.
  These are called automatically when you change `vhl/use-some-pkg-extension-p` via Customize.
  You can also call them manually for a temporary session-only toggle.
 - For design details on how extensions work, see Internals below ([How vhl/define-extension works](#internals-how-vhldefine-extension-works)).

### Other commands: two integration options

For commands where VHL cannot infer the changed area, there are two options:

#### Option 1: Quick ad-hoc advice (guarded)

Keep it simple: compute a range and call `vhl/add-range`. Optionally guard the
call so it only runs when volatile-highlights is available (the minor mode
being enabled/disabled is handled by VHL itself).

```emacs-lisp
(defun my/vhl/after-bookmark-jump (&rest _)
  (when (featurep 'volatile-highlights)
    (vhl/add-range (line-beginning-position) (line-end-position))))

(with-eval-after-load 'bookmark
  (advice-add 'bookmark-jump :after #'my/vhl/after-bookmark-jump))
```
More ad-hoc examples:

Highlight the next-error destination line via hook.

```emacs-lisp
(defun my/vhl/highlight-next-error ()
  (when (featurep 'volatile-highlights)
    (vhl/add-range (line-beginning-position) (line-end-position))))

(add-hook 'next-error-hook #'my/vhl/highlight-next-error)
```

Yasnippet: highlight the expanded snippet region after exiting a snippet.

```emacs-lisp
(with-eval-after-load 'yasnippet
  (defun my/vhl/yas-after-expand-hook ()
    (let ((f (yas--field-root (yas--snippet-active-field (car yas--active-snippets)))))
      (when (and f (featurep 'volatile-highlights))
        (vhl/add-range (yas--field-start f) (yas--field-end f)))))
  (add-hook 'yas-after-exit-snippet-hook #'my/vhl/yas-after-expand-hook))
```

##### Choosing hooks and advice kinds

- After advice (`:after`) is usually best when you want to highlight the final result.
- Around advice (`:around`) is useful if you need to compute a precise begin/end before and after.
- Filter-return advice (`:filter-return`) works for functions that set match data and return non-nil on success.
- Hooks may be simpler when packages already expose post-action hooks (e.g., `next-error-hook`).

#### Option 2: First-class extension (recommended)

Define on/off functions that add/remove your advice, then install a VHL
extension to automatically wire a Customize option and minor-mode lifecycle.

This ensures highlight-related work does not run while the minor mode is off,
and seamlessly reactivates when the mode is turned back on.

```emacs-lisp
(defun vhl/ext/bookmark/.after-jump (&rest _)
  (vhl/add-range (line-beginning-position) (line-end-position)))

(defun vhl/ext/bookmark/on ()
  (advice-add 'bookmark-jump :after #'vhl/ext/bookmark/.after-jump))

(defun vhl/ext/bookmark/off ()
  (advice-remove 'bookmark-jump #'vhl/ext/bookmark/.after-jump))

;; Creates `vhl/use-bookmark-extension-p` and hooks into the mode on/off
(vhl/install-extension 'bookmark)
```


Tips:
- To mark deletion points as 1-character highlights, enable `vhl/highlight-zero-width-ranges`.
- To explicitly mark a single position, use `vhl/add-position`.

## For Developers

Two simple rules:
- If you already know the changed range, call `vhl/add-range` (or `vhl/add-position` for a single point).
- If you need work to compute what to highlight, put that work inside one guard so nothing runs when the mode is off.

Guarded template

```emacs-lisp
(when (and (featurep 'volatile-highlights)
           (bound-and-true-p volatile-highlights-mode))
  ;; Compute or reuse bounds here, then highlight
  ;; Range example:
  (let ((beg ...) (end ...))
    (vhl/add-range beg end))
  ;; Or, deletion point example:
  ;; (vhl/add-position pos))
```

Examples

- Known range available

```emacs-lisp
(defun my-command-that-changes (beg end)
  ;; ... perform changes between BEG and END ...
  (when (and (featurep 'volatile-highlights)
             (bound-and-true-p volatile-highlights-mode))
    (vhl/add-range beg end)))
```

- Need to compute a range first

```emacs-lisp
(defun my-command-highlight-current-line ()
  (when (and (featurep 'volatile-highlights)
             (bound-and-true-p volatile-highlights-mode))
    (let ((b (line-beginning-position))
          (e (line-end-position)))
      (vhl/add-range b e))))
```

- Deletion point

```emacs-lisp
(defun my-command-that-deletes-at (pos)
  (when (and (featurep 'volatile-highlights)
             (bound-and-true-p volatile-highlights-mode))
    (vhl/add-position pos)))
```

Tips
- Prefer `vhl/add-position` for deletion points; enable `vhl/highlight-zero-width-ranges` to make them visible.
- Keep heavy computations (bounds, scanning) inside the guard so they are skipped when the mode is off.

Optional (rare): first-class extension

If you still want a user-visible toggle and a Customize option for your integration,
you can define `vhl/ext/<name>/on|off` and install it so it participates in the
minor-mode lifecycle. For most integrations, the guarded pattern above is simpler and sufficient.

## Internals: How vhl/define-extension works

`vhl/define-extension` generates advice around the target commands. While a
target command runs, a temporary `after-change-functions` handler is active; it
detects inserted and deleted ranges and calls `vhl/add-range` or
`vhl/add-position` to place transient highlights.

`vhl/install-extension` registers interactive on/off commands
`vhl/ext/<name>/on|off` and a user option `vhl/use-<name>-extension-p`, and it
respects that option when the minor mode turns on/off (loading/unloading the
extension accordingly). You can also invoke the on/off commands manually for a
session-only toggle.

## Reference: Core API

- `(vhl/add-range BEG END &optional BUF FACE)`
  - Add a transient highlight for the region.
    No-op when `volatile-highlights-mode` is disabled.
  - `BUF` defaults to the current buffer, `FACE` defaults to `vhl/default-face`.
- `(vhl/add-position POS &rest ARGS)`
  - Mark a single position as a 1-character highlight when `vhl/highlight-zero-width-ranges` is non-nil.
    No-op when `volatile-highlights-mode` is disabled.
- `(vhl/clear-all)`
  - Clear all VHL highlights (normally done automatically on next command).
- Extensions helper:
  - `(vhl/define-extension NAME &rest FUNCTIONS)`
    - Define a VHL extension named NAME, attaching standard advice to each of FUNCTIONS so inserts/deletes are highlighted automatically.
  - `(vhl/install-extension NAME &optional disabled-by-default-p)`
    - Register on/off commands `vhl/ext/NAME/on|off` and a user option `vhl/use-NAME-extension-p`, and hook the extension into the minor mode lifecycle. When `disabled-by-default-p` is non-nil, the Customize option defaults to nil; otherwise it defaults to t.
