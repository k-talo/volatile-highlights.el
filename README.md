# volatile-highlights.el &#x2014; Minor mode for visual feedback on some operations.


## Overview

This library provides minor mode `volatile-highlights-mode`, which brings visual feedback to some operations by highlighting portions relating to the operations.

All of highlights made by this library will be removed when any new operation is executed.


## INSTALLING

To install this library, save the file "volatile-highlights.el" to a directory in your `load-path` (you can view the current `load-path` using `C-h v load-path <RET>` within Emacs), then add following lines to your `.emacs` start up file:

```emacs-lisp
(require 'volatile-highlights)
(volatile-highlights-mode t)
```


## Using

To toggle volatile highlighting, type:

    M-x volatile-highlights-mode <RET>

While this minor mode is on, a string \`VHl' will be displayed on the modeline.

Currently, operations listed below will be highlighted While the minor mode \`volatile-highlights-mode' is on:

-   **`undo`:** Volatile highlights will be put on the text inserted by `undo`.

-   **`yank` and `yank-pop`:** Volatile highlights will be put on the text inserted by `yank`' or `yank-pop`.

-   **`kill-region`,  `kill-line`,  any other killing function:** Volatile highlights will be put at the positions where the killed text used to be.

-   **`delete-region`:** Same as `kill-region`, but not as reliable since `delete-region` is an inline function.

-   **`find-tag`:** Volatile highlights will be put on the tag name which was found by `find-tag`.

-   **`occur-mode-goto-occurrence` and `occur-mode-display-occurrence`:** Volatile highlights will be put on the occurrence which is selected by `occur-mode-goto-occurrence` or `occur-mode-display-occurrence`.

-   **Non incremental search operations:** Volatile highlights will be put on the the text found by commands listed below:
    
        nonincremental-search-forward
        nonincremental-search-backward
        nonincremental-re-search-forward
        nonincremental-re-search-backward
        nonincremental-repeat-search-forward
        nonincremental-repeat-search-backward

Highlighting support for each operations can be turned on/off individually via customization. Also check out the customization group by:

    M-x customize-group RET volatile-highlights RET


## Adjusting the looks to suit your taste

The following user options are provided to help you get the look you want:

-   **`vhl/use-pulsing-visual-effect-p`:** Whether to use visual effect 'pulsing' for volatile highlighting. Pulsing involves a bright highlight that slowly shifts to the background color.
    
    If the value is `nil`, highlight with an unchanging color until next command occurs.
    
    If `vhl/use-pulsing-visual-effect-p` is non-nil, but the return value of the function `vhl/pulse/available-p` is nil, then this flag is ignored.
    
    Default value is `nil`.

-   **`vhl/pulse-iterations`:** Number of iterations of a pulse animation for volatile highlights.
    
    Default value is `10`.

-   **`vhl/pulse-start-delay`:** Delay before pulse animation begins in seconds.
    
    Default value is `0.1`.

-   **`vhl/pulse-iteration-delay`:** Delay between iterations of the pulse animation in seconds.
    
    Default value is `0.03`.

-   **`Vhl/highlight-zero-width-ranges`:** If `t`, highlight the positions of zero-width ranges.
    
    For example, if a deletion is highlighted, then the position where the deleted text used to be would be highlighted.
    
    Default value is `nil`.

And you can change the color for volatile highlighting by editing the face:

-   **`vhl/default-face`:** Face used for volatile highlights.

When you edit the face, make sure the background color does not overlap with the default background color, otherwise the highlights will not be visible.


## Example snippets for using volatile highlights with other packages


### vip

```emacs-lisp
;;-----------------------------------------------------------------------------
;; Supporting vip-mode.
;;-----------------------------------------------------------------------------
(vhl/define-extension 'vip 'vip-yank)
(vhl/install-extension 'vip)
```


### evil

```emacs-lisp
;;-----------------------------------------------------------------------------
;; Supporting evil-mode.
;;-----------------------------------------------------------------------------
(vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
                      'evil-paste-pop 'evil-move)
(vhl/install-extension 'evil)
```


### undo-tree

```emacs-lisp
;;-----------------------------------------------------------------------------
;; Supporting undo-tree.
;;-----------------------------------------------------------------------------
(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
(vhl/install-extension 'undo-tree)
```
