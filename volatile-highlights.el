;;; volatile-highlights.el --- Transient visual feedback for edits -*- lexical-binding: t; -*-

;; Copyright (C) 2001, 2010-2016, 2024-2025 K-talo Miyazaki

;; Author: K-talo Miyazaki <Keitaro.Miyazaki@gmail.com>
;; Maintainer: K-talo Miyazaki <Keitaro.Miyazaki@gmail.com>
;; Created: 03 October 2001. (as utility functions in my `.emacs' file.)
;;          14 March   2010. (re-written as library `volatile-highlights.el')
;; Keywords: editing emulations convenience wp
;; URL: https://github.com/k-talo/volatile-highlights.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.20
;; Contributed by: Ryan Thompson and Le Wang.

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Overview
;; --------
;; `volatile-highlights-mode' is a global minor mode that provides
;; transient, visual feedback for common editing operations.  After an
;; operation completes, the affected text is highlighted briefly and
;; cleared on the next user command.
;;
;; Features (when the mode is enabled):
;; - undo: highlight the text restored by undo
;; - yank and yank-pop: highlight inserted text
;; - kill/delete: show where text used to be (optionally as a point)
;; - replacements: highlight results of `query-replace' and `replace-string'
;; - transposition commands: highlight swapped text from transpose-* and
;;   transpose-regions operations
;; - definitions: Emacs 25.1+ uses xref; older Emacs use find-tag
;; - occur: Emacs < 28 only (Emacs 28+ has built-in occur highlighting)
;; - non-incremental search commands
;; - hideshow: highlight shown blocks
;;
;; Customization
;; ------------
;; Customize the group `volatile-highlights' (M-x customize-group RET
;; volatile-highlights RET).
;; - vhl/animation-style (default: 'static): animation style
;;   for highlights.
;;   - 'static  : No animation.
;;   - 'fade-in : Fade in, then keep highlight until next command.
;;   - 'pulse   : Fade out, then clear automatically.
;; - vhl/highlight-zero-width-ranges (default: nil): also mark deletion
;;   points as a 1-character highlight.
;; - vhl/default-face: face used for highlights; adjust to match your theme.
;; Per-feature toggles are available via `vhl/use-<name>-extension-p'.
;; The xref integration is available but defaults to disabled.
;;
;; Notes for developers
;; --------------------
;; - The public APIs `vhl/add-range' and `vhl/add-position' are
;;   mode-aware and act as no-ops when `volatile-highlights-mode' is
;;   disabled.
;; - To integrate with your own commands, compute the range to
;;   highlight and call these APIs.  See docs/extending.md for patterns
;;   and examples.
;;
;; See README.md for installation and quick configuration, and
;; docs/extending.md for integration patterns and API reference.
;;
;; INSTALLING
;; ----------
;; Place this file on your `load-path', then enable the mode globally:
;;
;;   (require 'volatile-highlights)
;;   (volatile-highlights-mode 1)
;;
;;    - `vhl/animation-start-delay'
;;      Delay before the highlight animation begins in seconds.  For
;;      animated styles, this delay is counted after Emacs becomes idle
;;      (idle timer) to avoid interrupting rapid command sequences; set
;;      to 0 to start as soon as Emacs is idle.  For 'static, highlights
;;      appear immediately without waiting for idle.
;;
;;      Default value is `0.15'.
;;
;;    - `vhl/animation-frame-interval'
;;      Delay between animation ticks in seconds.
;;
;;      Default value is `0.04'.
;;
;;    - `vhl/highlight-zero-width-ranges'
;;      If `t', highlight the positions of zero-width ranges.
;;
;;      For example, if a deletion is highlighted, then the position
;;      where the deleted text used to be would be highlighted.
;;
;;      Default value is `nil'.
;;
;; And you can change the color for volatile highlighting by editing the face:
;;
;;    - `vhl/default-face'
;;      Face used for volatile highlights.
;;
;; When you edit the face, make sure the background color does not overlap with
;; the default background color, otherwise the highlights will not be visible.
;;
;;
;; EXAMPLE SNIPPETS FOR USING VOLATILE HIGHLIGHTS WITH OTHER PACKAGES
;; ==================================================================
;;
;; - vip-mode
;;
;;   (vhl/define-extension 'vip 'vip-yank)
;;   (vhl/install-extension 'vip)
;;
;; - evil-mode
;;
;;   (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
;;                         'evil-paste-pop 'evil-move)
;;   (vhl/install-extension 'evil)
;;
;; - undo-tree
;;
;;   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;   (vhl/install-extension 'undo-tree)


;;; Change Log:
;;
;; v1.18 Tue Jul 22 16:20:00 2025 JST
;;   - Fix: Correct group for vhl/use-pulsing-visual-effect-p
;;     The variable `vhl/use-pulsing-visual-effect-p` was incorrectly assigned
;;     to the `pulse` group. This commit moves it to the more appropriate
;;     `volatile-highlights-mode` group.
;; v1.17 Sun Jul 20 16:43:18 2025 JST
;;   - Supress a compiler warning "'easy-mmode-define-minor-mode' is an
;;     obsolete alias (as of 30.1)".
;;   - New user option `vhl/use-pulsing-visual-effect-p'.
;;     When this option has non-nil value, volatile highlights will be cleared
;;     with pulsing visual effect (fix #16).
;;     Note that the default value is `nil'.
;;     This feature is inspired by the library `pulse.el', which was written
;;     by Eric M. Ludlam.
;;   - Do not require etags
;;     This is not necessary for the advice mechanism to work, and causes
;;     unnecessary loading of etags and its dependencies at startup if
;;     volatile-highlights-mode is enabled.
;;
;; v1.16 Sat Sep 14 06:57:51 2024 JST
;;   - This release is a maintenance release to support new versions of Emacs.
;;     There are no notable new features, but the following fixes have been made.
;;   - Translate `defadvice' to `advice-add'.
;;   - Fixed a bug, `vhl/ext/occur' does not make highlights properly.
;;   - 'occur' command on Emacs >= 28 has volatile highlights feature, so
;;     'vhl/ext/occur' is not required. (fix #26)
;;   - Adapt to lexical binding.
;;   - Add year 2024 to copyright line.
;;   - Use `cl-lib'. (fix #21 and #23)
;;
;; v1.15 Sun Jun 12 10:40:31 2016 JST
;;   - Update documents, example snippets for other packages,
;;     regarding #14.
;;
;; v1.14 Sun Jun 12 10:12:30 2016 JST
;;   - Update documents, especially supporting `evil-mode',
;;     regarding #13.
;;   - Fixed a bug #14, an extension won't be loaded properly
;;     when it was installed by `vhl/install-extension'.
;;
;; v1.13 Sat May 21 11:02:36 2016 JST
;;   - Fixed a bug that highlighting was not working with nested volatile
;;     highlighting aware operations like `yak-pop'.
;;
;; v1.12  Sun Feb 21 19:09:29 2016 JST
;;   - Added autoload cookie.
;;
;; v1.11  Sun Oct  5 13:05:38 2014 JST
;;   - Fixed an error "Symbol's function definition is void: return",
;;     that occurs when highlight being created with `hideshow' commands.
;;
;; v1.10  Thu Mar 21 22:37:27 2013 JST
;;   - Use inherit in face definition when detected.
;;   - Suppress compiler warnings regarding to emacs/xemacs private
;;     functions by file local variable.
;;
;; v1.9  Tue Mar  5 00:52:35 2013 JST
;;   - Fixed errors in shell caused by dummy functions.
;;
;; v1.8  Wed Feb 15 00:08:14 2012 JST
;;   - Added "Contributed by: " line in header.
;;   - Added extension for hideshow.
;;
;; v1.7  Mon Feb 13 23:31:18 2012 JST
;;   - Fixed a bug required features are not loaded.
;;
;; v1.6  Thu Feb  2 06:59:48 2012 JST
;;   - Removed extensions for non standard features.
;;   - Suppress compiler warning "function `vhl/.make-list-string'
;;     defined multiple times".
;;   - Fixed compiler error "Symbol's function definition is void:
;;     vhl/.make-list-string".
;;
;;  v1.5  Tue Jan 31 22:19:04 2012 JST
;;   - Added extension for highlighting the position where text was
;;     killed from.
;;   - Added extension for highlighting the position where text was
;;     deleted from.
;;   - Provide a macro `vhl/define-extension' for easily defining new
;;     simple extensions with a single line of code. For usage
;;     examples, see the definitions of the undo, yank, kill, and
;;     delete extensions.
;;
;;  v1.4  Sun Jan 15 20:23:58 2012 JST
;;   - Suppress compiler warnings regarding to emacs/xemacs private
;;     functions.
;;   - Fixed bugs which occurs to xemacs.
;;
;;  v1.3, Sat Dec 18 16:44:14 2010 JST
;;   - Added extension for non-incremental search operations.
;;   - Fixed a bug that highlights won't be appear when
;;     occurrences is in folded line.
;;
;;  v1.2, Tue Nov 30 01:07:48 2010 JST
;;   - In `vhl/ext/occur', highlight all occurrences.
;;
;;  v1.1, Tue Nov  9 20:36:09 2010 JST
;;   - Fixed a bug that mode toggling feature was not working.

;;; Code:

(defconst vhl/version "1.20")

(eval-when-compile
  (require 'cl-lib)
  (require 'advice))
(require 'cl-lib)
(require 'color)
(require 'thingatpt)


;;;============================================================================
;;;
;;;  Private Variables.
;;;
;;;============================================================================

(defvar vhl/.hl-lst nil
  "List of volatile highlights.")

(defvar vhl/pulse/.faces-to-pulse-lst nil
  "List of faces which will be pulsed on next idle time.")

(defvar vhl/pulse/.timer-cb nil
  "Callback function to generate pulse animation with timer.")


;;;============================================================================
;;;
;;;  Faces.
;;;
;;;============================================================================

;;;###autoload
(defgroup volatile-highlights nil
  "Visual feedback on operations."
  :group 'editing)

;;;###autoload
(defface vhl/default-face
  '((t :inherit secondary-selection))
  "Face used for volatile highlights.

Adjust this face to match your theme for clear, unobtrusive feedback.
For guidance on choosing or deriving colors from your theme, see the
project docs: docs/appearance-and-tuning.md."
  :group 'volatile-highlights)


;;;============================================================================
;;;
;;;  Minor Mode Definition.
;;;
;;;============================================================================
;;;###autoload
(define-minor-mode volatile-highlights-mode
 "Global minor mode for transient visual feedback on common operations.

When enabled, operations such as undo, yank, kill/delete,
definition jumps (xref on Emacs 25.1+, `find-tag' on older Emacs),
occur (on Emacs < 28), non-incremental search, and hideshow will
briefly highlight the affected text.  Highlights are cleared on the
next user command.

Customize the group `volatile-highlights' for per-feature toggles
and appearance."
 :global t
 :init-value nil
 :lighter " VHl"
 (if volatile-highlights-mode
     (vhl/load-extensions)
   (vhl/unload-extensions)))

(defcustom vhl/animation-style 'static
  "Animation style for volatile highlighting.

Choose how highlights animate:
 - \\='static: No animation; keep a solid highlight until the next command.
 - \\='fade-in: Fade in from the default background to the highlight color,
   then keep the solid highlight until the next command.
 - \\='pulse: Fade out from the highlight color toward the default background,
   then clear automatically when the animation ends.

Timing and responsiveness:
  - Animated styles run on an idle timer after Emacs becomes idle, and
   honor `vhl/animation-start-delay'.  This avoids animations interrupting
   rapid command sequences.
 - \\='static shows highlights immediately (no idle wait) and is the most
   responsive option.

If an animation style is selected but `vhl/pulse/available-p' returns nil
on the current frame, fall back to static behavior."
  :group 'volatile-highlights
  :type '(choice (const :tag "Static (no animation)" static)
                 (const :tag "Fade-in then stay" fade-in)
                 (const :tag "Pulse (fade-out)" pulse)))

(defcustom vhl/use-pulsing-visual-effect-p nil
  "Obsolete.  Use `vhl/animation-style' instead.

Legacy configurations may still toggle this variable; while it remains
available for compatibility it no longer changes `vhl/animation-style'
directly.  Support for it is handled inside `vhl/pulse/.prepare-for-face'
and will be removed in a future major release."
  :group 'volatile-highlights
  :type '(choice (const :tag "Static (no animation)" nil)
                 (const :tag "Fade-in then stay" -1)
                 (other :tag "Pulse (fade-out)" t))
  :set (lambda (sym val)
         (set-default sym val)))

(make-obsolete-variable 'vhl/use-pulsing-visual-effect-p
                        'vhl/animation-style "1.19")

;; Backward compatibility for renamed animation defcustoms.
;; Keep old `vhl/pulse-*' user options as obsolete aliases. Declare
;; aliases before their referents to satisfy the byte-compiler.
(define-obsolete-variable-alias 'vhl/pulse-iterations
  'vhl/animation-mid-frames "1.19")
(define-obsolete-variable-alias 'vhl/pulse-start-delay
  'vhl/animation-start-delay "1.19")
(define-obsolete-variable-alias 'vhl/pulse-iteration-delay
  'vhl/animation-frame-interval "1.19")

(defcustom vhl/animation-mid-frames 9
  "Number of internal frames between the start and end colors.

The animation always shows the start frame first and the end frame
last; setting this value to 0 therefore yields just the start and stop
colors.  Higher values are smoother but increase CPU cost.  Applies to
both \\='fade-in and \\='pulse styles.  Typical values range from 4 to 10.
Suggested starting points: \\='fade-in: 4, \\='pulse: 10."
  :type 'number
  :group 'volatile-highlights)

(defcustom vhl/animation-start-delay 0.15
  "Delay (seconds) before starting the highlight animation.

For animated styles (\\='fade-in or \\='pulse), this delay is counted after
Emacs becomes idle; the animation runs on an idle timer.  This design
avoids animations interrupting rapid command sequences and adding
perceived lag during bursts of edits.  Set this to 0 to start as soon
as Emacs becomes idle (still after the command returns).

For \\='static, there is no idle wait and highlights appear immediately,
providing the best responsiveness.  Around 0.01 seconds yields almost
instant feedback; increase the delay toward 0.2 seconds if rapid edits
make the animation feel noisy."
  :type 'number
  :group 'volatile-highlights)

(defcustom vhl/animation-prestart-opacity nil
  "Opacity of the prestart hint shown before animated highlights.

Animated styles wait for Emacs to become idle, so rapid command
sequences would otherwise only reveal the last highlight.  The
prestart hint appears immediately while the full animation still runs
later.  When nil, choose a style-appropriate default: 0.0 for \\='fade-in
and 1.0 for \\='pulse.  Otherwise use a float between 0.0 (same as the
default background) and 1.0 (fully opaque highlight color)."
  :type '(choice (const :tag "Auto (style default)" nil)
                 (number :tag "Opacity"))
  :group 'volatile-highlights)

(defcustom vhl/animation-frame-interval 0.04
  "Delay between animation frame ticks in seconds.

Lower values advance the animation faster but can cost more CPU; higher
values slow it down.  Applies to both \\='fade-in and \\='pulse styles.
Typical values range from 0.03 to 0.05.  Suggested starting points:
\\='fade-in: 0.03, \\='pulse: 0.05."
  :type 'number
  :group 'volatile-highlights)

(define-obsolete-variable-alias 'Vhl/highlight-zero-width-ranges
  'vhl/highlight-zero-width-ranges "1.19")

(defcustom vhl/highlight-zero-width-ranges nil
  "When non-nil, also mark deletion points as 1-character highlights.

This is useful to indicate where text used to be after a delete/kill
operation.  The helper `vhl/add-position' respects this setting."
  :type 'boolean
  :group 'volatile-highlights)


;;;============================================================================
;;;
;;;  Public Functions/Commands.
;;;
;;;============================================================================

;;-----------------------------------------------------------------------------
;; (vhl/add-range BEG END &OPTIONAL BUF FACE) => VOID
;;-----------------------------------------------------------------------------
;;;###autoload
(defun vhl/add-range (beg end &optional buf face)
  "Add a transient highlight for the region [BEG, END) in buffer BUF.

The highlight uses FACE (defaults to `vhl/default-face').  When BUF is
nil, the current buffer is used.

Highlights are cleared on the next user command.  When
`volatile-highlights-mode' is disabled, this function is a no-op."
  (when volatile-highlights-mode
    (let* ((face (or face 'vhl/default-face))
		   hl)
      ;; Prepare for pulse animatnion.
      (vhl/pulse/.prepare-for-face face)
      
      ;; Highlight should be made after pulse animation setting is finished.
      (setq hl (vhl/.make-hl beg end buf face))
      ;; Remember this new highlight.
      (setq vhl/.hl-lst (cons hl vhl/.hl-lst))
      
	  (add-hook 'pre-command-hook 'vhl/clear-all))))
(define-obsolete-function-alias 'vhl/add 'vhl/add-range "1.5")

;;-----------------------------------------------------------------------------
;; (vhl/add-position POS &OPTIONAL BUF FACE) => VOID
;;-----------------------------------------------------------------------------
;;;###autoload
(defun vhl/add-position (pos &rest other-args)
  "Mark buffer position POS as a 1-character highlight.

Does nothing unless `vhl/highlight-zero-width-ranges' is non-nil.
Optional argument OTHER-ARGS are the same as for `vhl/add-range'.  When
`volatile-highlights-mode' is disabled, this function is a no-op."
  (when (and vhl/highlight-zero-width-ranges (not (zerop (buffer-size))))
    (when (> pos (buffer-size))
        (setq pos (- pos 1)))
    (apply 'vhl/add-range pos (+ pos 1) other-args)))

;;-----------------------------------------------------------------------------
;; (vhl/clear-all) => VOID
;;-----------------------------------------------------------------------------
;;;###autoload
(defun vhl/clear-all ()
  "Clear all volatile highlight overlays."
  (interactive)
  (while vhl/.hl-lst
	(vhl/.clear-hl (car vhl/.hl-lst))
	(setq vhl/.hl-lst
		  (cdr vhl/.hl-lst)))
  (vhl/pulse/reset)
	  (remove-hook 'pre-command-hook 'vhl/clear-all))

;;-----------------------------------------------------------------------------
;; (vhl/force-clear-all) => VOID
;;-----------------------------------------------------------------------------
;;;###autoload
(defun vhl/force-clear-all ()
  "Unconditionally clear all volatile highlight overlays in the current buffer."
  (interactive)
  (vhl/.force-clear-all-hl))


;;;============================================================================
;;;
;;;  Private Functions.
;;;
;;;============================================================================

;;-----------------------------------------------------------------------------
;; (vhl/.make-hl BEG END BUF FACE) => HIGHLIGHT
;;-----------------------------------------------------------------------------
(defun vhl/.make-hl (beg end buf face)
  "Create and return a highlight overlay from BEG to END in BUF using FACE."
  (let ((hl (make-overlay beg end buf)))
    (overlay-put hl 'face face)
    (overlay-put hl 'priority 1)
    (overlay-put hl 'volatile-highlights t)
    hl))

;;-----------------------------------------------------------------------------
;; (vhl/.clear-hl HIGHLIGHT) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/.clear-hl (hl)
  "Clear one highlight overlay HL."
  (when (overlayp hl)
    (delete-overlay hl)))

;;-----------------------------------------------------------------------------
;; (vhl/.force-clear-all-hl) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/.force-clear-all-hl ()
  "Unconditionally clear all volatile highlight overlays in the current buffer."
  (save-restriction
    (widen)
    (mapcar (lambda (hl)
              (and (overlay-get hl 'volatile-highlights)
                   (vhl/.clear-hl hl)))
            (overlays-in (point-min) (point-max)))))


;;;============================================================================
;;;
;;;  Public Functions/Commands for pulsing.
;;;
;;;============================================================================

;;-----------------------------------------------------------------------------
;; (vhl/pulse/available-p) => BOOL
;;-----------------------------------------------------------------------------
(defun vhl/pulse/available-p ()
  ;; This part is taken from `pulse-available-p' in
  ;; `pulse.el' by Eric M. Ludlam.
  "Return non-nil if pulsing is available on the current frame."
  (condition-case nil
      (let ((v (color-values (face-background 'default))))
        (numberp (car-safe v)))
    (error nil)))

;;-----------------------------------------------------------------------------
;; (vhl/pulse/reset FACE) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/pulse/reset ()
  "Reset pulsing state for volatile highlight overlays."
  (interactive)
  (dolist (face vhl/pulse/.faces-to-pulse-lst)
    (vhl/pulse/reset-face face))
  (setq vhl/pulse/.faces-to-pulse-lst nil)
  (when vhl/pulse/.timer-cb
    (cancel-timer vhl/pulse/.timer-cb))
  (setq vhl/pulse/.timer-cb nil))

;;-----------------------------------------------------------------------------
;; (vhl/pulse/reset-face FACE) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/pulse/reset-face (face)
  "Restore the background color of FACE and forget pulsing metadata.

This resets any temporary attributes used for the animation so future
highlights start from the face's original background."
  (interactive (list (read-face-name "Reset face for pulse"
                                     (or (face-at-point t) 'default)
                                     t)))
  (let ((orig-bg-color (and (facep face)
                            (get face 'vhl/pulse/orig-bg-color))))
    (when orig-bg-color
      (set-face-background face orig-bg-color)
      (put face 'vhl/pulse/orig-bg-color nil)
      (put face 'vhl/pulse/prestart-bg-color nil)
      (put face 'vhl/pulse/gradient-bg-color-lst nil))))


;;;============================================================================
;;;
;;;  Private Functions/Commands for pulsing.
;;;
;;;============================================================================
;; Naming note:
;; The `vhl/pulse/*' namespace is used as the umbrella for the animation
;; engine, covering both the classic "pulse" (fade-out) effect and
;; the "fade-in" effect. We intentionally keep the "pulse" terminology
;; in recognition of, and with respect to, Eric M. Ludlam's `pulse.el',
;; portions of which inspired and are adapted here.

;;-----------------------------------------------------------------------------
;; (vhl/pulse/.mix-rgb FROM TO AMOUNT) => LIST OF RGB COMPONENTS AS NUMBERS
;;-----------------------------------------------------------------------------
(defun vhl/pulse/.mix-rgb (from to amount)
  "Linearly blend two RGB lists FROM and TO by AMOUNT (0.0-1.0)."
  (cl-mapcar (lambda (from to)
               (+ (* (- 1 amount) from) (* amount to)))
             from to))

;;-----------------------------------------------------------------------------
;; (vhl/pulse/.make-color-gradient FACE) => LIST OF COLOR HEX STRINGS
;;-----------------------------------------------------------------------------
(defun vhl/pulse/.make-color-gradient (face)
  "Return a list of gradient colors for animating FACE.

When `vhl/animation-style' is \\='fade-in, generate a gradient
from the default background to the highlight color.  Otherwise generate
the standard pulse (highlight color to default background).  The
starting point is influenced by `vhl/animation-prestart-opacity'."

  ;; This logic is adapted from `pulse-momentary-highlight-overlay' in
  ;; `pulse.el' by Eric M. Ludlam.
  (let* ((fade-in-p (eq vhl/animation-style 'fade-in))
         (opacity (or (and (numberp vhl/animation-prestart-opacity)
                           ;; Clamp the value between 0.0 and 1.0.
                           (min 1.0 (max 0.0 vhl/animation-prestart-opacity)))
                      ;; When the value is not a number, fall back to a
                      ;; style-appropriate default.
                      (if fade-in-p 0.0 1.0)))
         (bg-name (or (face-attribute 'default :background nil t)
                      (frame-parameter nil 'background-color)))
         (hl-name (face-background face nil 'default))
         (hl-rgb (or (ignore-errors (color-name-to-rgb hl-name))
                     '(1.0 1.0 1.0))) ;; Fall back to "#FFFFFF"
         (bg-rgb (or (ignore-errors (color-name-to-rgb bg-name))
                     '(0.0 0.0 0.0))) ;; Fall back to "#000000"
         (prestart-rgb (vhl/pulse/.mix-rgb bg-rgb hl-rgb opacity))
         (start-rgb (if fade-in-p prestart-rgb hl-rgb))
         (stop-rgb (if fade-in-p hl-rgb bg-rgb))
         (mid-frames (if (numberp vhl/animation-mid-frames)
                         (max 0 (truncate vhl/animation-mid-frames))
                       0)))

    (put face 'vhl/pulse/prestart-bg-color (apply 'color-rgb-to-hex prestart-rgb))

    (mapcar (apply-partially 'apply 'color-rgb-to-hex)
            ;; `color-gradient' omits the start/stop colors; add them explicitly
            ;; so the animation always begins and ends with the expected frames.
            `(,start-rgb
              ,@(and (< 0 mid-frames) (color-gradient start-rgb stop-rgb mid-frames))
              ,stop-rgb))))

;;-----------------------------------------------------------------------------
;; (vhl/pulse/.prepare-for-face FACE) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/pulse/.prepare-for-face (face)
  "Prepare FACE for animation by saving original background and gradient."
  (let* ((vhl/animation-style (if (eq vhl/animation-style 'static)
                                  ;; Legacy compatibility: interpret the
                                  ;; obsolete `vhl/use-pulsing-visual-effect-p'
                                  ;; value while the user migrates to the new
                                  ;; defcustom.  Future releases can remove
                                  ;; this helper.
                                  (cond
                                   ((and (numberp vhl/use-pulsing-visual-effect-p)
                                         (<= vhl/use-pulsing-visual-effect-p 0))
                                    'fade-in)
                                   ((null vhl/use-pulsing-visual-effect-p)
                                    'static)
                                   (t
                                    'pulse))
                                vhl/animation-style))
         (use-anim-p (and (vhl/pulse/available-p)
                          (not (eq vhl/animation-style 'static)))))
    (when (and use-anim-p
               (facep face)
               ;; do nothing the face is already set up.
               (null (get face 'vhl/pulse/orig-bg-color)))
      
      ;; Make sure preparation for FACE is done just once.
      (when (not (memq face vhl/pulse/.faces-to-pulse-lst))
        
        ;; Just to be safe, clean up any meta information for animation;
        ;; then save new meta information, and then set start background color.
        (vhl/pulse/reset-face face)
        (put face 'vhl/pulse/orig-bg-color (face-background face nil 'default))
        (put face 'vhl/pulse/gradient-bg-color-lst
             (vhl/pulse/.make-color-gradient face))
        (set-face-background face (get face 'vhl/pulse/prestart-bg-color))
        
        ;; Remember which face will be pulsed on next idle time;
        ;; then make pulse animation on next idle time.
        (add-to-list 'vhl/pulse/.faces-to-pulse-lst face)
        (unless vhl/pulse/.timer-cb
          (setq vhl/pulse/.timer-cb
                (run-with-idle-timer
                 vhl/animation-start-delay nil #'vhl/pulse/.do-it)))))))
      

;;-----------------------------------------------------------------------------
;; (vhl/pulse/.do-it FACE) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/pulse/.do-it ()
  "Advance animation: step each scheduled face along its gradient.

On each tick, update background colors using the precomputed gradient.
If no colors remain, then:
 - For pulse (fade-out), clear highlights and reset state.
 - For fade-in, stop the timer but keep the static highlights until
   the next user command clears them."
  (let ((fade-in-p (eq vhl/animation-style 'fade-in))
        has-pending-gradient-colors-p)
    (dolist (face vhl/pulse/.faces-to-pulse-lst)
      (let ((gradient-colors (get face 'vhl/pulse/gradient-bg-color-lst)))
        (when gradient-colors
          (progn
            ;; Change background color of a face for the pulse animation.
            (set-face-background face (car gradient-colors))

            ;; Prepare gradient colors for next step.
            (setq gradient-colors (cdr gradient-colors))
            (put face 'vhl/pulse/gradient-bg-color-lst gradient-colors)
            (setq has-pending-gradient-colors-p
                  (or has-pending-gradient-colors-p gradient-colors))))))
    ;; Make delay.
    (sleep-for vhl/animation-frame-interval)

    (cond
     ((and has-pending-gradient-colors-p
           (not (input-pending-p)))
      ;; Step forward the pulse animation.
      (when vhl/pulse/.timer-cb
        (cancel-timer vhl/pulse/.timer-cb))
      (setq vhl/pulse/.timer-cb
            (run-with-timer 0 nil #'vhl/pulse/.do-it)))
     (t
      ;; When finished the pulse animation, clean up all highlights.
      (when (not fade-in-p)
        (vhl/clear-all))))))


;;;============================================================================
;;;
;;;  Functions to manage extensions.
;;;
;;;============================================================================
(defvar vhl/.installed-extensions nil
  "List of symbols naming installed volatile-highlights extensions.")

(defun vhl/install-extension (name &optional disabled-by-default-p)
  "Install extension NAME and define its user option.

Define the customizable variable `vhl/use-NAME-extension-p' (where
NAME is the extension name) which toggles tracking for NAME.  If
DISABLED-BY-DEFAULT-P is non-nil, the option defaults to nil; otherwise
it defaults to t."
  (let ((_fn-on  (intern (format "vhl/ext/%s/on" name)))
        (_fn-off (intern (format "vhl/ext/%s/off" name)))
        (cust-name (intern (format "vhl/use-%s-extension-p" name))))
    (cl-pushnew name vhl/.installed-extensions)
    (eval `(defcustom ,cust-name ,(not disabled-by-default-p)
             ,(format "A flag if highlighting support for `%s' is on or not." name)
             :type 'boolean
             :group 'volatile-highlights
             :set (lambda (sym-to-set val)
                    (set-default sym-to-set val)
                    (if val
                        (when volatile-highlights-mode
                          (vhl/load-extension (quote ,name)))
                      (vhl/unload-extension (quote ,name))))))))

(defun vhl/load-extension (name)
  "Load and activate extension NAME when its user option is enabled."
  (let ((fn-on  (intern (format "vhl/ext/%s/on" name)))
        (cust-name (intern (format "vhl/use-%s-extension-p" name))))
    (if (functionp fn-on)
        (when (and (boundp cust-name)
                   (eval cust-name))
          (apply fn-on nil))
      (message "[vhl] No load function for extension  `%s'" name))))

(defun vhl/unload-extension (name)
  "Deactivate and unload extension NAME if available."
  (let ((fn-off (intern (format "vhl/ext/%s/off" name))))
    (if (functionp fn-off)
        (apply fn-off nil)
      (message "[vhl] No unload function for extension  `%s'" name))))

(defun vhl/load-extensions ()
  "Load all installed volatile-highlights extensions."
  (dolist (name vhl/.installed-extensions)
    (vhl/load-extension name)))

(defun vhl/unload-extensions ()
  "Unload all installed volatile-highlights extensions."
  (dolist (name vhl/.installed-extensions)
    (vhl/unload-extension name)))


;;;============================================================================
;;;
;;;  Utility functions/macros for extensions.
;;;
;;;============================================================================
(defvar vhl/.after-change-hook-depth 0
  "Nest level counting active after-change tracking for VHL advice.")

(defun vhl/.push-to-after-change-hook (_fn-name)
  "Start tracking buffer modifications during an advised call."
  ;; Debug
  ;; (if (zerop vhl/.after-change-hook-depth)
  ;;     (message "vlh: push: %s" _fn-name)
  ;;   (message "vlh: skip push: %s" _fn-name))
  (when (zerop vhl/.after-change-hook-depth)
    (add-hook 'after-change-functions
              'vhl/.make-vhl-on-change))
  (setq vhl/.after-change-hook-depth
        (1+ vhl/.after-change-hook-depth)))

(defun vhl/.pop-from-after-change-hook (_fn-name)
  "Stop tracking buffer modifications when the advised call finishes."
  (setq vhl/.after-change-hook-depth
        (1- vhl/.after-change-hook-depth))
  ;; Debug
  ;; (if (zerop vhl/.after-change-hook-depth)
  ;;     (message "vlh: pop: %s" _fn-name)
  ;;   (message "vlh: skip pop: %s" _fn-name))
  (when (zerop vhl/.after-change-hook-depth)
    (remove-hook 'after-change-functions
                 'vhl/.make-vhl-on-change)))

(defun vhl/.make-vhl-on-change (beg end len-removed)
"Record a highlight for the change between BEG and END.

Highlight inserted text whenever the modified region has positive
width (END greater than BEG).  For pure deletions, where LEN-REMOVED
reports removed characters but END equals BEG, fall back to marking
the deletion point, subject to `vhl/highlight-zero-width-ranges'.  No
highlight is created when neither condition applies (for example,
notifications for text-property-only changes)."
  (let ((has-insert (> end beg))
        (has-delete (not (zerop len-removed))))
    (cond
     (has-insert
      ;; Highlight the newly inserted or replaced text.
      (vhl/add-range beg end))
     (has-delete
      ;; Pure deletion: optionally mark the point of removal.
      (vhl/add-position beg)))))

(defmacro vhl/give-advice-to-make-vhl-on-changes (fn-name)
  "Generate around-advice to track buffer modifications in FN-NAME.

Wrap FN-NAME so that buffer modifications during its call produce
volatile highlights."
  (let* ((ad-name (intern (concat "vhl/.advice-callback-fn/.make-vhl-on-"
                                  (format "%s" fn-name))))
         (g-orig-fn  (gensym))
         (g-orig-ret (gensym))
         (g-args     (gensym)))
    (or (symbolp fn-name)
        (error "vhl/give-advice-to-make-vhl-on-changes: `%s' is not type of symbol" fn-name))
    `(progn
       (defun ,ad-name (,g-orig-fn &rest ,g-args)
         (let (,g-orig-ret)
           (vhl/\.push-to-after-change-hook (quote ,fn-name))
           (unwind-protect (setq ,g-orig-ret (apply ,g-orig-fn ,g-args))
             (vhl/\.pop-from-after-change-hook (quote ,fn-name)))
           ,g-orig-ret))
       (advice-add (quote ,fn-name) :around (quote ,ad-name)))))

(defmacro vhl/cancel-advice-to-make-vhl-on-changes (fn-name)
  "Remove the around-advice installed for FN-NAME.

The advice was installed by `vhl/give-advice-to-make-vhl-on-changes'."
  (let ((ad-name (intern (concat "vhl/.advice-callback-fn/.make-vhl-on-"
                                  (format "%s" fn-name)))))
    `(advice-remove (quote ,fn-name) (quote ,ad-name))))

(defun vhl/require-noerror (feature &optional _filename)
  "Require FEATURE, returning nil instead of signaling a file error."
  (condition-case _c
      (require feature)
    (file-error nil)))

(eval-and-compile
;; Utility function by Ryan Thompson.
(defun vhl/.make-list-string (items)
  "Make an English-style list from a list of strings.

Convert a list of strings into a string that lists the ITEMS
separated by commas, as well as the word `and' before the last
item.  In other words, return a string of the way those items
would be listed in english.

This is included as a private support function for generating
lists of symbols to be included docstrings of auto-generated
extensions."
  (cl-assert (listp items))
  (cond ((null items)
         ;; Zero items
         "")
        ((null (cdr items))
         ;; One item
         (cl-assert (stringp (cl-first items)))
         (format "%s" (cl-first items)))
        ((null (cddr items))
         ;; Two items
         (cl-assert (stringp (cl-first items)))
         (cl-assert (stringp (cl-second items)))
         (apply 'format "%s and %s" items))
        ((null (cdddr items))
         ;; Three items
         (cl-assert (stringp (cl-first items)))
         (cl-assert (stringp (cl-second items)))
         (cl-assert (stringp (cl-third items)))
         (apply 'format "%s, %s, and %s" items))
        (t
         ;; 4 or more items
         (format "%s, %s" (cl-first items) (vhl/.make-list-string (cl-rest items)))))))

;; The following makes it trivial to define simple vhl extensions
(eval-and-compile
  (defun vhl/.extension-docstring (prefix name functions)
    "Build a docstring for a generated extension command.

Argument PREFIX is the verb (e.g., \"Turn on\"), NAME is the extension
symbol, and FUNCTIONS is the list of advised commands.

The docstring explains that the command is generated by
`vhl/define-extension' and linked to the `vhl/use-NAME-extension-p'
user option.  To avoid compiler warnings for excessive line width,
it formats long command lists by placing each entry on its own line."
    (let* ((names (mapcar (lambda (f)
                            (format "`%s'" (symbol-name (eval f))))
                          functions))
           (name-string (symbol-name (eval name)))
           (toggle-var (format "`vhl/use-%s-extension-p'" name-string))
           (body (if (cdr names)
                     (format "%s volatile highlighting for:\n%s"
                             prefix
                             (mapconcat (lambda (entry)
                                          (format "  %s" entry))
                                        names
                                        "\n"))
                   (format "%s volatile highlighting for %s." prefix (car names)))))
      (format "%s\n\nGenerated by `vhl/define-extension'\nCalled via %s"
              body toggle-var))))

(defmacro vhl/define-extension (name &rest functions)
  "Define a VHL extension called NAME and apply standard advice.

Apply volatile-highlights change tracking to each of FUNCTIONS so
insertions and deletions are highlighted.  Define interactive commands
`vhl/ext/NAME/on' and `vhl/ext/NAME/off' to enable or disable tracking
for NAME."
  (cl-assert (cl-first functions))
  (let* ((name-string (symbol-name (eval name)))
         (on-function-name (intern (format "vhl/ext/%s/on" name-string)))
         (on-body-form (cons
                        'progn
                        (mapcar (lambda (f)
                                  `(vhl/give-advice-to-make-vhl-on-changes ,(eval f)))
                                functions)))
         (on-doc-string (vhl/.extension-docstring "Turn on" name functions))

         (off-function-name (intern (format "vhl/ext/%s/off" name-string)))
         (off-body-form (cons
                         'progn
                         (mapcar (lambda (f)
                                   `(vhl/cancel-advice-to-make-vhl-on-changes ,(eval f)))
                                 functions)))
         (off-doc-string (vhl/.extension-docstring "Turn off" name functions)))
    `(progn
       (defun ,on-function-name ()
         ,on-doc-string
         (interactive)
         ,on-body-form)
       (defun ,off-function-name ()
         ,off-doc-string
         (interactive)
         ,off-body-form)
       nil)))


;;;============================================================================
;;;
;;;  Extensions.
;;;
;;;============================================================================

;;-----------------------------------------------------------------------------
;; Extension for supporting undo.
;;   -- Put volatile highlights on the text inserted by `undo'.
;;      (and may be `redo'...)
;;-----------------------------------------------------------------------------

(vhl/define-extension 'undo 'primitive-undo)
(vhl/install-extension 'undo)


;;-----------------------------------------------------------------------------
;; Extension for supporting yank/yank-pop.
;;   -- Put volatile highlights on the text inserted by `yank' or `yank-pop'.
;;-----------------------------------------------------------------------------

(vhl/define-extension 'yank 'yank 'yank-pop)
(vhl/install-extension 'yank)

;;-----------------------------------------------------------------------------
;; Extension for supporting kill.
;;   -- Put volatile highlights on the positions where killed text
;;      used to be.
;;-----------------------------------------------------------------------------

(vhl/define-extension 'kill 'kill-region)
(vhl/install-extension 'kill)

;;-----------------------------------------------------------------------------
;; Extension for supporting `delete-region'.
;;   -- Put volatile highlights on the positions where deleted text
;;      used to be. This is not so reliable since `delete-region' is
;;      an inline function and is pre-compiled sans advice into many
;;      other deletion functions.
;;-----------------------------------------------------------------------------

(vhl/define-extension 'delete 'delete-region)
(vhl/install-extension 'delete)


;;-----------------------------------------------------------------------------
;; Extension for supporting transpose commands.
;;   -- Highlight swapped text from core transpose-* utilities.
;;-----------------------------------------------------------------------------

(vhl/define-extension 'transpose 'transpose-chars 'transpose-words
                      'transpose-lines 'transpose-sexps
                      'transpose-regions)
(vhl/install-extension 'transpose)



;;-----------------------------------------------------------------------------
;; Extension for supporting query-replace and related commands.
;;   -- Put volatile highlights on replacements done via `perform-replace'.
;;      Covers `query-replace', `query-replace-regexp', `replace-string',
;;      and friends.
;;-----------------------------------------------------------------------------

(vhl/define-extension 'query-replace 'perform-replace)
(vhl/install-extension 'query-replace)


;;-----------------------------------------------------------------------------
;; Extension for supporting etags.
;;   -- Put volatile highlights on the tag name which was found by `find-tag'.
;;-----------------------------------------------------------------------------
(defun vhl/ext/etags/.after-find-tag (tagname &optional _next-p _regexp-p)
  "Highlight the found TAGNAME after `find-tag'."
  (let ((len (length tagname)))
    (save-excursion
      (search-forward tagname)
      (vhl/add-range (- (point) len) (point)))))

  (defun vhl/ext/etags/on ()
  "Turn on volatile highlighting for `etags'."
  (interactive)
  (advice-add 'find-tag :after #'vhl/ext/etags/.after-find-tag))

(defun vhl/ext/etags/off ()
  "Turn off volatile highlighting for `etags'."
  (interactive)
  (advice-remove 'find-tag #'vhl/ext/etags/.after-find-tag))

(vhl/install-extension 'etags)


;;-----------------------------------------------------------------------------
;; Extension for supporting xref jumps.
;;   -- Add a non-pulsing volatile highlight after xref jump/return events.
;;-----------------------------------------------------------------------------

(defun vhl/ext/xref/.highlight-current-symbol-or-line ()
  "Highlight the current symbol or the current line as a fallback."
  (let* ((bnds (bounds-of-thing-at-point 'symbol))
         (beg (car-safe bnds))
         (end (cdr-safe bnds)))
    (if (and beg end)
        (vhl/add-range beg end)
      (vhl/add-range (line-beginning-position) (line-end-position)))))

(defun vhl/ext/xref/.after-jump (&rest _args)
  "Run after xref jump/return events to place a volatile highlight."
  (vhl/ext/xref/.highlight-current-symbol-or-line))

(defun vhl/ext/xref/on ()
  "Turn on volatile highlighting for `xref' jumps."
  (interactive)
  (when (vhl/require-noerror 'xref)
    (add-hook 'xref-after-jump-hook #'vhl/ext/xref/.after-jump)
    (when (boundp 'xref-after-return-hook)
      (add-hook 'xref-after-return-hook #'vhl/ext/xref/.after-jump))))

(defun vhl/ext/xref/off ()
  "Turn off volatile highlighting for `xref' jumps."
  (interactive)
  (when (featurep 'xref)
    (remove-hook 'xref-after-jump-hook #'vhl/ext/xref/.after-jump)
    (when (boundp 'xref-after-return-hook)
      (remove-hook 'xref-after-return-hook #'vhl/ext/xref/.after-jump))))

(vhl/install-extension 'xref t)


;;-----------------------------------------------------------------------------
;; Extension for supporting occur.
;;   -- Put volatile highlights on occurrence which is selected by
;;      `occur-mode-goto-occurrence' or `occur-mode-display-occurrence'.
;;-----------------------------------------------------------------------------
(defvar vhl/ext/occur/*occur-str* ""
  "String of the current occur line (sans line number), used for highlighting.")

(defun vhl/ext/occur/.before-hook-fn (&rest _args)
  "Capture the current occur line (without the line number prefix) before jumping."
  (save-excursion
    (let* ((bol (progn (beginning-of-line) (point)))
           (eol (progn (end-of-line) (point))))
      (setq vhl/ext/occur/*occur-str* (and bol eol
                                           ;; Skip line number.
                                           (replace-regexp-in-string
                                            "^[ \t]*[0-9]+:" ""
                                            (buffer-substring bol eol)))))))

(defun vhl/ext/occur/.find-face-ranges-in-str (str face)
  "Find ranges where the specified FACE is applied in STR.
Returns a list of (beg . end), or nil if not found."
  (let ((ptr 0)
        (len (length str))
        be-lst be)
    (while (/= ptr len)
      (setq be (vhl/ext/occur/.find-face-ranges-in-str-aux str face ptr))
      (if (= (car be) len)
          (setq ptr len)
        (setq be-lst (cons be be-lst))
        (setq ptr (cdr be))))
    (reverse be-lst)))

(defun vhl/ext/occur/.str-has-face-at-pos-p (str face pos)
  "Return non-nil if STR has FACE at position POS."
  (let ((found-face (get-text-property pos 'face str)))
    (cond
     ((listp found-face)
      (member face found-face))
     ((atom found-face)
      (eq face found-face))
     (t nil))))

(defun vhl/ext/occur/.find-face-ranges-in-str-aux (str face pos)
  "Return one FACE range (beg . end) in STR starting at POS."
  (let ((ptr pos)
        (len (length str))
        beg end)
    
    ;; Find beggining of face range
    (when (vhl/ext/occur/.str-has-face-at-pos-p str face ptr)
      (setq beg ptr))
    
    (while (not beg)
      (setq ptr (next-single-property-change ptr 'face str len))
      (if (vhl/ext/occur/.str-has-face-at-pos-p str face ptr)
          (setq beg ptr)
        (when (= ptr len) (setq beg ptr))))
    
    ;; Find end of face range
    (while (not end)
      (setq ptr (next-single-property-change ptr 'face str len))
      (if (not (vhl/ext/occur/.str-has-face-at-pos-p str face ptr))
          (setq end ptr)
        (when (= ptr len) (setq end ptr))))
    (cons beg end)))

(defun vhl/ext/occur/.after-hook-fn (&rest _args)
  "Highlight the corresponding text in the target buffer after occur jump."
  (let ((marker (and vhl/ext/occur/*occur-str*
                     (get-text-property 0 'occur-target vhl/ext/occur/*occur-str*)))
        (be-lst nil))
    (when marker
      ;; Detect position of each occurrence by scanning face
      ;; `list-matching-lines-face' put on them.
      (setq be-lst
            (vhl/ext/occur/.find-face-ranges-in-str vhl/ext/occur/*occur-str*
                                                    list-matching-lines-face))
      ;; Put volatile highlights on occurrences.
      (with-current-buffer (marker-buffer marker)
        (let* ((bol (save-excursion
                      (goto-char (marker-position marker))
                      (beginning-of-line)
                      (point))))
          (dolist (be be-lst)
            (let ((pt-beg (+ bol (car be)))
                  (pt-end (+ bol (cdr be))))
              ;; When the occurrence is in folded line,
              ;; put highlight over whole line which
              ;; contains folded part.
              (dolist (ov (overlays-at pt-beg))
                (when (overlay-get ov 'invisible)
                  ;;(message "INVISIBLE: %s" ov)
                  (save-excursion
                    (goto-char (overlay-start ov))
                    (beginning-of-line)
                    (setq pt-beg (min pt-beg (point)))
                    (goto-char (overlay-end ov))
                    (end-of-line)
                    (setq pt-end (max pt-end (point))))))

              (vhl/add-range pt-beg
                             pt-end
                             nil
                             list-matching-lines-face))))))))

(defun vhl/ext/occur/on ()
  "Turn on volatile highlighting for `occur'."
  (interactive)

  (if (< emacs-major-version 28)
      (progn
        (advice-add 'occur-mode-goto-occurrence
                    :before #'vhl/ext/occur/.before-hook-fn)
        (advice-add 'occur-mode-goto-occurrence
                    :after  #'vhl/ext/occur/.after-hook-fn)

        (advice-add 'occur-mode-display-occurrence
                    :before #'vhl/ext/occur/.before-hook-fn)
        (advice-add 'occur-mode-display-occurrence
                    :after  #'vhl/ext/occur/.after-hook-fn)

        (advice-add 'occur-mode-goto-occurrence-other-window
                    :before #'vhl/ext/occur/.before-hook-fn)
        (advice-add 'occur-mode-goto-occurrence-other-window
                    :after  #'vhl/ext/occur/.after-hook-fn))
    (message "Emacs 28+ has built-in occur highlighting; vhl/ext/occur is not needed.")))

(defun vhl/ext/occur/off ()
  "Turn off volatile highlighting for `occur'."
  (interactive)

  (when (< emacs-major-version 28)
    (advice-remove 'occur-mode-goto-occurrence #'vhl/ext/occur/.before-hook-fn)
    (advice-remove 'occur-mode-goto-occurrence #'vhl/ext/occur/.after-hook-fn)
    
    (advice-remove 'occur-mode-display-occurrence #'vhl/ext/occur/.before-hook-fn)
    (advice-remove 'occur-mode-display-occurrence #'vhl/ext/occur/.after-hook-fn)
    
    (advice-remove 'occur-mode-goto-occurrence-other-window #'vhl/ext/occur/.before-hook-fn)
    (advice-remove 'occur-mode-goto-occurrence-other-window #'vhl/ext/occur/.after-hook-fn)))

;; `occur' command on Emacs >= 28 has volatile highlight feature,
;; so `vhl/ext/occur' is not required.
(when (< emacs-major-version 28)
  (vhl/install-extension 'occur))


;;-----------------------------------------------------------------------------
;; Extension for non-incremental search operations.
;;   -- Put volatile highlights on the text found by non-incremental search
;;      operations.
;;-----------------------------------------------------------------------------

(defun vhl/ext/nonincremental-search/.filter-return-fn (retval)
  "Highlight the last match when RETVAL is non-nil."
  (when retval
    (vhl/add-range (match-beginning 0) (match-end 0) nil 'match)))

(defun vhl/ext/nonincremental-search/on ()
  "Turn on volatile highlighting for non-incremental search operations."
  (interactive)
  (when (vhl/require-noerror 'menu-bar nil)
    (advice-add 'nonincremental-search-forward
                :filter-return
                #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-search-backward
                :filter-return
                #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-re-search-forward
                :filter-return
                #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-re-search-backward
                :filter-return
                #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-repeat-search-forward
                :filter-return
                #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-repeat-search-backward
                :filter-return
                #'vhl/ext/nonincremental-search/.filter-return-fn)))

(defun vhl/ext/nonincremental-search/off ()
  "Turn off volatile highlighting for  non-incremental search operations."
  (interactive)
  (when (vhl/require-noerror 'menu-bar nil)
    (advice-remove 'nonincremental-search-forward
                   #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-search-backward
                   #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-re-search-forward
                   #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-re-search-backward
                   #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-repeat-search-forward
                   #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-repeat-search-backward
                   #'vhl/ext/nonincremental-search/.filter-return-fn)))

(vhl/install-extension 'nonincremental-search)


;;-----------------------------------------------------------------------------
;; Extension for hideshow.
;;   -- Put volatile highlights on the text blocks which are shown/hidden
;;      by hideshow.
;;-----------------------------------------------------------------------------

(defun vhl/ext/hideshow/vhl/around-hook-fn (orig-fn &rest args)
  "Around advice for hideshow to highlight the revealed block.

ORIG-FN is the original function, ARGS are its arguments."
  (let* ((bol (save-excursion (progn (beginning-of-line) (point))))
         (eol (save-excursion (progn (end-of-line) (point))))
         (ov-folded (car (delq nil
                               (mapcar #'(lambda (ov)
                                           (and (overlay-get ov 'hs)
                                                ov))
                                       (overlays-in bol (1+ eol))))))
         (boov (and ov-folded (overlay-start ov-folded)))
         (eoov (and ov-folded (overlay-end ov-folded)))
         retval)

    (setq retval (apply orig-fn args))

    (when (and boov eoov)
      (vhl/add-range boov eoov))
    retval))

(defun vhl/ext/hideshow/.activate ()
  "Activate volatile highlighting for `hideshow' reveal."
  (advice-add 'hs-show-block :around #'vhl/ext/hideshow/vhl/around-hook-fn))

(defun vhl/ext/hideshow/on ()
  "Turn on volatile highlighting for `hideshow'."
  (interactive)

  ;; Attach advice now; it will take effect even if `hs-show-block'
  ;; is defined later.
  (vhl/ext/hideshow/.activate))

(defun vhl/ext/hideshow/off ()
  "Turn off volatile highlighting for `hideshow'."
  (advice-remove 'hs-show-block #'vhl/ext/hideshow/vhl/around-hook-fn))

(vhl/install-extension 'hideshow)


;; Provide feature after successful load.
(provide 'volatile-highlights)

;;; volatile-highlights.el ends here
