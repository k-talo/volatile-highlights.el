# Appearance and Tuning

This guide covers how to pick a face for volatile highlights and how to tune animations.

- For installation and basics, see the README: ../README.md
- For extension patterns and APIs, see: ./extending.md

## Choosing a Face

Default behavior
- The default face `vhl/default-face` inherits from `secondary-selection` to stay
  compatible with many themes out of the box. This choice keeps volatile
  highlights visible yet conservative for most users.

What to aim for
- Draw the eye briefly without being mistaken for a selection or an error.
- High enough contrast on your theme; not as loud as `region`, more noticeable
  than `lazy-highlight` in many themes.

Common options
- Compatibility (default): inherit `secondary-selection`.
- Selection-adjacent: derive a background from `region`, slightly lighter on
  dark themes or slightly darker on light themes.
- Search-adjacent: reuse the background of `lazy-highlight` to align with
  search navigation.
- Accent: inherit `highlight` (often a mild yellow) if it fits your theme.

Theme-derived examples

Region-derived background
```emacs-lisp
(let* ((base (face-attribute 'region :background nil t))
       (mode (frame-parameter nil 'background-mode))
       (derived (if (eq mode 'dark)
                    (color-lighten-name base 15)
                  (color-darken-name base 10))))
  (set-face-attribute 'vhl/default-face nil :inherit nil :background derived))
```

Lazy-highlight background
```emacs-lisp
(let ((base (face-attribute 'lazy-highlight :background nil t)))
  (set-face-attribute 'vhl/default-face nil :inherit nil :background base))
```

Reapply after theme changes
```emacs-lisp
(defun my/vhl-derive-face-after-theme (&rest _)
  ;; Reapply one of the derivation snippets above.
  )
(advice-add 'enable-theme :after #'my/vhl-derive-face-after-theme)
```

Notes
- Avoid making `vhl/default-face` identical to `region` to prevent confusion
  with active selections.
- Avoid reusing backgrounds associated with error or warning faces.
- VHL is intended for transient, local emphasis; extending to EOL is usually
  unnecessary and may reduce readability.

## Animation Styles

Choose animation via `vhl/animation-style`:
- `static`: no animation; lowest CPU and most responsive.
- `fade-in`: gradually appear, then stay until the next command.
- `pulse`: fade out, then clear automatically when the animation finishes.

## Tuning Animations

Core variables
- `vhl/animation-mid-frames`: internal frames between the start and end colors.
  Higher numbers insert more frames between those endpoints and increase CPU
  cost. Typical range: 4-10.
- `vhl/animation-frame-interval`: per-frame (tick) delay in seconds. Lower is
  faster; a common range is 0.03-0.05.
- `vhl/animation-start-delay` (default: 0.15s): delay before the animation begins
  (seconds). For
  animated styles, the delay is counted after Emacs becomes idle (idle timer),
  which avoids interrupting rapid command sequences. Values around 0.01 give
  almost instant feedback, but the animation may trigger after every command.
  Lengthen it toward 0.2 seconds if rapid edits make the animation feel noisy.
  For `static`, highlights appear immediately.
- `vhl/animation-prestart-opacity`: opacity of the hint shown before
  idle-driven animations. Increase this when you want lighter or darker instant
  feedback during rapid command sequences. Leave it `nil` to use the style
  defaults (`fade-in` = 0.0, `pulse` = 1.0).

Suggested starting points (tune to taste)
- `fade-in`: `vhl/animation-mid-frames` = 4, `vhl/animation-frame-interval` = 0.03
- `pulse`: `vhl/animation-mid-frames` = 10, `vhl/animation-frame-interval` = 0.05

Troubleshooting
- If highlights are hard to see, increase contrast by changing
  `vhl/default-face` or pick another base (region/lazy-highlight/highlight).
- On limited-color frames (e.g., some TTYs), animations may be skipped and
  fall back to static; this is expected.
- Avoid stacking multiple highlight systems for the same action (e.g., xref
  pulse plus VHL) unless you want the combined effect.
