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
- `vhl/animation-iterations`: steps per animation. Higher is smoother but costs
  more CPU. Typical range: 6-12.
- `vhl/animation-iteration-delay`: per-step delay in seconds. Lower is faster; a
  common range is 0.01-0.03.
- `vhl/animation-start-delay`: delay before the animation begins (seconds). For
  animated styles, the delay is counted after Emacs becomes idle (idle timer),
  which avoids interrupting rapid command sequences. Set to 0 to start as soon
  as Emacs becomes idle; 0.1-0.2 often keeps the UI responsive during bursts of
  edits. For `static`, highlights appear immediately.

Suggested starting points (tune to taste)
- `fade-in`: `vhl/animation-iterations` = 6, `vhl/animation-iteration-delay` = 0.03
- `pulse`: `vhl/animation-iterations` = 12, `vhl/animation-iteration-delay` = 0.05

Troubleshooting
- If highlights are hard to see, increase contrast by changing
  `vhl/default-face` or pick another base (region/lazy-highlight/highlight).
- On limited-color frames (e.g., some TTYs), animations may be skipped and
  fall back to static; this is expected.
- Avoid stacking multiple highlight systems for the same action (e.g., xref
  pulse plus VHL) unless you want the combined effect.
