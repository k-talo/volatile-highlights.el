# News

## 1.19 – 2025-09-21

### Animation updates
- Introduced a new animation mode: `'fade-in`;
  Choose animation mode via new defcustom `vhl/animation-style`, (`'static` | `'fade-in` | `'pulse`) ([`8752c8c`](https://github.com/k-talo/volatile-highlights.el/commit/8752c8c), [`cf3e74d`](https://github.com/k-talo/volatile-highlights.el/commit/cf3e74d), [`d82f01b`](https://github.com/k-talo/volatile-highlights.el/commit/d82f01b)).
- Introduced `vhl/animation-prestart-opacity` for immediate pre-flash hints ([`7cba75e`](https://github.com/k-talo/volatile-highlights.el/commit/7cba75e), [`e21445b`](https://github.com/k-talo/volatile-highlights.el/commit/e21445b)).
- Renamed animation defcustoms and marked the old `vhl/pulse-*` options obsolete ([`8752c8c`](https://github.com/k-talo/volatile-highlights.el/commit/8752c8c), [`7233c37`](https://github.com/k-talo/volatile-highlights.el/commit/7233c37), [`cf3e74d`](https://github.com/k-talo/volatile-highlights.el/commit/cf3e74d), [`d82f01b`](https://github.com/k-talo/volatile-highlights.el/commit/d82f01b), [`7cba75e`](https://github.com/k-talo/volatile-highlights.el/commit/7cba75e)):
  - `vhl/use-pulsing-visual-effect-p` -> `vhl/animation-style`
  - `vhl/pulse-start-delay` -> `vhl/animation-start-delay`
  - `vhl/pulse-iterations` -> `vhl/animation-mid-frames`
  - `vhl/pulse-iteration-delay` -> `vhl/animation-frame-interval`

### Highlight coverage

- Highlight replacements produced by `query-replace`, `replace-string`, etc. ([`8160ba4`](https://github.com/k-talo/volatile-highlights.el/commit/8160ba4)).
- Highlight transpose command results by `transpose-words`, `transpose-line`, etc. ([`19b3844`](https://github.com/k-talo/volatile-highlights.el/commit/19b3844)).
- Add a non-pulsing volatile highlight after `xref` jumps (default: off) ([`9b3121d`](https://github.com/k-talo/volatile-highlights.el/commit/9b3121d), [`d6ec9e1`](https://github.com/k-talo/volatile-highlights.el/commit/d6ec9e1)).

### Documentation & guides

- Update README and DOCS
  - Emphasize MELPA install and `use-package` configuration ([`c0eb319`](https://github.com/k-talo/volatile-highlights.el/commit/c0eb319));
    Replace `:hook` with `:config` to make sure `(volatile-highlights-mode 1)` ([`a6015bd`](https://github.com/k-talo/volatile-highlights.el/commit/a6015bd)).
  - Testing (ERT and byte-compile) ([`f33f256`](https://github.com/k-talo/volatile-highlights.el/commit/f33f256), [`e755c04`](https://github.com/k-talo/volatile-highlights.el/commit/e755c04)).
  - Clarified `xref` documentation regarding volatile highlights ([`28479b8`](https://github.com/k-talo/volatile-highlights.el/commit/28479b8), [`d6ec9e1`](https://github.com/k-talo/volatile-highlights.el/commit/d6ec9e1)).
  - Clarified `occur` documentation regarding to volatile highlights ([`360407f`](https://github.com/k-talo/volatile-highlights.el/commit/360407f)).
  - Expanded the extending guide: `docs/extending.md` (Quick Start, user/dev examples, internals, API) ([`6c81742`](https://github.com/k-talo/volatile-highlights.el/commit/6c81742), [`64b26ca`](https://github.com/k-talo/volatile-highlights.el/commit/64b26ca)).
  - Split appearance and tuning documentation: `docs/appearance-and-tuning.md`;
    Refreshed animation guidance;
    Replace setq with customize-set-variable in README;
    Add setopt notes (29.1+) ([`ea9f6b6`](https://github.com/k-talo/volatile-highlights.el/commit/ea9f6b6)).
  - Add tuning tips and suggested values;
    Update examples ([`ee3c7b8`](https://github.com/k-talo/volatile-highlights.el/commit/ee3c7b8))
- Adjust the appearance of the README
  - Align Overview wording with the package summary:
    "Transient visual feedback for edits.";
    Added README badges ([`7e48139`](https://github.com/k-talo/volatile-highlights.el/commit/7e48139)).
  - Added a volatile-highlights summary tagline to the README:
    "Highlights are volatile; they vanish on your next command." ([`23ab66e`](https://github.com/k-talo/volatile-highlights.el/commit/23ab66e)).
- Other document updats
  - Add LICENSE file and add LICENCE section to README ([`1a0f8b7`](https://github.com/k-talo/volatile-highlights.el/commit/1a0f8b7)).
  - Use standard email in headers;
    Add Issues link to README ([`9b3fe0b`](https://github.com/k-talo/volatile-highlights.el/commit/9b3fe0b)).

### Packaging & infrastructure
- Regression Fix
  - Fixed package autoload generation for package.el installs and added the missing autoload cookies ([`3952439`](https://github.com/k-talo/volatile-highlights.el/commit/3952439), [`66cf0b5`](https://github.com/k-talo/volatile-highlights.el/commit/66cf0b5)) [fix [#28](https://github.com/k-talo/volatile-highlights.el/issues/28)].
  - Ensured the default face inherits correctly from the active theme ([`63c1e3e`](https://github.com/k-talo/volatile-highlights.el/commit/63c1e3e), [`da88fd1`](https://github.com/k-talo/volatile-highlights.el/commit/da88fd1)).
- Compile & Test
  - Makefile
    - Add `Makefile` for `compile` and ERT `tests`; `EMACS`, `EMACSFLAGS` and `TMPDIR` flags ([`e755c04`](https://github.com/k-talo/volatile-highlights.el/commit/e755c04))
    - Add target `checkdoc`, `package-lint`, `lint`, `check`; `PKGDIR` flag ([`dea59aa`](https://github.com/k-talo/volatile-highlights.el/commit/dea59aa), [`b6fc3e4`](https://github.com/k-talo/volatile-highlights.el/commit/b6fc3e4))
  - Add ERT tests.
    - Add minimal ERT tests for `add-range`, `add-position`, `clear-all` ([`f33f256`](https://github.com/k-talo/volatile-highlights.el/commit/f33f256))
    - Add `yank`, `undo` tests and position clamp case ([`2dd309c`](https://github.com/k-talo/volatile-highlights.el/commit/2dd309c))
    - Add tests for `xref` ([`9b3121d`](https://github.com/k-talo/volatile-highlights.el/commit/9b3121d))
    - Run assertions with the minor mode enabled;
      Add tests ensuring `add-range/position` are no-ops when the mode is disabled ([`dcfcca1`](https://github.com/k-talo/volatile-highlights.el/commit/dcfcca1))
    - Add tests for `perform-replace` ([`8160ba4`](https://github.com/k-talo/volatile-highlights.el/commit/8160ba4))
    - Add tests for `animation-prestart-opacity` ([`7cba75e`](https://github.com/k-talo/volatile-highlights.el/commit/7cba75e))
  - LINT
    - `byte-compile` clean ([`66cf0b5`](https://github.com/k-talo/volatile-highlights.el/commit/66cf0b5), [`d26b952`](https://github.com/k-talo/volatile-highlights.el/commit/d26b952)).
    - `checkdoc` clean ([`f16d4e2`](https://github.com/k-talo/volatile-highlights.el/commit/f16d4e2), [`ee3c7b8`](https://github.com/k-talo/volatile-highlights.el/commit/ee3c7b8)).
    - `package-lint` clean, without symbol and namespace warnings with `vhl/` ([`b2d9738`](https://github.com/k-talo/volatile-highlights.el/commit/b2d9738)).
    - Remove XEmacs-specific code which has been effectively unmaintained for years ([`b2d9738`](https://github.com/k-talo/volatile-highlights.el/commit/b2d9738)).
- Refactor(API):
  - Unify extension arg names to `NAME` ([`dc963b7`](https://github.com/k-talo/volatile-highlights.el/commit/dc963b7));
  - Rename `Vhl/highlight-zero-width-ranges` to `vhl/highlight-zero-width-ranges` ([`15b587c`](https://github.com/k-talo/volatile-highlights.el/commit/15b587c)).
  - Move obsolete alias declarations before defcustoms to silence warnings ([`ee3c7b8`](https://github.com/k-talo/volatile-highlights.el/commit/ee3c7b8)).
- Chore
  - ALIGNED version metadata across headers ([`f33f256`](https://github.com/k-talo/volatile-highlights.el/commit/f33f256)).

## 1.18 – 2025-07-22
- Reassigned `vhl/use-pulsing-visual-effect-p` to the `volatile-highlights` customization group ([`6b3066f`](https://github.com/k-talo/volatile-highlights.el/commit/6b3066f)).
- Refreshed README `use-package` snippets to emphasise `customize-set-variable` ([`c0eb319`](https://github.com/k-talo/volatile-highlights.el/commit/c0eb319), [`a3e7192`](https://github.com/k-talo/volatile-highlights.el/commit/a3e7192)).
- Tagged release v1.18 ([`11861d7`](https://github.com/k-talo/volatile-highlights.el/commit/11861d7)).

## 1.17 – 2025-07-20
- Introduced a pulsing visual effect via the `vhl/use-pulsing-visual-effect-p` defcustom ([`6c11eac`](https://github.com/k-talo/volatile-highlights.el/commit/6c11eac)) \[fix [#16](https://github.com/k-talo/volatile-highlights.el/issues/16)].
- Converted project documentation from Org to Markdown ([`09e2fcb`](https://github.com/k-talo/volatile-highlights.el/commit/09e2fcb), [`a3e7192`](https://github.com/k-talo/volatile-highlights.el/commit/a3e7192)).
- Suppressed the Emacs 30.1 warning about `easy-mmode-define-minor-mode` ([`2ca4053`](https://github.com/k-talo/volatile-highlights.el/commit/2ca4053)).
- Stopped eagerly requiring `etags` to reduce startup work ([`fd4dca4`](https://github.com/k-talo/volatile-highlights.el/commit/fd4dca4)).
- Tagged release v1.17 ([`9007fca`](https://github.com/k-talo/volatile-highlights.el/commit/9007fca)).

## 1.16 – 2024-09-14
- Translated every legacy `defadvice` form to `advice-add` ([`07f674f`](https://github.com/k-talo/volatile-highlights.el/commit/07f674f)).
- Adopted lexical binding throughout the file ([`40f0b6d`](https://github.com/k-talo/volatile-highlights.el/commit/40f0b6d)).
- Replaced the deprecated `cl` dependency with `cl-lib` ([`f580177`](https://github.com/k-talo/volatile-highlights.el/commit/f580177)) \[fix [#23](https://github.com/k-talo/volatile-highlights.el/issues/23), [#24](https://github.com/k-talo/volatile-highlights.el/issues/24)].
- Fixed volatile highlights produced by the occur extension ([`d9d1739`](https://github.com/k-talo/volatile-highlights.el/commit/d9d1739)).
- Disabled the occur extension automatically on Emacs 28+ ([`e72d680`](https://github.com/k-talo/volatile-highlights.el/commit/e72d680)) \[fix [#26](https://github.com/k-talo/volatile-highlights.el/issues/26)].
- Clarified the modeline string behaviour in the README ([`c519aa7`](https://github.com/k-talo/volatile-highlights.el/commit/c519aa7)).
- Updated copyright notices to include 2024 ([`18a6df2`](https://github.com/k-talo/volatile-highlights.el/commit/18a6df2)).
- Tagged release v1.16 ([`afccb5c`](https://github.com/k-talo/volatile-highlights.el/commit/afccb5c)).

## 1.15 – 2016-06-12
- Updated documentation/snippets for other packages based on user feedback ([`9a20091`](https://github.com/k-talo/volatile-highlights.el/commit/9a20091)) \[issue [#14](https://github.com/k-talo/volatile-highlights.el/issues/14)].
- Tagged release v1.15 ([`9a20091`](https://github.com/k-talo/volatile-highlights.el/commit/9a20091)).

## 1.14 – 2016-06-12
- Documented `evil-mode` integration patterns ([`cdbff96`](https://github.com/k-talo/volatile-highlights.el/commit/cdbff96)) \[issue [#7](https://github.com/k-talo/volatile-highlights.el/issues/7), [#13](https://github.com/k-talo/volatile-highlights.el/issues/13)].
- Fixed `vhl/install-extension` so extensions load correctly ([`7835e09`](https://github.com/k-talo/volatile-highlights.el/commit/7835e09)) \[fix [#14](https://github.com/k-talo/volatile-highlights.el/issues/14)].
- Tagged release v1.14 ([`b8082e2`](https://github.com/k-talo/volatile-highlights.el/commit/b8082e2)).

## 1.13 – 2016-05-21
- Fixed nested highlight-aware operations such as `yank-pop` ([`bb5e30a`](https://github.com/k-talo/volatile-highlights.el/commit/bb5e30a)) \[fix [#12](https://github.com/k-talo/volatile-highlights.el/issues/12)].
- Tagged release v1.13 ([`bb5e30a`](https://github.com/k-talo/volatile-highlights.el/commit/bb5e30a)).

## 1.12 – 2016-02-21
- Added autoload cookies for the public entry points ([`47f8c80`](https://github.com/k-talo/volatile-highlights.el/commit/47f8c80)).
- Registered the package on MELPA Stable \[issue [#11](https://github.com/k-talo/volatile-highlights.el/issues/11)].
- Tagged release v1.12 ([`e39e560`](https://github.com/k-talo/volatile-highlights.el/commit/e39e560)).

## 1.11 – 2014-10-05
- Fixed the hideshow "Symbol's function definition is void: return" error ([`7d9569f`](https://github.com/k-talo/volatile-highlights.el/commit/7d9569f)) \[fix [#6](https://github.com/k-talo/volatile-highlights.el/issues/6)].
- Added README.org and refreshed documentation content ([`3636108`](https://github.com/k-talo/volatile-highlights.el/commit/3636108), [`f3ea4dc`](https://github.com/k-talo/volatile-highlights.el/commit/f3ea4dc), [`df94679`](https://github.com/k-talo/volatile-highlights.el/commit/df94679)).
- Tagged release v1.11 ([`fb2abc2`](https://github.com/k-talo/volatile-highlights.el/commit/fb2abc2)).

## 1.10 – 2013-03-21
- Made the default face inherit when available ([`d904bde`](https://github.com/k-talo/volatile-highlights.el/commit/d904bde)).
- Silenced private Emacs/XEmacs helper warnings via file-local settings ([`7c1eceb`](https://github.com/k-talo/volatile-highlights.el/commit/7c1eceb)).
- Tagged release v1.10 ([`028e428`](https://github.com/k-talo/volatile-highlights.el/commit/028e428)).

## 1.9 – 2013-03-05
- Fixed shell-environment errors caused by placeholder functions ([`8c0c905`](https://github.com/k-talo/volatile-highlights.el/commit/8c0c905)) \[fix [#3](https://github.com/k-talo/volatile-highlights.el/issues/3)].
- Tagged release v1.9 ([`d2c34ab`](https://github.com/k-talo/volatile-highlights.el/commit/d2c34ab)).

## 1.8 – 2012-02-15
- Added the hideshow extension ([`8c717b2`](https://github.com/k-talo/volatile-highlights.el/commit/8c717b2)).
- Added a “Contributed by:” header ([`b07173b`](https://github.com/k-talo/volatile-highlights.el/commit/b07173b)).
- Tagged release v1.8 ([`64b2930`](https://github.com/k-talo/volatile-highlights.el/commit/64b2930)).

## 1.7 – 2012-02-13
- Ensured required features are loaded before use ([`a1fbf17`](https://github.com/k-talo/volatile-highlights.el/commit/a1fbf17)).
- Tagged release v1.7 ([`5b09333`](https://github.com/k-talo/volatile-highlights.el/commit/5b09333)).

## 1.6 – 2012-02-02
- Removed extensions for non-standard features ([`1551072`](https://github.com/k-talo/volatile-highlights.el/commit/1551072)).
- Resolved duplicate `vhl/.make-list-string` definitions ([`747a4ee`](https://github.com/k-talo/volatile-highlights.el/commit/747a4ee)).
- Tagged release v1.6 ([`26b1299`](https://github.com/k-talo/volatile-highlights.el/commit/26b1299)).

## 1.5 – 2012-01-31
- Introduced a `vhl/highlight-zero-width-ranges` preference ([`4f8cfa5`](https://github.com/k-talo/volatile-highlights.el/commit/4f8cfa5))
- Renamed `vhl/add` to `vhl/add-range`, added `vhl/add-position`  ([`c4ce302`](https://github.com/k-talo/volatile-highlights.el/commit/c4ce302), [`3786e66`](https://github.com/k-talo/volatile-highlights.el/commit/3786e66)).
- Introduced the `vhl/define-extension` macro for simple extensions ([`ad5506a`](https://github.com/k-talo/volatile-highlights.el/commit/ad5506a)).
- Converted the undo/yank extensions to the new macro ([`5a73228`](https://github.com/k-talo/volatile-highlights.el/commit/5a73228)).
- Added an extension highlighting where text was killed from ([`1059c8b`](https://github.com/k-talo/volatile-highlights.el/commit/1059c8b)).
- Added an extension highlighting where text was deleted from ([`fa91a4b`](https://github.com/k-talo/volatile-highlights.el/commit/fa91a4b)).
- Documented the new kill/delete extensions ([`cf8665d`](https://github.com/k-talo/volatile-highlights.el/commit/cf8665d)).
- Tagged release v1.5 ([`21ced00`](https://github.com/k-talo/volatile-highlights.el/commit/21ced00)).

## 1.4 – 2012-01-15
- Suppressed warnings raised by Emacs/XEmacs private helpers ([`6467222`](https://github.com/k-talo/volatile-highlights.el/commit/6467222)).
- Fixed XEmacs-specific regressions ([`93b36c8`](https://github.com/k-talo/volatile-highlights.el/commit/93b36c8)).
- Tagged release v1.4 ([`181a4be`](https://github.com/k-talo/volatile-highlights.el/commit/181a4be)).

## 1.3 – 2010-12-18
- Added an extension for non-incremental search operations ([`316ce11`](https://github.com/k-talo/volatile-highlights.el/commit/316ce11)).
- Fixed occur-mode highlights inside folded lines ([`b42cc0b`](https://github.com/k-talo/volatile-highlights.el/commit/b42cc0b), [`283a66a`](https://github.com/k-talo/volatile-highlights.el/commit/283a66a)).
- Tagged release v1.3 ([`d879de3`](https://github.com/k-talo/volatile-highlights.el/commit/d879de3)).

## 1.2 – 2010-11-30
- Highlighted every match in the occur extension ([`3cf4ec1`](https://github.com/k-talo/volatile-highlights.el/commit/3cf4ec1)).
- Tagged release v1.2 ([`278b0ee`](https://github.com/k-talo/volatile-highlights.el/commit/278b0ee)).

## 1.1 – 2010-11-09
- Fixed the mode toggling logic ([`1fdf188`](https://github.com/k-talo/volatile-highlights.el/commit/1fdf188)).
- Tagged release v1.1 ([`505e0da`](https://github.com/k-talo/volatile-highlights.el/commit/505e0da)).

## 1.0 – 2010-03-14
- Imported `volatile-highlights.el` from `.emacs` helper functions ([`894be89`](https://github.com/k-talo/volatile-highlights.el/commit/894be89)).

## 0.0 – 2001-10-03
- Initial utility helpers written by K-talo inside `.emacs`.
