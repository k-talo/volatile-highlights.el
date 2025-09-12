;;; test-volatile-highlights.el --- ERT tests for volatile-highlights -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Minimal, fast ERT tests for volatile-highlights public API.

;;; Code:

(require 'ert)
(require 'volatile-highlights)

(defun vhl/test--count-vhl-overlays (beg end)
  "Return count of overlays with property \='volatile-highlights between BEG and END."
  (cl-count-if (lambda (ov) (overlay-get ov 'volatile-highlights))
               (overlays-in beg end)))

(ert-deftest vhl/test-add-range-creates-overlay ()
  "vhl/add-range creates a volatile highlight overlay in current buffer."
  (with-temp-buffer
    (insert "abcdef")
    (vhl/add-range 2 5)
    (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 1))
    (vhl/clear-all)
    (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0))))

(ert-deftest vhl/test-clear-all-removes-overlays ()
  "vhl/clear-all removes all volatile highlight overlays."
  (with-temp-buffer
    (insert "12345")
    (vhl/add-range 1 3)
    (vhl/add-range 3 5)
    (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 2))
    (vhl/clear-all)
    (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0))))

(ert-deftest vhl/test-add-position-respects-zero-width-flag ()
  "vhl/add-position only highlights when Vhl/highlight-zero-width-ranges is non-nil."
  (with-temp-buffer
    (insert "xyz")
    (let ((Vhl/highlight-zero-width-ranges nil))
      (vhl/add-position 2)
      (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0)))
    (vhl/clear-all)
    (let ((Vhl/highlight-zero-width-ranges t))
      (vhl/add-position 2)
      (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 1)))
    (vhl/clear-all)))

(provide 'test-volatile-highlights)

;;; test-volatile-highlights.el ends here

