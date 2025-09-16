;;; test-volatile-highlights.el --- ERT tests for volatile-highlights -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Minimal, fast ERT tests for volatile-highlights public API.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'volatile-highlights)

(defun vhl/test--count-vhl-overlays (beg end)
  "Return count of overlays with property \='volatile-highlights between
BEG and END."
  (cl-count-if (lambda (ov) (overlay-get ov 'volatile-highlights))
               (overlays-in beg end)))

(defun vhl/test--vhl-overlays (beg end)
  "Return list of overlays with property \='volatile-highlights between
BEG and END."
  (cl-remove-if-not (lambda (ov) (overlay-get ov 'volatile-highlights))
                    (overlays-in beg end)))

(defmacro vhl/test--silence (&rest body)
  "Evaluate BODY with messages suppressed."
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(ert-deftest vhl/test-add-range-creates-overlay ()
  "vhl/add-range creates a volatile highlight overlay in current buffer."
  (with-temp-buffer
    (insert "abcdef")
    (let ((volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (vhl/add-range 2 5)
        (volatile-highlights-mode -1)))
    (let* ((ovs (vhl/test--vhl-overlays (point-min) (point-max)))
           (ov (car ovs)))
      (should (= (length ovs) 1))
      (should (= (overlay-start ov) 2))
      (should (= (overlay-end ov) 5))
      (should (string= (buffer-substring-no-properties (overlay-start ov)
                                                       (overlay-end ov))
                       "bcd")))
    (vhl/clear-all)
    (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0))))

(ert-deftest vhl/test-clear-all-removes-overlays ()
  "vhl/clear-all removes all volatile highlight overlays."
  (with-temp-buffer
    (insert "12345")
    (let ((volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (progn
            (vhl/add-range 1 3)
            (vhl/add-range 3 5))
        (volatile-highlights-mode -1)))
    (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 2))
    (vhl/clear-all)
    (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0))))

(ert-deftest vhl/test-add-position-respects-zero-width-flag ()
  "vhl/add-position only highlights when vhl/highlight-zero-width-ranges is
non-nil."
  (with-temp-buffer
    (insert "xyz")
    (let ((vhl/highlight-zero-width-ranges nil)
          (volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (vhl/add-position 2)
        (volatile-highlights-mode -1))
      (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0)))
    (vhl/clear-all)
    (let ((vhl/highlight-zero-width-ranges t)
          (volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (vhl/add-position 2)
        (volatile-highlights-mode -1))
      (let* ((ovs (vhl/test--vhl-overlays (point-min) (point-max)))
             (ov (car ovs)))
        (should (= (length ovs) 1))
        (should (= (overlay-start ov) 2))
        (should (= (overlay-end ov) 3))
        (should (string= (buffer-substring-no-properties (overlay-start ov)
                                                         (overlay-end ov))
                         "y"))))
    (vhl/clear-all)))

(ert-deftest vhl/test-yank-creates-and-clears ()
  "yank creates a highlight and next command clears it."
  (with-temp-buffer
    (let ((volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (progn
            (insert "foo ")
            (setq kill-ring '("BAR"))
            (goto-char (point-max))
            (vhl/test--silence (yank))
            (let* ((ovs (vhl/test--vhl-overlays (point-min) (point-max)))
                   (ov (car ovs)))
              (should (= (length ovs) 1))
              (should (string= (buffer-substring-no-properties (overlay-start ov)
                                                               (overlay-end ov))
                               "BAR")))
            ;; Simulate next command
            (run-hooks 'pre-command-hook)
            (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0)))
        (volatile-highlights-mode -1)))))

(ert-deftest vhl/test-replace-string-highlights ()
  "replace-string highlights replacements made by \='perform-replace."
  (with-temp-buffer
    (let ((volatile-highlights-mode nil))
      (insert "foo baz foo")
      (goto-char (point-min))
      (volatile-highlights-mode 1)
      (unwind-protect
          (progn
            (vhl/test--silence
             (replace-string "foo" "bar"))
            (let* ((ovs (vhl/test--vhl-overlays (point-min) (point-max)))
                   (texts (mapcar (lambda (ov)
                                    (buffer-substring-no-properties
                                     (overlay-start ov)
                                     (overlay-end ov)))
                                  ovs)))
              (should (= (length ovs) 2))
              (should (cl-every (lambda (text) (string= text "bar")) texts)))
            ;; Simulate next command to clear highlights
            (run-hooks 'pre-command-hook)
            (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0)))
        (volatile-highlights-mode -1)))))

(ert-deftest vhl/test-undo-highlights ()
  "undo highlights affected text."
  (with-temp-buffer
    (let ((volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (progn
            (buffer-enable-undo)
            (undo-boundary)
            (insert "abcdef")
            (undo-boundary)
            ;; Delete some text, then undo to trigger highlighting
            (delete-region 3 6)
            (undo-boundary)
            (vhl/test--silence (undo))
            (let* ((ovs (vhl/test--vhl-overlays (point-min) (point-max)))
                   (ov (car ovs)))
              (should (= (length ovs) 1))
              (should (= (overlay-start ov) 3))
              (should (= (overlay-end ov) 6))
              (should (string= (buffer-substring-no-properties (overlay-start ov)
                                                               (overlay-end ov))
                               "cde"))))
        (volatile-highlights-mode -1)))))

(ert-deftest vhl/test-add-position-clamps-to-buffer-size ()
  "vhl/add-position clamps when POS is beyond buffer size."
  (with-temp-buffer
    (insert "hi")
    (let ((vhl/highlight-zero-width-ranges t)
          (volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (vhl/add-position 10)
        (volatile-highlights-mode -1))
      (let* ((ovs (vhl/test--vhl-overlays (point-min) (point-max)))
             (ov (car ovs))
             (lastc (buffer-substring-no-properties (1- (point-max)) (point-max))))
        (should (= (length ovs) 1))
        (should (<= (overlay-start ov) (point-max)))
        (should (<= (overlay-end ov) (point-max)))
        (let ((len (- (overlay-end ov) (overlay-start ov))))
          (should (member len '(0 1)))
          (when (= len 1)
            (should (string= (buffer-substring-no-properties (overlay-start ov)
                                                             (overlay-end ov))
                             lastc))
            ;; If zero-length, it should be at point-max (clamped)
            (when (= len 0)
              (should (= (overlay-start ov) (point-max)))
              (should (= (overlay-end ov) (point-max))))))))
    (vhl/clear-all)))

(ert-deftest vhl/test-add-range-noop-when-mode-disabled ()
  "vhl/add-range is a no-op when volatile-highlights-mode is disabled."
  (with-temp-buffer
    (insert "abcdef")
    (let ((volatile-highlights-mode nil))
      (vhl/add-range 2 5)
      (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0)))))

(ert-deftest vhl/test-add-position-noop-when-mode-disabled ()
  "vhl/add-position is a no-op when volatile-highlights-mode is disabled."
  (with-temp-buffer
    (insert "abc")
    (let ((volatile-highlights-mode nil)
          (vhl/highlight-zero-width-ranges t))
      (vhl/add-position 2)
      (should (= (vhl/test--count-vhl-overlays (point-min) (point-max)) 0)))))

(ert-deftest vhl/test-xref-after-jump-highlights-symbol ()
  "xref after-jump hook highlights the symbol at point."
  (with-temp-buffer
    (let ((volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (progn
            (insert "foo bar")
            (goto-char 2) ;; on "o" in "foo"
            (vhl/ext/xref/.after-jump)
            (let* ((ovs (vhl/test--vhl-overlays (point-min) (point-max)))
                   (ov (car ovs)))
              (should (= (length ovs) 1))
              (should (string= (buffer-substring-no-properties (overlay-start ov)
                                                               (overlay-end ov))
                               "foo"))))
        (volatile-highlights-mode -1)))))

(ert-deftest vhl/test-xref-after-jump-line-fallback ()
  "xref after-jump hook falls back to line highlight when no symbol."
  (with-temp-buffer
    (let ((volatile-highlights-mode nil))
      (volatile-highlights-mode 1)
      (unwind-protect
          (progn
            (insert "   \nnext")
            (goto-char 2) ;; whitespace line
            (vhl/ext/xref/.after-jump)
            (let* ((ovs (vhl/test--vhl-overlays (point-min) (point-max)))
                   (ov (car ovs)))
              (should (= (length ovs) 1))
              (should (= (overlay-start ov) (line-beginning-position)))
              (should (= (overlay-end ov) (line-end-position)))))
        (volatile-highlights-mode -1)))))

(provide 'test-volatile-highlights)

;;; test-volatile-highlights.el ends here
