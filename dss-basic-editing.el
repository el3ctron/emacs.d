(defun dss/cap-sentence ()
  (interactive)
  (save-excursion
    (backward-sentence)
    (capitalize-word 1)))

(defun dss/quote-region (start end &optional c)
  (interactive "r")
  (let ((c (or c "\"")))
  (save-excursion
    (goto-char start)
    (insert c)
    (goto-char (+ 1 end))
    (insert c))))

(defun dss/single-quote-region (start end)
  (interactive "r")
  (dss/quote-region start end "'"))

(require 'misc)      ; forward-to-word & backward-to-word

(require 'visible-mark)
(global-visible-mark-mode t)

(require 'goto-last-change)             ;there is another version of this library called goto-chg.el

;; see
;; http://nicolas-lara.blogspot.com/2009/11/emacs-mark-stack.html
;; http://www.bloomington.in.us/~brutt/marker-visit.el

;;; http://emacs-fu.blogspot.com/2009/05/tracking-changes.html
(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode t)
;; (global-set-key (kbd "<f6>")      'highlight-changes-visible-mode) ;; changes
;; ;; remove the change-highlight in region
;; (global-set-key (kbd "S-<f6>")    'highlight-changes-remove-highlight)
;; (global-set-key (kbd "<M-prior>") 'highlight-changes-next-change)
;; (global-set-key (kbd "<M-next>")  'highlight-changes-previous-change)



(provide 'dss-basic-editing)
