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

(provide 'dss-basic-editing)
