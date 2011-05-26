(defun dss/file-to-string (file)
  "There must be a built-in that does this... why can't I find it?"
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun dss/map-define-key (mode-map keylist fname)
  "Like define-key but the key arg is a list that should be mapped over"
  (mapc (lambda (k)
          (progn
            (define-key mode-map k fname)))
        keylist))

(provide 'dss-elisp-funcs)
