(defun dss/file-to-string (file)
  "There must be a built-in that does this... why can't I find it?"
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(provide 'dss-elisp-funcs)
