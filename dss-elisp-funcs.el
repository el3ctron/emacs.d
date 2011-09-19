(defun dss/local-shell-command-to-string (str)
  "An alternative to shell-command-to-string that is always local
  so tramp doesn't get in the way"
  (interactive)
  (with-temp-buffer
    (call-process-shell-command str nil t)
    (buffer-string)))

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


(defun dss/call-command-with-input (command input-str out-buffer &rest args)
  "A convenient wrapper around `call-process-region` that accepts a
  string input instead of a buffer region"
  (let ((args (or args "")))
    (with-temp-buffer
      (insert input-str)
      (call-process-region
       (point-min) (point-max) command nil out-buffer nil args))))

(defun dss/async-call-command (command input &rest args)
  "An asynchronous version of `dss/call-command-with-input`"
  (apply 'dss/call-command-with-input (append (list command input 0) args)))

(provide 'dss-elisp-funcs)
