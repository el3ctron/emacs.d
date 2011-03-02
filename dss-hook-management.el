(defun dss/add-after-save-hook (fname)
  (interactive "aWhich function: ")
  (add-hook 'after-save-hook fname t t))

(defun dss/get-after-save-hook-funcs ()
  (delq nil (mapcar
             (lambda (e) (if (symbolp e) e))
             after-save-hook)))

(defun dss/remove-after-save-hook (fname)
  (interactive (list (ido-completing-read
                      "Which? "
                      (mapcar 'symbol-name (dss/get-after-save-hook-funcs)))))
  (remove-hook 'after-save-hook
               (if (stringp fname)
                   (intern fname)
                 fname)
               t))

(provide 'dss-hook-management)
