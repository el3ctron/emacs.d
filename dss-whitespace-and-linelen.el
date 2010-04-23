(load-library "col-highlight") ; for highlighting the current col

(setq require-final-newline t) ; auto-insert final newlines in all files

(defun dss/load-lineker-mode ()
  (interactive)
  (require 'lineker)
  (setq lineker-column-limit 90)
  (lineker-mode))

;; whitespace highlighting and cleanup

(require 'show-wspace)
(defun dss/show-ws ()
  (setq show-trailing-whitespace t)
  (show-ws-highlight-tabs)
  (hl-line-mode 1))

(defun dss/whitespace-cleanup ()
  "Trim all trailing whitespace in the current buffer, and untabify."
  (interactive)
  (save-excursion
    (whitespace-cleanup)
    (untabify (point-min) (point-max))))

(defun dss/install-whitespace-cleanup-hook ()
  "Add this to any major mode hook to show whitespace during editing
   and trip it before saving"
  (dss/show-ws)
  (add-hook 'before-save-hook 'dss/whitespace-cleanup nil t))

(add-hook 'emacs-lisp-mode-hook 'dss/install-whitespace-cleanup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-whitespace-and-linelen)
