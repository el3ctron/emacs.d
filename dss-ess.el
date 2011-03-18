(require 'dss-elisp-funcs)
;;; Installed via gentoo / portage
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")

(if (file-exists-p "/usr/share/emacs/etc/ess")
    (condition-case nil
        (require 'ess-site)
      (message "Error while loading ess")))

(defun dss/ess-end-of-buffer ()
  (with-current-buffer "*R*"
    (end-of-buffer)
    (dss/sync-point-all-windows)))

(defun dss/ess-load-file ()
  (interactive)
  (call-interactively 'ess-eval-buffer)
  (dss/ess-end-of-buffer)
  (run-with-timer 1 nil 'dss/ess-end-of-buffer))

(defun dss/ess-electric-pair ()
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun dss/ess-mode-hook ()
  (interactive)
  (dss/map-define-key ess-mode-map '("\"" "(" "[" "{") 'dss/ess-electric-pair)
  (define-key ess-mode-map (kbd "C-c C-c") #'dss/ess-load-file))

(add-hook 'ess-mode-hook 'dss/ess-mode-hook)

(require 'ess-inf)
(defun dss/inferior-ess-mode-hook ()
  (interactive)
  (dss/map-define-key inferior-ess-mode-map '("\"" "(" "[" "{") 'dss/ess-electric-pair))
(add-hook 'inferior-ess-mode-hook 'dss/inferior-ess-mode-hook)

(define-key inferior-ess-mode-map (kbd "C-d")
  (lambda ()
    (interactive)
    (ess-quit-r)))

(provide 'dss-ess)
