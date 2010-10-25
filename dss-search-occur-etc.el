(setq-default case-fold-search t)

(eval-after-load "isearch" '(require 'isearch+))

;; follow mode for output of grep and occur
(require 'fm)
(defun fm-start-off()
  (fm-start)
  (fm-toggle))
(add-hook 'occur-mode-hook 'fm-start-off)
;(add-hook 'ibuffer-mode-hooks 'fm-start-off)
(add-hook 'ibuffer-occur-mode-hooks 'fm-start-off)
(add-hook 'occur-mode-hook (lambda () (linum-mode 1)))
(add-hook 'grep-mode-hook (lambda () (linum-mode 1)))

;;; isearch-forward-at-point
;; from http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
(defvar isearch-initial-string nil)

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-search-occur-etc)
