(setq shell-command-switch "-lc")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; multi-term
(autoload 'multi-term "multi-term")
(setq multi-term-program "/bin/bash")
(require 'multi-term)

(defun dss/cd_term (dir)
  (let (tmp-buffer)
    (set-buffer (setq tmp-buffer (get-buffer-create "*multi-term-launcher*")))
    (setq default-directory dir)
    (multi-term)
    (kill-buffer tmp-buffer)
    ))

(defun dss/term-toggle-mode ()
  "Toggle between term-char-mode and term-line-mode."
  (interactive)
  (if (term-in-line-mode)
      (progn
        (term-char-mode)
        (term-send-raw-string "\C-e"))
    (term-line-mode)))

(defun dss/term-dabbrev ()
  (interactive)
  (let ((beg (point)))
    (dabbrev-expand nil)
    (kill-region beg (point)))
  (term-send-raw-string (substring-no-properties (current-kill 0))))

(defun dss/term-backward-kill-word ()
  (interactive)
  (if (term-in-line-mode)
      (backward-kill-word)
    (term-send-backward-kill-word)))

(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-raw-escape-map "/"
               'dss/term-dabbrev)
             (define-key term-mode-map (kbd "C-c C-j")
               'dss/term-toggle-mode)
             (define-key term-mode-map (kbd "M-DEL")
               'dss/term-backward-kill-word)
             (define-key term-mode-map (kbd "M-g")
               'dss/term-toggle-mode)
             (linum-mode -1)))

(defun dss/term-yank ()
  (interactive)
  (if (term-in-line-mode)
      (yank)
    (term-paste)))

(defun dss/term-reverse-search ()
  (interactive)
  (if (term-in-line-mode)
      (isearch-backward)
    (term-send-reverse-search-history)))

;(defun term-forward-search ()
;  (interactive)
;  (if (term-in-line-mode)
;      (isearch-forward)
;    (term-send-forward-search-history)))

(setq term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-x C-x" . term-send-raw)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-r" . dss/term-reverse-search)
     ("C-m" . term-send-raw)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-N" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-," . term-send-input)
     ("M-." . comint-dynamic-complete)
     ("Od" . term-send-backward-word)
     ("Oc" . term-send-forward-word)
     ("M-d" . term-send-forward-kill-word)
     ("M-g" . dss/term-toggle-mode)
     ("C-y" . dss/term-yank)))

;; also see http://dea.googlecode.com/svn/trunk/my-lisps/multi-term-settings.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-term)
