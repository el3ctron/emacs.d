(require 'diminish)
(require 'paredit)
(require 'eldoc)
(diminish 'eldoc-mode "")
(diminish 'paredit-mode "PE")


(require 'dss-codenav-helpers)

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(defvar dss-lisp-modes-hook nil)

(defun dss/lisp-modes-init ()
  (linum-mode t)
  (paredit-mode +1)
  (dss/highlight-watchwords)
  (run-hooks 'dss-lisp-modes-hook))

;;  idea from esk-paren-face in emacs starter Kit
(defface dss-paren-face
   '((((class color))
      ;; 9e9e9e or a8a8a8 are also good
      (:foreground "#b2b2b2")))
   "Face used to dim parentheses."
   :group 'dss-faces)

(defface dss-end-paren-face
  '((((class color))
     (:foreground "#8e8e8e")))         ;#9e9e9e
  "Face used to dim parentheses."
  :group 'dss-faces)

;; this form also from emacs starter kit
(dolist (x '(scheme emacs-lisp lisp lisp-interaction clojure))
  (font-lock-add-keywords
     (intern (concat (symbol-name x) "-mode"))
     '(("(" . 'dss-paren-face)      ;("(\\|)" . 'dss-paren-face)
       (")" . 'dss-end-paren-face)
       ))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'dss/lisp-modes-init))


(defun dss/goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up.

  Copied from: http://www.emacswiki.org/emacs/ParenthesisMatching"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))))))))


(defun dss/paredit-backward-delete ()
  (interactive)
  (if mark-active
      (call-interactively 'delete-region)
    (paredit-backward-delete)))

(define-key paredit-mode-map (kbd "DEL") 'dss/paredit-backward-delete)

(defun dss/paredit-open-parenthesis ()
  (interactive)
  (cond ((and (not (or mark-active (dss/in-string-p)))
              (looking-at-p "[\(a-z\"]"))
         (progn
           (mark-sexp)
           (paredit-open-parenthesis)
           (save-excursion (insert " "))))
        (t (paredit-open-parenthesis))))
(define-key paredit-mode-map "(" 'dss/paredit-open-parenthesis)

(defun dss/paredit-semicolon ()
  (interactive)
  (if (looking-at-p " +\(")
      (progn
        (search-forward "(")
        (backward-char)))
  (cond ((and (not mark-active) (looking-at-p "\("))
         (progn
           (mark-sexp)
           (paredit-comment-dwim)
           (save-excursion (reindent-then-newline-and-indent))
           (indent-according-to-mode)))
        ((and (not mark-active)
              (looking-at-p "^[[:blank:]]*$"))
         (insert ";;; "))
        ((and (not mark-active)
              (and (looking-back "^[[:blank:]]*")
                   (looking-at-p "[[:blank:]]*$")))
         (insert ";; "))
        (t (paredit-semicolon))))
(define-key paredit-mode-map ";" 'dss/paredit-semicolon)

(defun dss/paredit-open-line ()
  (interactive)
  (save-excursion
    (reindent-then-newline-and-indent))
  (indent-according-to-mode))
(define-key paredit-mode-map (kbd "M-o") 'dss/paredit-open-line)

(defun dss/replace-sexp ()
  (interactive)
  (if (dss/in-string-p)
      (dss/mark-string)
    (mark-sexp))
  (delete-region (point) (mark))
  (yank))
(define-key paredit-mode-map (kbd "C-M-y") 'dss/replace-sexp)

(defun dss/paredit-kill-ring-save ()
  (interactive)
  (if (not mark-active)
      (save-excursion
        (if (looking-at-p " +\(")
            (progn
              (search-forward "(")
              (backward-char)))
        (mark-sexp)
        (call-interactively 'kill-ring-save))
    (call-interactively 'kill-ring-save)))

(define-key paredit-mode-map (kbd "M-w") 'dss/paredit-kill-ring-save)

(defun dss/paredit-yank ()
  (interactive)
  (if (not mark-active)
      (progn
        (call-interactively 'yank)
        (if (and (looking-back "\)")
                 (looking-at-p "\("))
            (progn
              (reindent-then-newline-and-indent)
              (if (looking-at-p "^")
                  (newline)))))
    (call-interactively 'yank)))

(define-key paredit-mode-map (kbd "C-y") 'dss/paredit-yank)

(define-key paredit-mode-map (kbd "C-M-s") 'paredit-backward-up)

(define-key paredit-mode-map (kbd "C-M-k") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-j") 'paredit-backward-barf-sexp)

;; (define-key paredit-mode-map (kbd "M-b") 'paredit-for)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elisp
(defun dss/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid.
  Comes from the emacs starter kit"
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(defun dss/elisp-mode-hook ()
  (dss/remove-elc-on-save)
  (eldoc-mode 1)
  (setq mode-name "EL:")
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-complete-lisp-symbol
          try-complete-lisp-symbol-partially
          try-expand-dabbrev)))
(add-hook 'emacs-lisp-mode-hook 'dss/elisp-mode-hook)

(defun dss/find-function-at-point ()
  "Find directly the function at point in the current window."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb))))

(define-key emacs-lisp-mode-map (kbd "M-.") 'dss/find-function-at-point)

;; ;; http://www.emacswiki.org/emacs/FlymakeElisp
;; (defun flymake-elisp-init ()
;;   (if (string-match "^ " (buffer-name))
;;       nil
;;     (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                          'flymake-create-temp-inplace))
;;            (local-file  (file-relative-name
;;                          temp-file
;;                          (file-name-directory buffer-file-name))))
;;       (list
;;        (expand-file-name invocation-name invocation-directory)
;;        (list
;;         "-Q" "--batch" "--eval"
;;         (prin1-to-string
;;          (quote
;;           (dolist (file command-line-args-left)
;;             (with-temp-buffer
;;               (insert-file-contents file)
;;               (condition-case data
;;                   (scan-sexps (point-min) (point-max))
;;                 (scan-error
;;                  (goto-char (nth 2 data))
;;                  (princ (format "%setq:%setq: error: Unmatched bracket or quote\n"
;;                                 file (line-number-at-pos)))))))))
;;         local-file)))))
;; (push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)
;; (add-hook 'emacs-lisp-mode-hook 'flymake-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; real lisp hackers use the lambda character
;; courtesy of stefan monnier on c.l.l
(defun sm-lambda-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun dss/pretty-lambda-enable ()
  (interactive)
  (add-hook 'dss-lisp-modes-hook 'sm-lambda-mode-hook)
  (add-hook 'python-mode-hook 'sm-lambda-mode-hook))

(defun dss/pretty-lambda-disable ()
  (interactive)
  (remove-hook 'dss-lisp-modes-hook 'sm-lambda-mode-hook)
  (remove-hook 'python-mode-hook 'sm-lambda-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-lisps)
