;;; cheetah-mode.el -- A derivation of nxml-mode for Cheetah .tmpl files

;(define-derived-mode cheetah-mode html-mode "Cheetah"
(define-derived-mode cheetah-mode nxml-mode "Cheetah"
  (make-face 'cheetah-variable-face)
  (font-lock-add-keywords
   nil
   '(
     ("\\(#\\(from\\|else\\|include\\|extends\\|set\\|def\\|import\\|for\\|if\\|end\\)+\\)\\>" 1 font-lock-type-face)
     ("\\(#\\(from\\|for\\|end\\)\\).*\\<\\(for\\|import\\|def\\|if\\|in\\)\\>" 3 font-lock-type-face)
     ("\\(##.*\\)\n" 1 font-lock-comment-face)
     ("\\(\\$\\(?:\\sw\\|}\\|{\\|\\s_\\)+\\)" 1 font-lock-variable-name-face))
   )
  (font-lock-mode 1)
  )
;(mapc (lambda (list)
;        (mapc (lambda (pair)
;                (if (or (eq (cdr pair) 'html-mode)
;                       (eq (cdr pair) 'cheetah-mode)
;                        (eq (cdr pair) 'php-mode))
;                    (setcdr pair (lambda ()
;                                   (require 'nxhtml-mode "/site-lisp/nxhtml/autostart")
;                                   (nxhtml-mumamo-mode)))))
;              list))
;      (list auto-mode-alist magic-mode-alist))
(provide 'cheetah-mode)
