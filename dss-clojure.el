;;; setting up clojure/slime http://technomancy.us/126
(require 'dss-slime)
(require 'durendal)

(defmacro dss/defface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(setq dss-clojure-punctuation nil)
(dss/defface dss-clojure-punctuation "#afaf5f" "Dss-Highlight punctuation") ; #b2b2b2
(dss/defface dss-clojure-braces "#49b2c7" "Dss-Highlight braces")
(dss/defface dss-clojure-brackets "#49b2c7" "Dss-Highlight brackets")
(dss/defface dss-clojure-keyword "khaki" "Dss-Highlight keywords")
(dss/defface dss-clojure-namespace "#c476f1" "Dss-Highlight namespace")
(dss/defface dss-clojure-quote "white" "Dss-Highlight quote" (:background "#333333"))
(dss/defface dss-clojure-backtick "brightwhite" "Dss-Highlight backtick"
             (:background "#5f0000"))
(dss/defface dss-clojure-java-call "#5f87ff" "Dss-Highlight Java calls") ;#4bcf68
(dss/defface dss-clojure-special "#b8bb00" "Dss-Highlight special")
(dss/defface dss-clojure-strf "bright green" "Dss-Highlight strf" (:background "#333333"))
(dss/defface dss-clojure-number "#b8bb00" "Dss-Highlight number")
(dss/defface dss-clojure-double-quote "green" "Dss-Highlight special"
             (:background "unspecified"))

(defun dss/clojure-add-extra-fontlock ()
  (interactive)
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '(
            (("\\(|\\|=\\|,\\|&\\|~\\|@\\|#\\|\\\\\\|+\\|_\\|:\\)"
              . 'dss-clojure-punctuation))
            (("#?\\^?{\\|}" . 'dss-clojure-braces))
            (("\\[\\|\\]" . 'dss-clojure-brackets))
            (("'" . 'dss-clojure-quote))
            (("`\\|@\\|~" . 'dss-clojure-backtick))
            (("[^\\w+]\\(:\\w+\\)" 1 'dss-clojure-keyword))
            (("#\(" . 'dss-clojure-special))
            (("%[0-9.]*\\(([a-zA-Z0-9]*)\\)?[fdsfr]*" 0 'dss-clojure-strf prepend))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)"
              1 'dss-clojure-java-call))
            ))
  (font-lock-fontify-buffer))

(defun dss/init-clojure ()
  (interactive)
  ;; (dss/init-elpa)
  (clojure-slime-config))

(defun dss/slime-repl-after-pmark-p ()
  (>= (point) slime-repl-input-start-mark))

(defmacro dss/def-alternate-key (name pred match no-match)
  `(defun ,name ()
     (interactive)
     (if (,pred)
         (call-interactively ,match)
       (call-interactively ,no-match))))

(dss/def-alternate-key dss/slime-repl-next-input
                       dss/slime-repl-after-pmark-p
                       'slime-repl-next-input 'next-line)

(dss/def-alternate-key dss/slime-repl-previous-input
                       dss/slime-repl-after-pmark-p
                       'slime-repl-previous-input 'previous-line)

(dss/def-alternate-key dss/slime-repl-previous-matching-input
                       dss/slime-repl-after-pmark-p
                       'slime-repl-previous-matching-input 'scroll-down-command)

(dss/def-alternate-key dss/slime-repl-next-matching-input
                       dss/slime-repl-after-pmark-p
                       'slime-repl-next-matching-input 'scroll-up-command)

(define-skeleton dss/clojure-use-skeleton
  "use skeleton"
  nil
  "(use '" @ - _ ")")

(dss/def-alternate-key dss/slime-repl-use
                       slime-repl-at-prompt-start-p
                       'dss/clojure-use-skeleton 'self-insert-command)



(define-skeleton dss/clojure-defun-skeleton
  "clojure defun skeleton"
  nil
  "(defn " @ - " [" @ "]" \n >
  @ _
    ")")

(define-skeleton dss/clojure-let-skeleton
  "let skeleton"
  nil
  "(let [" @ - "]" \n >
  @ _ ")")

(defun dss/clojure-setup-skeletons ()
  (interactive)
  (make-local-variable 'dss-defun-skeleton-func)
  (setq dss-defun-skeleton-func 'dss/clojure-defun-skeleton)
  (setq dss-let-skeleton-func 'dss/clojure-let-skeleton))

(defun dss/clojure-mode-hook ()
  (interactive)
  (dss/clojure-add-extra-fontlock)
  (dss/clojure-setup-skeletons))

(add-hook 'clojure-mode-hook 'dss/clojure-mode-hook)

(defun dss/slime-repl-mode-setup-map (&optional mode-map)
  (interactive)
  (let ((mode-map (or mode-map slime-repl-mode-map)))
    (define-key mode-map (kbd "<down>") 'dss/slime-repl-next-input)
    (define-key mode-map (kbd "<up>") 'dss/slime-repl-previous-input)
    (define-key mode-map (kbd "<prior>") 'dss/slime-repl-previous-matching-input)
    (define-key mode-map (kbd "<next>") 'dss/slime-repl-next-matching-input)

    ;; (define-key mode-map (kbd "C-M-r") 'comint-history-isearch-backward)
    ;; (define-key mode-map (kbd "C-M-s") 'comint-history-isearch-search)

    (define-key mode-map (kbd "M-p") 'previous-line)
    (define-key mode-map (kbd "M-n") 'next-line)
    (define-key mode-map (kbd "C-M-l") 'end-of-buffer)
    (define-key mode-map "{" 'paredit-open-curly)
    (define-key mode-map "}" 'paredit-close-curly)
    (define-key mode-map "u" 'dss/slime-repl-use)))

;;; see Clementson's Blog: Clojure SLIME Mods for Java Documentation
;;; http://bc.tech.coop/blog/081120.html

(defun dss/slime-repl-hook ()
  (interactive)

  ;; also see
  ;; https://github.com/technomancy/durendal/blob/master/durendal.el
  ;; and https://github.com/tcrayford/clojure-refactoring
  (paredit-mode 1)
  (dss/load-slime-completion)
  (dss/slime-repl-mode-setup-map)
  (dss/clojure-setup-skeletons)

  (when (< emacs-major-version 24)
    (set (make-local-variable 'forward-sexp-function)
         'clojure-forward-sexp))
  ;; the rest is semi-copied from
  ;; http://stackoverflow.com/questions/2474804/is-there-a-colored-repl-for-clojure
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (font-lock-mode nil)
  (clojure-mode-font-lock-setup)
  (set-syntax-table clojure-mode-syntax-table)
  (font-lock-mode t)
  (ad-activate #'slime-repl-emit)
  (ad-activate #'slime-repl-insert-prompt)
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (dss/clojure-add-extra-fontlock))
(add-hook 'slime-repl-mode-hook 'dss/slime-repl-hook)

(provide 'dss-clojure)
