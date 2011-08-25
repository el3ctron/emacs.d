(require 'skeleton)
(require 'diminish)
(require 'paredit)
(require 'eldoc)
(diminish 'eldoc-mode "")
(diminish 'paredit-mode "PE")

(eval-when-compile
  (require 'cl))
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
     (:foreground "#8a8a8a")))
  "Face used to dim parentheses."
  :group 'dss-faces)

(defface dss-end-paren-face
  '((((class color))
     (:foreground "#8a8a8a")))         ;#9e9e9e
  "Face used to dim parentheses."
  :group 'dss-faces)

;; this form also from emacs starter kit
(dolist (x '(scheme emacs-lisp lisp slime-repl lisp-interaction clojure))
  (font-lock-add-keywords
   (intern (concat (symbol-name x) "-mode"))
   '(("(" . 'dss-paren-face)      ;("(\\|)" . 'dss-paren-face)
     (")" . 'dss-end-paren-face)
     ))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'dss/lisp-modes-init))

(setq dss-greyscale
      ["#080808"
       "#121212"
       "#1c1c1c"
       "#262626"
       "#303030"
       "#3a3a3a"
       "#444444"
       "#4e4e4e"
       "#585858"
       "#626262"
       "#6c6c6c"
       "#767676"
       "#808080"
       "#8a8a8a"
       "#949494"
       "#9e9e9e"
       "#a8a8a8"
       "#b2b2b2"
       "#bcbcbc"
       "#c6c6c6"
       "#d0d0d0"
       "#dadada"
       "#e4e4e4"
       "#eeeeee"])

(defun dss/greyscale-pos (colour)
  (interactive)
  (or
   (position colour dss-greyscale :test 'string=)
   (1- (length dss-greyscale))))

(defun dss/greyscale-incr (colour &optional i)
  (interactive)
  (let* ((i (or i 1))
         (min-or-max (if (> i 0) 'min 'max))
         (arg2 (if (> i 0) (1- (length dss-greyscale)) 0)))
    (elt dss-greyscale
         (funcall min-or-max (+ (dss/greyscale-pos colour) i) arg2))))

(defun dss/greyscale-decr (colour &optional i)
  (interactive)
  (dss/greyscale-incr colour (or i -1)))

;; (dss/greyscale-incr (elt dss-greyscale 0) 25)
;; (dss/greyscale-incr (elt dss-greyscale (1- (length dss-greyscale))) 4)

                                        ;rainbow-delimiters-depth-1-face

;; (loop for i from 1 to 9
;;       do (message (symbol-name (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face")))))
(require 'rainbow-delimiters)
(setq rainbow-delimiters-max-face-count 8)

(defun dss/lisp-setup-rainbow-delimeters ()
  (let ((dss-rainbow-delim-colors
         [
          "red"
          "brightblue"
          "yellow"
          "purple"
          "cyan"
          "#626262"
          "#6c6c6c"
          "#767676"
          "#808080"
          "#8a8a8a"
          "#949494"
          "#9e9e9e"
          "#a8a8a8"
          "#b2b2b2"
          "#bcbcbc"
          "#c6c6c6"
          "#d0d0d0"
          "#dadada"
          "#e4e4e4"
          "#eeeeee"]))
    (loop for i from 1 to rainbow-delimiters-max-face-count
          do (set-face-foreground
              (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))
              (elt dss-rainbow-delim-colors (1- i)))))
  )

(dss/lisp-setup-rainbow-delimeters)

(defun dss/lisp-rainbow-flash ()
  (interactive)
  (let* ((depth 1)
         (beg (save-excursion
                (cond
                 ((looking-at "\\s\(") (point))
                 (t
                  (progn
                    (call-interactively 'dss/goto-match-paren)
                    (point))))))
         (end (save-excursion
                (cond
                 ((looking-at "\\s\(") (forward-list 1))
                 (t
                  (call-interactively 'dss/goto-match-paren)
                  (forward-list 1)))
                (point))))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (re-search-forward rainbow-delimiters-delim-regex end t))
        (backward-char) ; re-search-forward places point after delim; go back.
        (unless (rainbow-delimiters-char-ineligible-p (point))
          (let ((delim (char-after (point))))
            (cond ((eq ?\( delim)       ; (
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "paren" depth (point)))
                  ((eq ?\) delim)       ; )
                   (rainbow-delimiters-apply-color "paren" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched paren
                                   (1- depth))))
                  ((eq ?\[ delim)       ; [
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "bracket" depth (point)))
                  ((eq ?\] delim)       ; ]
                   (rainbow-delimiters-apply-color "bracket" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched bracket
                                   (1- depth))))
                  ((eq ?\{ delim)       ; {
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "brace" depth (point)))
                  ((eq ?\} delim)       ; }
                   (rainbow-delimiters-apply-color "brace" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched brace
                                   (1- depth)))))))
        ;; move past delimiter so re-search-forward doesn't pick it up again
        (forward-char)))
    (save-excursion (font-lock-fontify-region beg end))
    (sit-for 3)
    (save-excursion
      (rainbow-delimiters-unpropertize-region beg end)
      (font-lock-fontify-region beg end))))

(defun dss/lisp-set-parens-color (i)
  (interactive "n1-23: ")
  (dolist (face '(dss-paren-face dss-end-paren-face))
    (set-face-foreground face (elt dss-greyscale i))))

(defun dss/lisp-brighten-parens ()
  (interactive)
  (dolist (face '(dss-paren-face dss-end-paren-face))
    (set-face-foreground face (dss/greyscale-incr (face-foreground face) 2))))

(defun dss/lisp-dim-parens ()
  (interactive)
  (dolist (face '(dss-paren-face dss-end-paren-face))
    (set-face-foreground face (dss/greyscale-decr (face-foreground face) -2))))

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
           (if (looking-at-p "\(")
               (save-excursion (insert " ")))))
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

(define-skeleton dss/elisp-let-skeleton
  "A simple e-lisp let skeleton"
  nil
  "(let ((" @ - "))" \n >
  @ _ ")")
(setq dss-let-skeleton-func 'dss/elisp-let-skeleton)

(defun dss/paredit-l-or-sexp-wrap-let ()
  (interactive)
  (if (and (not (dss/in-string-p))
           (looking-at-p "\("))
      (progn
        (if (not mark-active)
            (mark-sexp))
        (funcall dss-let-skeleton-func))
    (self-insert-command 1)))
(define-key paredit-mode-map "l" 'dss/paredit-l-or-sexp-wrap-let)

(define-skeleton dss/elisp-defun-skeleton
  "A simple e-lisp defun skeleton"
  nil
  "(defun dss/" @ - " (" @ ")" \n >
  "(interactive" @ ")" \n >
  @ _
    ")")
(setq dss-defun-skeleton-func 'dss/elisp-defun-skeleton)

(defun dss/in-slime-repl-p ()
  (equal mode-name "REPL"))

(defun dss/paredit-d-or-defun ()
  (interactive)
  (if (and (not (dss/in-string-p))
           (or (and (dss/in-slime-repl-p)
                    (slime-repl-at-prompt-start-p))
               (looking-at-p "^")))
      (progn (if (and (not (looking-at "\(def"))
                      (looking-at "\("))
                 (if (not mark-active)
                     (mark-sexp)))
             (funcall dss-defun-skeleton-func)
             (dss/indent-defun))
    (self-insert-command 1)))
(define-key paredit-mode-map "d" 'dss/paredit-d-or-defun)

(defun dss/paredit-9-or-paren ()
  (interactive)
  (if (and (not (dss/in-string-p))
           (or (looking-at-p "^")
               (looking-at-p "\(")
               (and (dss/in-slime-repl-p)
                    (slime-repl-at-prompt-start-p))))
      (dss/paredit-open-parenthesis)
    (self-insert-command 1)))
(define-key paredit-mode-map "9" 'dss/paredit-9-or-paren)

(define-skeleton dss-elisp-progn-skeleton
  "A simple e-lisp progn skeleton"
  nil
  "(progn" @ \n >
  @ _ - ")"
    (dss/indent-defun))

(defun dss/paredit-p-or-progn ()
  (interactive)
  (if (and (not (dss/in-string-p))
           (looking-at-p "\("))
      (progn
        (if (not mark-active)
            (mark-sexp))
        (dss-elisp-progn-skeleton))
    (self-insert-command 1)))
(define-key paredit-mode-map "p" 'dss/paredit-p-or-progn)

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
    (call-interactively 'yank))
  (condition-case nil (dss/indent-defun)))

(define-key paredit-mode-map (kbd "C-y") 'dss/paredit-yank)

(define-key paredit-mode-map (kbd "C-M-s") 'paredit-backward-up)

(define-key paredit-mode-map (kbd "C-M-k") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-j") 'paredit-backward-slurp-sexp)

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
