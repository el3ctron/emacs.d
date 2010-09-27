(require 'paredit)
(require 'eldoc)
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
      (:foreground "#9e9e9e")))
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

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (dss/remove-elc-on-save)
   (eldoc-mode)
   (setq hippie-expand-try-functions-list
    '(try-expand-dabbrev-visible
      try-complete-lisp-symbol
      try-complete-lisp-symbol-partially
      try-expand-dabbrev))))

(defun dss/find-function-at-point ()
  "Find directly the function at point in the current window."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb))))

(define-key emacs-lisp-mode-map (kbd "M-.") 'dss/find-function-at-point)

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
