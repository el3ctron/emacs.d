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
(add-hook 'emacs-lisp-mode-hook 'dss/remove-elc-on-save)


(require 'paredit)
(defun dss/paredit-backward-delete ()
  (interactive)
  (if mark-active
      (call-interactively 'delete-region)
    (paredit-backward-delete)))
(define-key paredit-mode-map (kbd "DEL") 'dss/paredit-backward-delete)
(require 'paredit)
;; study http://www.emacswiki.org/emacs/ParEdit
;; http://www.emacswiki.org/emacs/PareditCheatsheet

(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))


(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(add-hook 'emacs-lisp-mode-hook (lambda () (eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible try-complete-lisp-symbol try-complete-lisp-symbol-partially try-expand-dabbrev))))



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
  (add-hook 'emacs-lisp-mode-hook 'sm-lambda-mode-hook)
  (add-hook 'lisp-mode-hook 'sm-lambda-mode-hook)
  (add-hook 'lisp-interactive-mode-hook 'sm-lamba-mode-hook)
  (add-hook 'scheme-mode-hook 'sm-lambda-mode-hook)
  (add-hook 'python-mode-hook 'sm-lambda-mode-hook))

(defun dss/pretty-lambda-disable ()
  (interactive)
  (remove-hook 'emacs-lisp-mode-hook 'sm-lambda-mode-hook)
  (remove-hook 'lisp-mode-hook 'sm-lambda-mode-hook)
  (remove-hook 'lisp-interactive-mode-hook 'sm-lamba-mode-hook)
  (remove-hook 'scheme-mode-hook 'sm-lambda-mode-hook)
  (remove-hook 'python-mode-hook 'sm-lambda-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; another implementation of pretty lambda
;;(defun sm-lambda-mode-hook ()
;;  (substitute-patterns-with-unicode (list (cons "\\(lambda\\)" 'lambda))))
;;
;;(defun unicode-symbol (name)
;;   "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
;; or GREATER-THAN into an actual Unicode character code. "
;;   (decode-char 'ucs (case name
;;                       ('left-arrow 8592)
;;                       ('up-arrow 8593)
;;                       ('right-arrow 8594)
;;                       ('down-arrow 8595)
;;                       ('double-vertical-bar #X2551)
;;                       ('equal #X003d)
;;                       ('not-equal #X2260)
;;                       ('identical #X2261)
;;                       ('not-identical #X2262)
;;                       ('less-than #X003c)
;;                       ('greater-than #X003e)
;;                               ('less-than-or-equal-to #X2264)
;;                               ('greater-than-or-equal-to #X2265)
;;                       ('logical-and #X2227)
;;                       ('logical-or #X2228)
;;                       ('logical-neg #X00AC)
;;                       ('nil #X2205)
;;                       ('horizontal-ellipsis #X2026)
;;                       ('double-exclamation #X203C)
;;                       ('prime #X2032)
;;                       ('double-prime #X2033)
;;                       ('for-all #X2200)
;;                       ('there-exists #X2203)
;;                       ('element-of #X2208)
;;                       ('square-root #X221A)
;;                       ('squared #X00B2)
;;                       ('cubed #X00B3)
;;                       ('lambda #X03BB)
;;                       ('alpha #X03B1)
;;                       ('beta #X03B2)
;;                       ('gamma #X03B3)
;;                       ('delta #X03B4))))
;;
;;(defun substitute-patterns-with-unicode (patterns)
;;   "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
;;   (mapcar #'(lambda (x)
;;               (substitute-pattern-with-unicode (car x)
;;                                                (cdr x)))
;;           patterns))
;;
;;(defun substitute-pattern-with-unicode (pattern symbol)
;;  "Add a font lock hook to replace the matched part of PATTERN with the
;;     Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
;;  (interactive)
;;  (font-lock-add-keywords
;;   nil `((,pattern
;;          (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                    ,(unicode-symbol symbol)
;;                                    'decompose-region)
;;                    nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-lisps)
