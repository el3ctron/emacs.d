(require 'auto-complete)
(require 'ido)
(require 'comint)
(require 'org)
(require 'fm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional custom keymaps, populated below
(defvar f2-map (make-sparse-keymap))
;(defvar f3-map (make-sparse-keymap))
(defvar f4-map (make-sparse-keymap))
(defvar f6-map (make-sparse-keymap))
(defvar f7-map (make-sparse-keymap))
(defvar f8-map (make-sparse-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global map
(define-key global-map (kbd "M-X") 'smex-update-and-run)
(define-key global-map (kbd "M-x") 'smex)
(define-key global-map (kbd "M-s") 'ido-goto-symbol)
(define-key global-map (kbd "M-]") 'dss/goto-match-paren)

(defun dss/hippie-expand ()
  (interactive)
  (if (not ac-completing)
      (call-interactively 'hippie-expand)
    (ac-expand)))
(define-key ac-completing-map (kbd "M-/") 'ac-expand)
(global-set-key (kbd "M-/") 'dss/hippie-expand)
(global-set-key (kbd "M-TAB") 'dabbrev-expand)


(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-M-r") 'org-remember)

(global-set-key (kbd "C-x r b") 'dss/bookmark-jump)
(global-set-key (kbd "C-x r v") 'list-register)

(define-key help-map "." 'find-function)
(define-key help-map "," 'find-variable)

(define-key global-map [(f2)] f2-map)
(define-key global-map "\eOQ" f2-map)

(define-key global-map [(f3)] 'dss/smex)
(define-key global-map "\eOR" 'dss/smex)

(define-key global-map [(f4)] f4-map)
(define-key global-map "\eOS" f4-map)
(define-key global-map [(f5)] 'undo)
(define-key global-map [(f6)] f6-map)
(define-key global-map [(f7)] f7-map)
(define-key global-map [(f8)] f8-map)

(define-key global-map [(alt insert)] 'yank-pop)
(define-key global-map "\e[4~" 'end-of-line)
(define-key global-map "\e\e[4~" 'end-of-buffer)
(define-key global-map "\e\e[1~" 'beginning-of-buffer)

(define-key global-map "\eOa" 'backward-paragraph)
(define-key global-map "\e\eOA" 'backward-paragraph)
(define-key global-map "\e\e[A" 'backward-paragraph)
(define-key global-map "\e\eOB" 'forward-paragraph)
(define-key global-map "\e\e[B" 'forward-paragraph)
(define-key global-map "\eOb" 'forward-paragraph)

(define-key global-map "\eOc" (kbd "M-f"))
(require 'misc)
(define-key global-map (kbd "M-f") 'forward-to-word)
(define-key global-map "\eOd" 'backward-word)

;;
(define-key global-map "\e[A" (kbd "<up>"))
(define-key global-map "\e[B" (kbd "<down>"))
(define-key global-map "\e[C" (kbd "<right>"))
(define-key global-map "\e[D" (kbd "<left>"))

(define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
(define-key paredit-mode-map (kbd "C-<left>") 'backward-word)

(define-key ac-completing-map "\e[A" 'ac-previous)
(define-key ac-completing-map "\e[B" 'ac-next)

(define-key ac-completing-map "\eOa" 'ac-quick-help-scroll-up)
(define-key ac-completing-map "\eOb" 'ac-quick-help-scroll-down)

(define-key ido-common-completion-map "\e[A" 'previous-history-element)
(define-key ido-common-completion-map "\e[B" 'next-history-element)
(define-key ido-common-completion-map "\e[C" 'ido-next-match)
(define-key ido-common-completion-map "\e[D" 'ido-prev-match)

(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(define-key global-map [mouse-4] 'down-slightly)
(define-key global-map [mouse-6] 'down-slightly)
(define-key global-map [mouse-5] 'up-slightly)



;http://www.emacsblog.org/2007/02/27/quick-tip-add-occur-to-isearch/
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun dss/confirm-exit-emacs ()
  "ask for confirmation before exiting emacs"
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'dss/confirm-exit-emacs)
(global-set-key "\C-x\C-m" 'execute-extended-command)
                      ;for when I don't want to use smex
(global-set-key "\C-x\C-b" 'ibuffer)

;comint-previous-matching-input-from-input
  ;; (define-key py-shell-map (quote [up]) 'comint-previous-matching-input-from-input)
  ;; (define-key py-shell-map (quote [down]) 'comint-next-matching-input-from-input)

(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map [(meta p)]
              'comint-previous-matching-input-from-input)
            (define-key comint-mode-map [(meta n)]
              'comint-next-matching-input-from-input)
            (define-key comint-mode-map [(control n)]
              'comint-next-input)
            (define-key comint-mode-map [(control p)]
              'comint-previous-input)
            ))


(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "M-TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional custom maps, defined and hooked into global-map above

;; f2-map

(define-key f2-map "j" 'windmove-left)
(define-key f2-map "l" 'windmove-right)
(define-key f2-map "k" 'windmove-down)
(define-key f2-map "i" 'windmove-up)

(define-key f2-map "m" 'flymake-goto-prev-error)
(define-key f2-map "," 'flymake-goto-next-error)

(define-key f2-map "[" 'isearch-forward-at-point)

(define-key f2-map "=" 'dvc-status)
(define-key f2-map "-" 'dvc-diff)

(define-key f2-map "r" 'recursive-edit)


;; f4-map

(define-key f4-map " " 'fixup-whitespace)
(defun dss/eval-region-or-last-sexp ()
  (interactive)
  (if mark-active
      (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

(define-key f4-map "s" 'dss/eval-region-or-last-sexp)
(define-key f4-map "d" 'dss/eval-defun)

(define-key f4-map "0" 'dss/out-sexp)
(define-key f4-map "8" 'dss/out-one-sexp)

(define-key f4-map [(f4)] (kbd "TAB"))
;(define-key f4-map [(f4)] 'kill-ring-save)
(define-key f4-map "i" 'yank)
(define-key f4-map "u" 'undo)

(define-key f4-map "y" 'dss/mark-string)
(define-key f4-map "m" 'mark-sexp)

(define-key f4-map "'" (kbd "\""))

;;(define-key f4-map "n" 'dss/goto-match-paren)

(define-key f4-map ";" 'goto-last-change)

(define-key f4-map "6" 'dss/backward-string)
(define-key f4-map "7" 'dss/forward-string)

(define-key f4-map "/" 'dss/goto-match-paren)
(define-key f4-map "]" 'dss/smex)


;; f6-map


(defun dss/insert-todo ()
  (interactive)
  (insert comment-start)
  (insert "@@TR: "))

(defun open-next-line()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun fm-occur(arg)
  (interactive "soccur: ")
  (occur arg)
  (other-window 1)
  (fm-start))

(define-key f6-map "`" 'open-next-line)
(define-key f6-map [(f6)] 'open-next-line)
(define-key f6-map "1" 'replace-string)
(define-key f6-map "i" 'dss/insert-todo)
(define-key f6-map "l" 'linum-mode)
(define-key f6-map "o" 'fm-occur)
(define-key f6-map ";" 'string-rectangle)

(define-key f6-map "k" 'dss/kill-buffer)

;; f7-map, which I rarely use these days
(require 'k2-mode) ; my old keymap extension package
(k2-mode) ; start k2-mode by default

(define-key f7-map "l" 'list-bookmarks)
(define-key f7-map "g" 'dss/bookmark-jump)
(define-key f7-map "b" 'ido-switch-buffer)

(define-key f7-map "a" 'auto-complete-mode)
(define-key f7-map "=" 'dss/toggle-current-window-dedication)

;(define-key f7-map "1" 'k2-kill-whole-sexp)
;(define-key f7-map "2" 'k2-copy-whole-sexp)
;(define-key f7-map "3" 'mark-sexp)
(define-key f7-map "'" 'k2-kill-whole-paragraph)
(define-key f7-map "," 'k2-copy-whole-paragraph)
(define-key f7-map "." 'mark-paragraph)

;; (define-key f7-map "a" 'k2-kill-whole-line)
;; (define-key f7-map "o" 'k2-copy-whole-line)
;; (define-key f7-map "e" 'k2-mark-whole-line)

;; (define-key f7-map ";" 'k2-kill-whole-sentence)
;; (define-key f7-map "q" 'k2-copy-whole-sentence)
;; (define-key f7-map "j" 'k2-mark-whole-sentence)

(define-key f7-map "0" 'k2-toggle-mark)

;; (define-key f7-map "g" 'k2-kill-whole-defun)
;; (define-key f7-map "c" 'k2-copy-whole-defun)
;; (define-key f7-map "r" 'mark-defun)

;; (define-key f7-map "h" 'k2-kill-whole-word)
;; (define-key f7-map "t" 'k2-copy-whole-word)
;; (define-key f7-map "n" 'mark-word)



;; f8 map for org-mode
;; @@TR: add bindings for org node promote, demote
(define-key f8-map "n" 'org-insert-heading-respect-content)
(define-key f8-map "s" 'org-insert-subheading)
(define-key f8-map "t" 'org-insert-todo-heading-respect-content)
(define-key f8-map "m" 'dss/multi-term)

(define-key f8-map "r" 'org-remember)

(define-key f8-map "i" 'org-clock-in)
(define-key f8-map "o" 'org-clock-out)
(define-key f8-map "d" (lambda ()
                          "C-u C-u C-c C-x C-i -> org-clock-mark-default-task"
                          (interactive)
                          (org-clock-in '(16))))
(define-key f8-map "." 'org-time-stamp)

(define-key f8-map "c" 'org-clock-cancel)

;(define-key f8-map "_" 'org-clock-select-task)
(defun dss/org-clock-in-select ()
  "C-u C-c C-x C-i -> org-clock-select-task"
  (interactive)
  (org-clock-in '(4)))

(defun dss/org-clock-goto-select-task ()
  "C-u C-c C-x C-i -> org-clock-goto via org-clock-select-task"
  (interactive)
  (org-clock-goto '(4)))

(defun dss/org-refile-goto ()
  "org-goto using refile ui"
  (interactive)
  (org-refile '(4)))

(define-key f8-map "_" 'dss/org-clock-in-select)
(define-key f8-map "-" 'org-clock-goto)
(define-key f8-map "'" 'dss/org-clock-goto-select-task)
(define-key f8-map "g" 'dss/org-refile-goto)

;; (define-key f8-map "l" (lambda ()
;;                           "org-goto last refile location"
;;                           (interactive)
;;                           (org-refile '(16))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-keybindings)
