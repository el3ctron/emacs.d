(fset 'yes-or-no-p 'y-or-n-p) ; less typing

;; encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; visual frame / modeline settings
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq frame-title-format "Emacs--> %S: %f")
(setq visible-bell t)

;; modeline settings
(column-number-mode 1)
(setq display-time-day-and-date t)
(display-time-mode 1)

;; tabs and column defs
(setq-default indent-tabs-mode nil) ; no fucking tabs!
(setq tab-width 4) ; but just in case
(setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92)))
(setq fill-column 80)
(setq c-basic-offset 4)

;; reenable some disabled features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; scrolling behaviour
(setq scroll-step 1)
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position nil)

;; misc minor / helper modes
(global-font-lock-mode 1)
(delete-selection-mode 1)
(setq kill-whole-line t) ; if at the beg of line C-k includes the newline chars

(setq show-paren-style (quote parenthesis))
(show-paren-mode t)

(set-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun dss/set-undo-boundary ()
  (interactive)
  (undo-boundary))

;; mouse and selection settings
(add-hook 'after-init-hook (lambda ()
                             (xterm-mouse-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-basic-default-config)
