(setq-default bookmark-default-file (concat dss-ephemeral-dir "emacs.bmk"))
(setq bookmark-save-flag 1)

(setq savehist-file (concat dss-ephemeral-dir "history"))
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

(require 'saveplace)
(setq save-place-file (concat dss-ephemeral-dir "saveplace"))
(setq-default save-place t)

(require 'recentf)
(setq recentf-max-saved-items 100)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/session")
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file (concat dss-ephemeral-dir "session"))
(setq session-save-file-coding-system 'utf-8)

;;; xahlee's lib
(defun dss/command-freq-init ()
  (setq-default command-frequency-table-file (concat dss-ephemeral-dir "frequencies"))
  (require 'command-frequency)
  (command-frequency-table-load)
  (command-frequency-mode 1)
  (command-frequency-autosave-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-command-history-and-freqs)
