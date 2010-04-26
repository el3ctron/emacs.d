(icomplete-mode)
(partial-completion-mode)


(setq-default abbrev-mode t)
(setq save-abbrevs t)
(setq abbrev-file-name (concat dss-ephemeral-dir "abbrev_defs"))
(if (file-exists-p abbrev-file-name)
    (read-abbrev-file abbrev-file-name t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
(require 'ido)
(ido-mode t) ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)

;; from http://emacs-fu.blogspot.com/2009_02_01_archive.html
(setq
  ido-ignore-buffers  '("\\` "  "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
                        "^\*compilation" "^\*GTAGS" "^session\.*")
  ;ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point t
  ido-use-url-at-point nil
  ;ido-max-prospects 8              ; don't spam my minibuffer
  ;ido-confirm-unique-completion t ; wait for RET, even with unique completion
  ido-save-directory-list-file (concat dss-ephemeral-dir "ido.last")
  )

;; also see dss/load-rope-completion in dss-python.el

;http://www.rlazo.org/blog/entry/2008/sep/13/insert-a-path-into-the-current-buffer/
(defun dss/insert-path (file)
 "insert file"
 (interactive "FPath: ")
 (insert (expand-file-name file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-completion)
