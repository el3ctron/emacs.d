
(require 'slime)
(slime-setup '(slime-repl slime-js))
(setq slime-protocol-version 'ignore)

(add-to-list 'load-path (concat dss-vendor-dir "ac-slime"))
(require 'ac-slime)
(defun dss/load-slime-completion()
  (interactive)
  (setq ac-sources (list
                    ac-source-slime-simple
                    ;;ac-source-slime-fuzzy
                    ac-source-words-in-buffer
                    ;;ac-source-filename
                    ac-source-dss-filename
                    ;;ac-source-words-in-same-mode-buffers
                    ;;ac-source-dictionary
                    )))


(defun dss/slime-hook ()
  (paredit-mode 1)
  (dss/load-slime-completion))

(add-hook 'slime-mode-hook 'dss/slime-hook)

(provide 'dss-slime)
