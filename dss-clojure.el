;;; setting up clojure/slime http://technomancy.us/126
;;; requires elpa

(defun dss/init-clojure ()
  (interactive)
  (dss/init-elpa)
  (clojure-slime-config)
  (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup))

(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))
        (defun paredit-mode-enable () (paredit-mode 1))
        (add-hook 'slime-mode-hook 'paredit-mode-enable)
        (add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
        (add-hook 'slime-mode-hook 'dss/load-slime-completion)
        (add-hook 'slime-repl-mode-hook 'dss/load-slime-completion)
        (setq slime-protocol-version 'ignore)))

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
(provide 'dss-clojure)
