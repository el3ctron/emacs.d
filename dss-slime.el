
(require 'slime)
(slime-setup '(slime-repl))
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


;;; the following code is just me playing around
;; (let ((string "(find-namespaces-on-classpath)"))
;;   (slime-eval-async `(swank:eval-and-grab-output ,string)
;;     (lambda (result)
;;       (destructuring-bind (output value) result
;;         (insert output value)))))

;; (defun dss/slime-eval (string)
;;   (interactive)
;;   (slime-eval `(swank:eval-and-grab-output ,string)))

;; (let ((string "(find-namespaces-on-classpath)"))
;;   (dss/slime-eval string)
;;   )
;; (setq foobar (dss/slime-eval "1234"))
;; foobar
;; (slime-interactive-eval )
