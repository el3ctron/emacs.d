;;; setting up clojure/slime http://technomancy.us/126
(require 'dss-slime)
(require 'durendal)

(defun dss/init-clojure ()
  (interactive)
  (dss/init-elpa)
  (clojure-slime-config)
  (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup))


(defun dss/slime-repl-hook ()
  (interactive)

  ;; also see
  ;; https://github.com/technomancy/durendal/blob/master/durendal.el
  ;; and https://github.com/tcrayford/clojure-refactoring
  (paredit-mode 1)
  (dss/load-slime-completion)

  ;; the rest is semi-copied from
  ;; http://stackoverflow.com/questions/2474804/is-there-a-colored-repl-for-clojure
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (font-lock-mode nil)
  (clojure-mode-font-lock-setup)
  (font-lock-mode t)
  (ad-activate #'slime-repl-emit)
  (ad-activate #'slime-repl-insert-prompt)
  )
(add-hook 'slime-repl-mode-hook 'dss/slime-repl-hook)

(provide 'dss-clojure)
