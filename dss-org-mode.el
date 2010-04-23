(add-to-list 'load-path "/usr/share/emacs/site-lisp/org-mode")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(require 'org-install)
(require 'remember)
(org-remember-insinuate)
;; user specific org-mode stuff is loaded after this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-org-mode)
