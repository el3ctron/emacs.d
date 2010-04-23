;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml
(add-to-list 'load-path "~/.emacs.d/vendor/nxml-html5-el")
(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/~/.emacs.d/vendor/nxml-html5-el/schemas.xml"))
(require 'whattf-dt)

(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sch$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss$" . nxml-mode))

(add-hook 'nxml-mode-hook
          (lambda ()
            (outline-minor-mode)
;;             (xslt-process-mode t)
            (setq xslt-process-default-processor (quote (Xalan)))
            (setq nxml-child-indent 2)
            (setq nxml-auto-insert-xml-declaration-flag t)
            (setq nxml-slash-auto-complete-flag t)

            (setq nxml-bind-meta-tab-to-complete-flag t)

            (setq outline-regexp "^[ \t]*\<[a-zA-Z]+")
            (local-set-key (kbd "C-;") 'my-outline-toggle-children)
            (local-set-key (kbd "<return>") 'newline-and-indent)
            (local-set-key (kbd "<tab>") 'nxml-complete)))

(provide 'dss-nxml)
