;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize)
  (add-to-list 'package-archives
                   '("technomancy" . "http://repo.technomancy.us/emacs/") t))


;; (defun dss/init-elpa ()
;;   (interactive)
;;   (unless (boundp 'package-initialize)
;;     (when
;;         (load
;;          (expand-file-name (concat dss-dotfiles-dir "elpa/package.el")))
;;       (add-to-list 'package-archives
;;                    '("technomancy" . "http://repo.technomancy.us/emacs/") t)
;;       (package-initialize))))

(provide 'dss-elpa-support)
