;; I keep this off for now as I don't find elpa to be complete enough for my needs

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; This was installed by package-install.el.
;; ;;; This provides support for the package system and
;; ;;; interfacing with ELPA, the package archive.
;; ;;; Move this code earlier if you want to reference
;; ;;; packages in your .emacs.
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))


(defun dss/init-elpa ()
  (interactive)
  (unless (boundp 'package-initialize)
    (when
        (load
         (expand-file-name (concat dss-dotfiles-dir "elpa/package.el")))
      (package-initialize))))

(provide 'dss-elpa-support)
