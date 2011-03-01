(when (not (load "~/.emacs.d/el-get/el-get/el-get.el" t))
  (error "Please bootstrap el-get using the instructions here: http://github.com/dimitri/el-get/, then restart Emacs"))
;;; see https://github.com/purcell/emacs.d/blob/master/init-el-get.el


(setq el-get-byte-compile t
      el-get-generate-autoloads t
      el-get-sources
      '(el-get
        (:name unit-test :type emacswiki)

        ))

(defun el-get-update-all ()
  "Update all el-get packages
  This was copied from https://github.com/purcell/emacs.d/blob/master/init-el-get.el"
  (interactive)
  (dolist (package (mapcar 'el-get-source-name el-get-sources))
    (unless (memq (plist-get (el-get-package-def package) :type) '(http-tar elpa))
      (el-get-update package))))


(el-get 'sync)


(provide 'dss-init-el-get)
