(when (not (load "~/.emacs.d/el-get/el-get/el-get.el" t))
  (error "Please bootstrap el-get using the instructions here: http://github.com/dimitri/el-get/, then restart Emacs"))
;;; see https://github.com/purcell/emacs.d/blob/master/init-el-get.el


(setq el-get-byte-compile t
      el-get-generate-autoloads t
      el-get-sources
      '(el-get
        package
        (:name unit-test :type emacswiki)
        rainbow-mode
        smex
        ;slime
        ;(:name slime :type git :url "git://sbcl.boinkor.net/slime.git" :load-path ("." "./contrib") :compile nil :load "slime-autoloads.el") ; Overridden to prefer git mirror
        (:name slime-fuzzy :type http :url
               "http://elder-gods.org/~larry/repos/slime-tracker/contrib/slime-fuzzy.el")
        ac-slime
        clojure-mode
        durendal

        undo-tree
        (:name moz :type git :url "http://github.com/bard/mozrepl.git" :load-path "chrome/content/moz.el")
        (:name pomodoro :type emacswiki)
        (:name lineker :type emacswiki)
        (:name js2-mode :type git :url "https://github.com/mooz/js2-mode")))

(defun el-get-update-all ()
  "Update all el-get packages
  This was copied from https://github.com/purcell/emacs.d/blob/master/init-el-get.el"
  (interactive)
  (dolist (package (mapcar 'el-get-source-name el-get-sources))
    (unless (memq (plist-get (el-get-package-def package) :type) '(http-tar elpa))
      (el-get-update package))))


(el-get 'sync)


(provide 'dss-init-el-get)
