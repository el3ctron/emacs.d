(when (not (load "~/.emacs.d/el-get/el-get/el-get.el" t))
  (error "Please bootstrap el-get using the instructions here: http://github.com/dimitri/el-get/, then restart Emacs"))
;;; see https://github.com/purcell/emacs.d/blob/master/init-el-get.el

(setq el-get-byte-compile t
      el-get-generate-autoloads t
      el-get-sources
      '(el-get
        package
        ;;cedet
        ;;ecb
        (:name desktop-recover :type git :url "https://github.com/doomvox/desktop-recover.git")

        ess

        ;;vm
        flim
        apel
        semi
        bbdb
        emacs-jabber
        (:name wanderlust :type git
               :url "https://github.com/wanderlust/wanderlust.git"
               :load-path ("site-lisp/wl" "elmo")
               ;; :build `,(mapcar
               ;;           (lambda (target)
               ;;             (concat "make " target " EMACS=" el-get-emacs))
               ;;           '("clean" "all"))
               :build (mapcar
                       (lambda (target-and-dirs)
                         (list el-get-emacs
                               (mapcar (lambda (pkg)
                                         (mapcar (lambda (d) `("-L" ,d)) (el-get-load-path pkg)))
                                       (append
                                        '("apel" "flim" "semi")
                                        (when (el-get-package-exists-p "bbdb") (list "bbdb"))))
                               "--eval" (prin1-to-string
                                         '(progn (setq wl-install-utils t)
                                                 (setq wl-info-lang "en")
                                                 (setq wl-news-lang "en")))

                               (split-string "-batch -q -no-site-file -l WL-MK -f")
                               target-and-dirs))
                       '(("wl-texinfo-format" "doc")
                         ("compile-wl-package"  "site-lisp" "icons")
                         ("install-wl-package" "site-lisp" "icons")))
               :info "doc/wl.info")

        magit
        magithub
        (:name gist :type git :url "https://github.com/tels7ar/gist.el")
        auto-complete
        (:name org-mode
               :type git
               :url "git://orgmode.org/org-mode.git"
               :info "doc"
               :build `,(mapcar
                         (lambda (target)
                           (concat "make " target " EMACS=" el-get-emacs))
                         '("clean" "all"))
               :load-path ("lisp" "contrib/lisp")
               :autoloads nil
               :features org-install)
        (:name unit-test :type emacswiki)
        rainbow-mode
        smex
        bookmark+
        (:name slime :type git
               ;;:url "https://github.com/technomancy/slime.git"
               :url "git://sbcl.boinkor.net/slime.git"
               :load "slime-autoloads.el")
        (:name slime-repl :type git
               ;;:url "https://github.com/technomancy/slime.git"
               :url "git://sbcl.boinkor.net/slime.git"
               :load "contrib/slime-repl.el")
                                        ;(:name slime :type git :url "git://sbcl.boinkor.net/slime.git" :load-path ("." "./contrib") :compile nil :load "slime-autoloads.el") ; Overridden to prefer git mirror

        (:name slime-fuzzy :type http :url
               "http://elder-gods.org/~larry/repos/slime-tracker/contrib/slime-fuzzy.el")
        ac-slime
        clojure-mode

        (:name eredis :type http :url "http://eredis.googlecode.com/svn/trunk/eredis.el")
        ;; http://code.google.com/p/eredis/wiki/wiki for docs

        ;; (:name clojure-test-mode
        ;;        :type git
        ;;        :url "https://github.com/technomancy/clojure-mode.git"
        ;;        )
        ;; (:name swank-clojure
        ;;        :type git
        ;;        :url "https://github.com/technomancy/clojure-mode.git"
        ;;        )
        scala-mode
        ensime
        yaml-mode
        (:name mustache-mode
               :type http
               :url "https://raw.github.com/defunkt/mustache/master/contrib/mustache-mode.el")
        durendal
        (:name ac-dabbrev :type emacswiki)
        pymacs
        yasnippet
        iedit
        undo-tree
        (:name relax :type git :url "https://github.com/technomancy/relax.el")
        coffee-mode

        python-mode
        ;; alternate python mode
                                        ;(:name python :type git :url "https://github.com/fgallina/python.el.git")

        (:name moz :type git :url "http://github.com/bard/mozrepl.git"
               :load "chrome/content/moz.el")
        (:name pomodoro :type emacswiki)
        (:name rainbow-delimiters :type git :url "https://github.com/jlr/rainbow-delimiters.git")
        (:name lineker :type http
               :url "http://www.helsinki.fi/~sjpaavol/programs/lineker.el")
        (:name js2-mode :type git :url "https://github.com/mooz/js2-mode")
        (:name elein :type git :url "https://github.com/remvee/elein.git")
        (:name pg :type http :url "http://www.online-marketwatch.com/pgel/pg.el")))

(defun el-get-update-all ()
  "Update all el-get packages
  This was copied from https://github.com/purcell/emacs.d/blob/master/init-el-get.el"
  (interactive)
  (dolist (package (mapcar 'el-get-source-name el-get-sources))
    (unless (memq (plist-get (el-get-package-def package) :type) '(http-tar elpa))
      (el-get-update package))))

(el-get)

(provide 'dss-init-el-get)
