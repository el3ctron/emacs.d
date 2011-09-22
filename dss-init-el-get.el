;;; bootstrap
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer (url-retrieve-synchronously
                        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp))
  (load-library "el-get"))

;;; see https://github.com/purcell/emacs.d for more ideas

;;; load packages
(setq el-get-byte-compile t
      el-get-generate-autoloads t
      el-get-sources
      '(
        (:name wanderlust :type git
               :url "https://github.com/wanderlust/wanderlust.git"
               :load-path ("site-lisp/wl" "elmo")
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
        (:name gist :type git :url "https://github.com/tels7ar/gist.el")
        (:name auto-complete
               :website "http://cx4a.org/software/auto-complete/"
               :description "The most intelligent auto-completion extension."
               :type git
               :url "http://github.com/m2ym/auto-complete.git"
               :load-path "."
               :post-init (lambda ()
                            (require 'auto-complete)
                            (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
                            ;; the elc is buggy for some reason
                            (let ((f "~/.emacs.d/el-get/auto-complete/auto-complete-config.elc"))
                              (if (file-exists-p f)
                                  (delete-file f)))
                            (require 'auto-complete-config)
                            (ac-config-default)
                            ))
        (:name slime-fuzzy :type http :url
               "http://elder-gods.org/~larry/repos/slime-tracker/contrib/slime-fuzzy.el")
        (:name isearch+ :type emacswiki)
        (:name idle-highlight-mode :type git :url
               "https://github.com/nonsequitur/idle-highlight-mode.git")
        (:name list-register :type http :url "http://www.bookshelf.jp/elc/list-register.el")
        (:name bm :type http :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")
        (:name window-numbering :type git :url "https://github.com/nschum/window-numbering.el.git")
        (:name column-marker :type http
               :url "http://www.emacswiki.org/emacs/download/column-marker.el")
        (:name col-highlight :type emacswiki)
        (:name eredis :type http :url "http://eredis.googlecode.com/svn/trunk/eredis.el")
        (:name desktop-recover :type git :url "https://github.com/doomvox/desktop-recover.git")
        (:name ac-dabbrev :type emacswiki)
        (:name show-wspace :type emacswiki)
        (:name relax :type git :url "https://github.com/technomancy/relax.el")
        (:name moz :type git :url "http://github.com/bard/mozrepl.git"
               :load "chrome/content/moz.el")
        (:name rainbow-delimiters :type git :url "https://github.com/jlr/rainbow-delimiters.git")
        (:name lineker :type http
               :url "http://www.helsinki.fi/~sjpaavol/programs/lineker.el")
        (:name js2-mode :type git :url "https://github.com/mooz/js2-mode")
        (:name elein :type git :url "https://github.com/remvee/elein.git")
        (:name pg :type http :url "http://www.online-marketwatch.com/pgel/pg.el")
        (:name stompem :type git :url "https://github.com/jwhitlark/Stompem.git")))


(setq dss-el-get-packages
      '(package
        smex command-frequency
        session desktop-recover

        diminish
        color-theme
        rainbow-mode
        isearch+
        undo-tree goto-last-change
        filladapt
        show-wspace lineker
        iedit highlight-symbol idle-highlight-mode
        list-register bm breadcrumb
        window-numbering column-marker vline col-highlight

        dvc egg magit magithub gist

        yasnippet auto-complete ac-dabbrev

        paredit rainbow-delimiters autopair
        slime slime-fuzzy ac-slime
        clojure-mode elein
        coffee-mode
        python-mode pymacs ipython virtualenv pylookup
        haskell-mode
        lua-mode
        js2-mode
        yaml-mode


        org-mode
        ess
        wanderlust emacs-jabber
        multi-term

        eredis
        relax
        moz
        pg
        ;; nose
        ;; flymake-point or flymake-cursor ;; I'm installing this from the vendors directory now
        ;; bookmark+ ;; the el-get recipe for this seems to be broken

        ))

;; (setq dss-el-get-packages (mapcar (lambda (s)
;;                                     (intern (el-get-source-name s)))
;;                                   el-get-sources))

(defun el-get-update-all ()
  "Update all el-get packages
  This was copied from https://github.com/purcell/emacs.d/blob/master/init-el-get.el"
  (interactive)
  (dolist (package dss-el-get-packages)
    (unless (memq (plist-get (el-get-package-def package) :type) '(http-tar elpa))
      (el-get-update package))))

(el-get 'wait dss-el-get-packages)

(provide 'dss-init-el-get)
