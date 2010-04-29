(setq dss-dotfiles-dir (file-name-directory
                        (or (buffer-file-name) load-file-name)))
(setq dss-ephemeral-dir "~/.emacs.ephemeral/")
(unless (file-exists-p dss-ephemeral-dir)
  (mkdir dss-ephemeral-dir))
(setq dss-vendor-dir (concat dss-dotfiles-dir "vendor/"))

(add-to-list 'load-path dss-dotfiles-dir)
(add-to-list 'load-path dss-vendor-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see .emacs.d/requirements.txt I install most dependencies in
;; /usr/share/emacs/site-lisp via gentoo's portage
(require 'dss-basic-default-config)

;; where to store ephemeral stuff, and what to store between sessions
(require 'dss-command-history-and-freqs)
(require 'dss-tmp-files)

(require 'dss-elpa-support)

;; global stuff
(require 'dss-whitespace-and-linelen)
(require 'dss-buffer-and-window-handling)
(require 'dss-completion)
(require 'dss-search-occur-etc)
(require 'dss-bookmarks-registers)
(require 'dss-vc)

;; non-lang major modes
(require 'dss-org-mode)
(require 'dss-term)
(require 'dss-ecb)

;; language related major modes
(require 'dss-python)
(require 'dss-javascript)
(require 'dss-ess) ; emacs-speaks-statistics for R-lang support
(require 'dss-lisps)
(require 'dss-haskell)
(require 'dss-nxml)
(require 'dss-lua)
(require 'dss-clojure)

;; other support funcs and minor modes
(require 'dss-codenav-helpers)
(require 'dss-keybindings)
(require 'dss-colorthemes)
(require 'dss-browser-integration)
(require 'dss-clipboard-integration)
(require 'dss-yas)

;; alternative location for custom-set-variables and custom-set-faces
(setq custom-file (concat dss-dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(require 'dss-user-specific-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex must be loaded last as it creates a cache of available commands
(require 'smex)
(setq smex-save-file (concat dss-ephemeral-dir "smex.save"))
(smex-initialize)
