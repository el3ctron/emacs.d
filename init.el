(add-to-list 'load-path (file-name-directory (or (buffer-file-name) load-file-name)))
(require 'dss-paths)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see .emacs.d/requirements.txt I install most dependencies in
;; /usr/share/emacs/site-lisp via gentoo's portage
(require 'dss-basic-default-config)
(require 'dss-no-rsi)
;; where to store ephemeral stuff, and what to store between sessions
(require 'dss-command-history-and-freqs)
(require 'dss-tmp-files)

(require 'dss-elpa-support)

;; global stuff
(require 'dss-basic-editing)
(require 'dss-whitespace-and-linelen)
(require 'dss-buffer-and-window-handling)
(require 'dss-codenav-helpers)
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
(require 'dss-smex)
