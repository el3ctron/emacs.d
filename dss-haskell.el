;; Haskell
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-to-list 'load-path "/home/tavis/.cabal/share/scion-current/emacs")
(require 'scion)

(add-to-list 'load-path "/home/tavis/.cabal/share/ghc-mod-current")
(require 'ghc)

(add-to-list 'load-path "/home/tavis/.cabal/share/hlint-current")
(require 'hs-lint)

;; if ./cabal/bin is not in your $PATH
(setq scion-program "~/.cabal/bin/scion-server")

;;; see http://www.emacswiki.org/cgi-bin/wiki/FlymakeHaskell

(defun dss/haskell-hook ()
  ;; (scion-mode 1)
  ;; (scion-flycheck-on-save 1)
  (linum-mode 1)
  (dss/install-whitespace-cleanup-hook)
  (dss/load-lineker-mode)
  (mapc (lambda (char)
            (progn
              (define-key haskell-mode-map char 'dss/electric-pair)
              (define-key inferior-haskell-mode-map char 'dss/electric-pair)
              ))
          '("\"" "(" "[" "{")))

(add-hook 'haskell-mode-hook 'dss/haskell-hook)

;; Use ido-mode completion (matches anywhere, not just beginning)
;;
;; WARNING: This causes some versions of Emacs to fail so badly
;; that Emacs needs to be restarted.
(setq scion-completing-read-function 'ido-completing-read)

(provide 'dss-haskell)
