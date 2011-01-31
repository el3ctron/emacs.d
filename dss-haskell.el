;; Haskell
(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-to-list 'load-path "/home/tavis/.cabal/share/scion-current/emacs")
(require 'scion nil t)
(setq scion-completing-read-function 'ido-completing-read)
;; if ./cabal/bin is not in your $PATH
(setq scion-program "~/.cabal/bin/scion-server")

(add-to-list 'load-path "/home/tavis/.cabal/share/ghc-mod-current")
(require 'ghc nil t)

(add-to-list 'load-path "/home/tavis/.cabal/share/hlint-current")
(require 'hs-lint nil t)

;;; see http://www.emacswiki.org/cgi-bin/wiki/FlymakeHaskell

(defun dss/haskell-electric-pair ()
  ;; this version doesn't check to see if we're inside of a string or comment
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun dss/haskell-hook ()
  ;; (scion-mode 1)
  ;; (scion-flycheck-on-save 1)
  (linum-mode 1)
  (dss/install-whitespace-cleanup-hook)
  (dss/load-lineker-mode)
  (mapc (lambda (char)
          (progn
            (define-key haskell-mode-map char 'dss/haskell-electric-pair)
            ;; (define-key inferior-haskell-mode-map char 'dss/haskell-electric-pair)
            ))
        '("\"" "(" "[" "{")))

;inferior-haskell-mode-map

(add-hook 'haskell-mode-hook 'dss/haskell-hook)

(provide 'dss-haskell)
