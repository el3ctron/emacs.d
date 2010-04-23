;; python-mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/python-mode")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ipython")
(autoload 'python-mode "python-mode" "PY" t)

(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq pycodechecker "dss_pycheck")
(when (load "flymake" t)
  (load-library "flymake-cursor")
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

(defun dss/load-rope-completion ()
  (interactive)
  (require 'auto-complete-python)
  (setq ropemacs-enable-autoimport t)
  (ac-ropemacs-setup)
  (setq ac-candidate-menu-height 20)
  (smex-update))

(defun dss/python-mode-hook ()
  (dss/install-whitespace-cleanup-hook)
  (turn-on-auto-fill)
  (which-function-mode t)
  (set (make-variable-buffer-local 'beginning-of-defun-function)
       'py-beginning-of-def-or-class)
  (setq outline-regexp "def\\|class ")

  (require 'ipython)
  (setq ipython-command "ipython")
  (setq py-python-command-args '("-colors" "Linux"))
  ;(require 'eldoc)
  ;(eldoc-mode 1)

  (if (and (string-match "\\.py$" (buffer-name))
           ; and isn't a py-shell tmp buffer:
           (not (string-match "python-" (buffer-name))))
      (progn
        (unless dss/ecb-loaded
          (dss/load-ecb)
          (smex-update))
        (dss/load-lineker-mode)
        ;(flyspell-prog-mode)
        (flymake-mode t)
        (dss/load-rope-completion)
        (ropemacs-mode t)
        ;(ecb-rebuild-methods-buffer)
      ))

  ;; custom keybindings
  ;(local-set-key (kbd "C-c a") 'py-beginning-of-def-or-class)
  ;(local-set-key (kbd "M-<right>") 'py-forward-into-nomenclature)
  ;(local-set-key (kbd "M-<left>") 'py-backward-into-nomenclature)
  ;(local-set-key (kbd "M-DEL") 'py-backward-kill-nomenclature)
  (define-key py-shell-map "\C-e" (lambda ()
                                    (interactive)
                                    (goto-char (point-max))))
  (define-key py-shell-map (quote [up]) 'comint-previous-matching-input-from-input)
  (define-key py-shell-map (quote [down]) 'comint-next-matching-input-from-input)
  )
(add-hook 'python-mode-hook 'dss/python-mode-hook)

(autoload 'rst "rst")
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

(autoload 'doctest-mode "doctest-mode" "Editing mode for Python Doctest examples." t)
(autoload 'doctest-register-mmm-classes "doctest-mode")
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(doctest-register-mmm-classes t t)
; # @@TR: eldoc

;; cheetah .tmpl files
(autoload 'cheetah-mode "cheetah-mode")
(add-to-list 'auto-mode-alist '("\\.tmpl$" . cheetah-mode))

;; `Cython' mode.
(autoload 'cython-mode "cython-mode")
(add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd$" . cython-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python-mode helpers

(defun dss/pylint-msgid-at-point ()
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info msgid)
      (if (eq (car elem) line-no)
            (let ((err (car (second elem))))
              (setq msgid (second (split-string (flymake-ler-text err)))))))))

(defun dss/pylint-silence (msgid)
  (interactive (list (read-from-minibuffer "msgid: " (dss/pylint-msgid-at-point))))
  (save-excursion
    (comment-dwim nil)
    (if (looking-at "pylint:")
        (progn (end-of-line)
               (insert ","))
        (insert "pylint: disable-msg="))
    (insert msgid)))

;; pylookup, to look though online Python docs
;; (git clone git://github.com/tsgates/pylookup.git)
(load-file "~/.emacs.d/vendor/pylookup/pylookup.el")
(eval-when-compile (require 'pylookup))
(setq pylookup-program "~/.emacs.d/vendor/pylookup/pylookup.py")
(setq pylookup-db-file "~/.emacs.d/vendor/pylookup/pylookup.db")
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(global-set-key "\C-ch" 'pylookup-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-python)
