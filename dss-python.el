;; python-mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/python-mode")
(autoload 'python-mode "python-mode" "PY" t)


(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq pycodechecker "dss_pycheck") ; this is a wrapper around pep8.py, pyflakes and pylint
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

;; (defun dss/load-rope-completion ()
;;   (interactive)
;;   (require 'auto-complete-python)
;;   (setq ropemacs-enable-autoimport nil)
;;   (ac-ropemacs-setup)
;;   (smex-update))
;;

;; setup pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(defvar dss-ropemacs-loaded nil)
(defun dss/ropemacs-init ()
  (interactive)
  (unless dss-ropemacs-loaded
    (if (not (boundp 'ropemacs-global-prefix))
        (setq ropemacs-global-prefix nil))
    (pymacs-load "ropemacs" "rope-")
    (setq ropemacs-enable-autoimport nil)
    (setq dss-ropemacs-loaded t)))


(defun dss/py-next-line ()
  (interactive)
  (end-of-line)
  (py-newline-and-indent)
  )

(defun dss/python-mode-hook ()
  (dss/install-whitespace-cleanup-hook)
  (turn-on-auto-fill)
  (which-function-mode t)
  (set (make-variable-buffer-local 'beginning-of-defun-function)
       'py-beginning-of-def-or-class)
  (setq outline-regexp "def\\|class ")

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
        (dss/ropemacs-init)
        (ropemacs-mode t)

        (dss/load-rope-completion)
        ;(ecb-rebuild-methods-buffer)
      ))

  ;; custom keybindings
  (define-key py-mode-map "\"" 'dss/electric-pair)
  (define-key py-mode-map "\'" 'dss/electric-pair)
  (define-key py-mode-map "(" 'dss/electric-pair);dss/electric-pair)
  (define-key py-mode-map "[" 'dss/electric-pair)
  (define-key py-mode-map "{" 'dss/electric-pair)
  (define-key py-mode-map (kbd "M-RET") 'dss/py-next-line)

  (define-key py-shell-map "\"" 'dss/electric-pair)
  (define-key py-shell-map "\'" 'dss/electric-pair)
  (define-key py-shell-map "(" 'dss/electric-pair);dss/electric-pair)
  (define-key py-shell-map "[" 'dss/electric-pair)
  (define-key py-shell-map "{" 'dss/electric-pair)

  (define-key py-shell-map "\C-e" (lambda ()
                                    (interactive)
                                    (goto-char (point-max))))
  (define-key py-shell-map (quote [up]) 'comint-previous-matching-input-from-input)
  (define-key py-shell-map (quote [down]) 'comint-next-matching-input-from-input)

  (local-set-key "\C-ch" 'pylookup-lookup)
  )
(add-hook 'python-mode-hook 'dss/python-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python-mode helpers

;; ipython related
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ipython")
(require 'ipython)
(setq ipython-command "ipython")
(defun dss/start-ipy-complete ()
  (interactive)
  (setq ac-sources '(ac-source-dss-ipy-dot ac-source-dss-ipy ac-source-filename))
  )
(add-hook 'ipython-shell-hook 'dss/start-ipy-complete)
(add-hook 'py-shell-hook 'dss/start-ipy-complete)
;;

(autoload 'rst "rst")
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))
;;
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
(setq dss-pylookup-dir (concat dss-vendor-dir "pylookup/"))
(setq pylookup-program (concat dss-pylookup-dir "pylookup.py"))
(setq pylookup-db-file (concat dss-pylookup-dir "pylookup.db"))

(load-file (concat dss-pylookup-dir "pylookup.el"))
(eval-when-compile (require 'pylookup))
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-python)
