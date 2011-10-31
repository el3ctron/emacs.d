(require 'slime)
(slime-setup '(slime-repl))
(setq slime-protocol-version 'ignore)

(add-to-list 'load-path (concat dss-vendor-dir "ac-slime"))
(require 'ac-slime)
(defun dss/load-slime-completion()
  (interactive)
  (setq ac-sources (list
                    ac-source-slime-simple
                    ;;ac-source-slime-fuzzy
                    ac-source-words-in-buffer
                    ;;ac-source-filename
                    ac-source-dss-filename
                    ;;ac-source-words-in-same-mode-buffers
                    ;;ac-source-dictionary
                    )))

;;; http://common-lisp.net/project/slime/doc/html/Setting-up-pathname-translations.html
;; (add-hook 'slime-connected-hook
;;           (lambda ()
;;             (push (list ".*"
;;                         (lambda (filename)
;;                           filename)
;;                         (lambda (filename)
;;                           filename))
;;                   slime-filename-translations)
;;             (push (list "li47-21"
;;                         (lambda (filename)
;;                           (subseq filename (length "/ssh:hunchentoot@sample.ponto-dot.com#5201:")))
;;                         (lambda (filename)
;;                           (concat "/ssh:hunchentoot@sample.ponto-dot.com#5201:" filename)))
;;                   slime-filename-translations)))
;;; /scpc:sociallinks:


(defun slime-tramp-local-filename (f)
  (interactive)
  (if (file-remote-p f)
      (tramp-file-name-localname
       (tramp-dissect-file-name f))
    f))

(defun slime-tramp-remote-filename (f)
  (interactive)
  (if (file-remote-p default-directory)
      (tramp-make-tramp-file-name
       (tramp-file-name-method
    (tramp-dissect-file-name default-directory))
       (tramp-file-name-user
    (tramp-dissect-file-name default-directory))
       (tramp-file-name-host
    (tramp-dissect-file-name default-directory))
       f)
    f))

(defun slime-remote-file-name-hook ()
  (interactive)
    (setq slime-from-lisp-filename-function
      'slime-tramp-remote-filename)
    (setq slime-to-lisp-filename-function
      'slime-tramp-local-filename))

(add-hook 'slime-connected-hook 'slime-remote-file-name-hook)

(defun dss/slime-hook ()
  (paredit-mode 1)
  (dss/load-slime-completion))

(add-hook 'slime-mode-hook 'dss/slime-hook)
(provide 'dss-slime)


;;; the following code is just me playing around
;; (let ((string "(find-namespaces-on-classpath)"))
;;   (slime-eval-async `(swank:eval-and-grab-output ,string)
;;     (lambda (result)
;;       (destructuring-bind (output value) result
;;         (insert output value)))))

;; (defun dss/slime-eval (string)
;;   (interactive)
;;   (slime-eval `(swank:eval-and-grab-output ,string)))

;; (let ((string "(find-namespaces-on-classpath)"))
;;   (dss/slime-eval string)
;;   )
;; (setq foobar (dss/slime-eval "1234"))
;; foobar
;; (slime-interactive-eval )
