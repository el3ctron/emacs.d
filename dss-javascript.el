(require 'dss-paths)
(require 'cc-vars)
(require 'dss-codenav-helpers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yegge's js2-mode with better indentation support

(add-to-list 'load-path (concat dss-vendor-dir "js2-mode"))
(require 'js2-mode)

(defun dss/js-electric-pair ()
  ;; this version doesn't check to see if we're inside of a string or comment
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun dss/js2-indent-function ()
  "This is just a copy of http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode"
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(require 'flymake-jslint)
(defun dss/js2-mode-hook ()
  (require 'espresso)
  (dss/lintnode -1)
  (flymake-jslint-init)
  (flymake-mode)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-newline 0)
  (c-toggle-hungry-state 1)
  (setq js2-indent-on-enter-key t)
  (setq js2-enter-indents-newline t)
  (set (make-local-variable 'indent-line-function) 'dss/js2-indent-function)
  (define-key js2-mode-map [(meta control "|")] 'cperl-lineup)
  (define-key js2-mode-map [(meta control "\;")]
    (lambda()
      (interactive)
      (insert "/* -----[ ")
      (save-excursion
        (insert " ]----- */"))
      ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'dss/indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (mapc (lambda (char)
          (progn
            (define-key js2-mode-map char 'dss/js-electric-pair)))
        '("\"" "'" "(" "[" "{")))

(add-hook 'js2-mode-hook 'dss/js2-mode-hook)
(add-hook 'js2-mode-hook 'dss/install-whitespace-cleanup-hook)
(add-hook 'js2-mode-hook '(lambda () (linum-mode t)))

(defun dss/jslint-ignore ()
  (interactive)
  (save-excursion
    (call-interactively 'comment-dwim)
    (end-of-line)
    (insert " jslint-ignore")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alternately espresso-mode instead of js2-mode

(autoload 'espresso-mode "espresso" "espresso-mode" t)
(add-hook 'espresso-mode-hook 'run-coding-hook)
(setq espresso-indent-level 4)

(eval-after-load 'espresso
  '(progn (define-key espresso-mode-map "{" 'paredit-open-curly)
          (define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
          ;; fixes problem with pretty function font-lock
          (define-key espresso-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'espresso-mode `(("\\(function *\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;; or
;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'coffee-mode)
(setq coffee-js-mode 'js2-mode)

(defun dss/coffee-compile-buffer ()
  "Compiles the current buffer and displays the JS in another buffer."
  (interactive)
  (save-excursion
    (save-window-excursion
      (dss/coffee-compile-region (point-min) (point-max)))))

(defun dss/coffee-compile-region (start end)
  "Compiles a region and displays the JS in another buffer."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      ;; (kill-buffer buffer)
      (with-current-buffer buffer
        (delete-region (point-min) (point-max)))))

  (call-process-region start end coffee-command nil
                       (get-buffer-create coffee-compiled-buffer-name)
                       nil
                       "-s" "-p" "--bare")
  (switch-to-buffer (get-buffer coffee-compiled-buffer-name))
  (funcall coffee-js-mode)
  (goto-char (point-min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dss-javascript)
