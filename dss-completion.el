(eval-when-compile (require 'cl))

(setq-default abbrev-mode t)
(setq save-abbrevs t)
(setq abbrev-file-name (concat dss-ephemeral-dir "abbrev_defs"))
(if (file-exists-p abbrev-file-name)
    (read-abbrev-file abbrev-file-name t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
(require 'ido)
(ido-mode t) ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)

;; from http://emacs-fu.blogspot.com/2009_02_01_archive.html
(setq
  ido-ignore-buffers  '("\\` "  "^\*Back" ".*Completions\*" "^\*Ido" "^\*trace"
                        "^\*compilation" "^\*GTAGS" "^session\.*")
  ;ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point t
  ido-use-url-at-point nil
  ;ido-max-prospects 8              ; don't spam my minibuffer
  ;ido-confirm-unique-completion t ; wait for RET, even with unique completion
  ido-save-directory-list-file (concat dss-ephemeral-dir "ido.last")
  )



;;; http://stackoverflow.com/questions/905338/can-i-use-ido-completing-read-instead-of-completing-read-everywhere
(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:

\(defadvice foo (around original-completing-read-only activate)
  (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                               allcomp
                               nil require-match initial-input hist def))
        ad-do-it))) " ")

;; also see dss/load-rope-completion in dss-python.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dabbrev and hippie-expand

(setq-default dabbrev-case-replace nil)
(setq-default hippie-expand-try-functions-list '(try-expand-abbrev try-expand-dabbrev-visible try-expand-dabbrev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete mode

;; new version of auto-complete.el
(add-to-list 'load-path (concat dss-vendor-dir "auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat dss-dotfiles-dir "ac-dict"))
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.2)
(setq ac-menu-height 20)
(setq ac-use-comphist nil)
(setq ac-candidate-limit 25)
(ac-config-default)


(defvar dss-ropemacs-completions-cache nil)
(defun dss/rope-candidates (prefix)
  (with-no-warnings
    (setq dss-ropemacs-completions-cache
          (delete ""
            (mapcar
                (lambda (completion)
                  (concat ac-prefix completion))
                (ignore-errors
                  (rope-completions))))))
  dss-ropemacs-completions-cache)

(ac-define-source dss-rope
  '((candidates . (dss/rope-candidates ac-prefix))
    (requires . 0)
    (cache . t)
    (symbol . "f")))

(ac-define-source dss-rope-dot
  '((candidates . (dss/rope-candidates ac-prefix))
    (prefix . c-dot)
    (requires . 0)
    (symbol . "f")))

(defvar ac-source-dss-filename '((init setq ac-filename-cache nil)
 (candidates . ac-filename-candidate)
 (requires . 0)
 (action . ac-start)
 (prefix . file)
 (limit)))

(defun dss/load-rope-completion()
  (interactive)
  (setq ac-sources (list
                    ac-source-dss-rope-dot
                    ac-source-yasnippet
                    ac-source-dss-rope
                    ac-source-words-in-buffer
                    ;;ac-source-filename
                    ac-source-dss-filename
                    ;;ac-source-words-in-same-mode-buffers
                    ;;ac-source-dictionary
                    )))


(defun dss/unload-rope-completion()
  (interactive)
  (setq ac-sources (list
                    ac-source-yasnippet
                    ac-source-words-in-buffer
                    ac-source-dss-filename
                    ;ac-source-words-in-same-mode-buffers
                    ac-source-dictionary
                    )))


(defun dss-ipython-completion-candidate (&optional use-ido)
  "Try to complete the python symbol before point. Only knows about the stuff
in the current *Python* session."
  (let* ((ugly-return nil)
         (sep ";")
         (python-process (or (get-buffer-process (current-buffer))
                                        ;XXX hack for .py buffers
                             (get-process py-which-bufname)))
         ;; XXX currently we go backwards to find the beginning of an
         ;; expression part; a more powerful approach in the future might be
         ;; to let ipython have the complete line, so that context can be used
         ;; to do things like filename completion etc.

         (beg (save-excursion (skip-chars-backward "a-z0-9A-Z_." (point-at-bol))
                               (point)))
         (end (point))
         (pattern (buffer-substring-no-properties beg end))

         (completions nil)
         (completion nil)
         (comint-preoutput-filter-functions
          (append comint-preoutput-filter-functions
                  '(ansi-color-filter-apply
                    (lambda (string)
                      (setq ugly-return (concat ugly-return string))
                      "")))))

    (message pattern)
    (process-send-string python-process
                         (format ipython-completion-command-string pattern))
    (accept-process-output python-process)
    (setq completions
          (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))

    (setq completions (if (string-match "\\." pattern)
                          (mapcar
                             (lambda (completion)
                               (car (last (cdr (split-string completion "\\.")))))
                             completions)
                        completions))
    (if use-ido
        (let* ((prefix-beg (if (string-match "\\." pattern)
                               (save-excursion (skip-chars-backward "a-z0-9A-Z_" (point-at-bol))
                                               (point))
                             beg))
               (prefix (buffer-substring-no-properties prefix-beg end))
               (choice (if (<= (length completions) 1)
                           (car completions)
                         (ido-completing-read "Choice:" completions nil nil prefix nil prefix)))
               )
          (if (and choice (not (string= pattern choice)))
              (progn
                (message "%s %s %s %s" prefix prefix-beg beg (point-at-bol))
                (delete-region prefix-beg end)
                (insert choice))))
      (progn
        ;(message "not using ido")
        completions))))


(defun dss/ido-ipython-complete ()
  (interactive)
  (dss-ipython-completion-candidate t))

(ac-define-source dss-ipy
  '((candidates . dss-ipython-completion-candidate)
    (requires . 0)
    (symbol . "f")))

(ac-define-source dss-ipy-dot
  '((candidates . dss-ipython-completion-candidate)
    (prefix . c-dot)
    (requires . 0)
    (symbol . "f")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dss/in-string-p ()
  (eq 'string (syntax-ppss-context (syntax-ppss))))

(defun dss/beginning-of-string ()
  "Go to beginning of string around point.
Do nothing if not in string."
  ;; from loveshack's python-beginning-of-string
  (interactive)
  (let ((state (syntax-ppss)))
    (when (eq 'string (syntax-ppss-context state))
      (goto-char (nth 8 state)))))

(defun dss/electric-pair ()
  "If at end of line, insert character pair without surrounding spaces.
   Otherwise, just insert the typed character."
  (interactive)
  ;(if (eolp) (let (parens-require-spaces) (insert-pair))
  ;  (self-insert-command 1)))
  (if (dss/in-string-p)
      (self-insert-command 1)
    (let (parens-require-spaces)
      (insert-pair))))

(defun dss/ac-electric-pair ()
  (interactive)
  (ac-complete)
  (dss/electric-pair))
(define-key ac-completing-map "(" 'dss/ac-electric-pair)


;http://www.rlazo.org/blog/entry/2008/sep/13/insert-a-path-into-the-current-buffer/
(defun dss/insert-path (file)
 "insert file"
 (interactive "FPath: ")
 (insert (expand-file-name file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-completion)
