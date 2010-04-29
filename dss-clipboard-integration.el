(setq x-select-enable-clipboard t)
;;
;;   ;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;;   ;; "I prefer using the "clipboard" selection (the one the
;;   ;; typically is used by c-c/c-v) before the primary selection
;;   ;; (that uses mouse-select/middle-button-click)
;;   ;; If emacs is run in a terminal, the clipboard- functions have no
;;   ;; effect. Instead, we use of xsel"
;;

(defun dss/tty-x-clipboard-init ()
  (interactive)
  (unless window-system
    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--input" "-p" "--clipboard")))
    (defun xsel-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    ;; Idea from
    ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
    ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
    ))

;; also see http://stackoverflow.com/questions/994563/integrate-readlines-kill-ring-and-the-x11-clipboard
;; http://bbs.archlinux.org/viewtopic.php?id=80226
;; and run the following
;; #autocutsel -fork &
;; #autocutsel -selection PRIMARY -fork &

(provide 'dss-clipboard-integration)
