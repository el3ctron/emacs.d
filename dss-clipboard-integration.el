(require 'browse-kill-ring)

(setq x-select-enable-clipboard t)

;; Ideas and code stolen from
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html

(defun dss-xsel-cut-function (text &optional push)
  ;; Insert text to temp-buffer, and "send" content to xsel stdin
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--input" "-p" "--clipboard")))

(defun dss-xsel-paste-function()
  ;; Find out what is current selection by xsel. If it is different
  ;; from the top of the kill-ring (car kill-ring), then return
  ;; it. Else, nil is returned, so whatever is in the top of the
  ;; kill-ring will be used.
  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output)))

(defun dss/tty-x-clipboard-init ()
  (interactive)
  (unless window-system
    (unless (getenv "DISPLAY")
      (setenv "DISPLAY" ":0"))
    (setq interprogram-cut-function 'dss-xsel-cut-function)
    (setq interprogram-paste-function 'dss-xsel-paste-function)))

(defun dss/tty-x-clipboard-disable ()
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil)
  )


;; also see http://stackoverflow.com/questions/994563/integrate-readlines-kill-ring-and-the-x11-clipboard
;; http://bbs.archlinux.org/viewtopic.php?id=80226
;; and run the following
;; #autocutsel -fork &
;; #autocutsel -selection PRIMARY -fork &

(provide 'dss-clipboard-integration)
