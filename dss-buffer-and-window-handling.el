(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)

(require 'window-numbering)
(window-numbering-mode 1)
;; (setq window-numbering-assign-func
;;       (lambda () (when (equal (buffer-name) "*Calculator*") 9)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(when (fboundp 'winner-mode)
      (winner-mode 1))


; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun dss/toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(defun dss/sync-point-all-windows ()
  (interactive)
  (mapc '(lambda (f)
           (mapc '(lambda (w)
                    (if (eq (window-buffer w)
                            (current-buffer))
                        (set-window-point w (point))))
                 (window-list f)))
        (frame-list)))

(defun dss/frame-by-name (name)
  (car (delq nil
             (mapcar (lambda (fr) (if (string= (frame-parameter fr 'name) name) fr))
                     (frame-list)))))

(defun dss/window-list ()
  (interactive)
  (mapc #'(lambda (f)
           (insert (format "%S\n" f))
           (let ((selectedw (frame-selected-window f)))
             (mapc '(lambda (w)
                      (if (eq w selectedw)
                          (insert "*"))
                      (insert (format "  %S" w))
                      (insert (format "  %s" (buffer-name (window-buffer w))))
                      (insert (format ":%S\n" (window-point w))))
                   (window-list f))))
        (frame-list)))

;;; get-buffer-window-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dss/kill-clean-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (and buf (not (buffer-modified-p buf))
         (kill-buffer buf))))

(defun dss/kill-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (kill-buffer buf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-buffer-and-window-handling)
