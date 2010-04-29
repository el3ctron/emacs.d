;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmarks & registers
(require 'bookmark+)
(defun dss/bookmark-jump (bookmark)
  (interactive
   (progn
     (require 'bookmark)
     (bookmark-maybe-load-default-file)
     (list (ido-completing-read "Jump to bookmark: "
                                (mapcar 'car bookmark-alist)))))
  (bookmark-jump bookmark))

; study http://emacs-fu.blogspot.com/2009/01/using-registers.html
(require 'list-register)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-bookmarks-registers)
