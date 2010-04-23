;;; Installed via gentoo / portage
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")

(if (file-exists-p "/usr/share/emacs/etc/ess")
    (condition-case nil
        (require 'ess-site)
      (message ("Error while loading ess"))))

(provide 'dss-ess)
