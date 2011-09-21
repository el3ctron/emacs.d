(require 'dss-elisp-funcs)
(require 'dss-term)

(defun dss/gentoo-eix-choice (prefix)
  (interactive "sPattern:")
  (let* ((eix-list
          (split-string
           (dss/local-shell-command-to-string
            (concat
             "eix --only-names '" prefix "'"))))
         (choice (ido-completing-read "Which package: " eix-list)))
    choice))

(defun dss/gentoo-flagedit (pat)
  (interactive "sPattern:")
  (let ((choice (dss/gentoo-eix-choice pat)))
    (dss/remote-term "root@localhost" (concat "flaggie " choice " +~amd64"))))

(defun dss/gentoo-emerge (pat)
  (interactive "sPattern:")
  (let ((choice (dss/gentoo-eix-choice pat)))
    (dss/remote-term "root@localhost" (concat "emerge -avuDN " choice))))

(defun dss/gentoo-eix-sync ()
  (interactive)
  (dss/remote-term "root@localhost" "eix-sync\n"))

(defun dss/gentoo-emerge-world ()
  (interactive)
  (dss/remote-term "root@localhost" "emerge -avuDN world\n"))

(defun dss/gentoo-emerge-system ()
  (interactive)
  (dss/remote-term "root@localhost" "emerge -avuDN system\n"))

(defun dss/gentoo-fix-system-time ()
  (interactive)
  (dss/remote-term
   "root@localhost"
   "date; /etc/init.d/ntpd stop; ntpdate tavismac; /etc/init.d/ntpd start;  date\nsleep 3 && exit\n"))

(provide 'dss-gentoo)
