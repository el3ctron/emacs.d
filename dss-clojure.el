;;; setting up clojure/slime http://technomancy.us/126
;;; requires elpa

(defun dss/init-clojure ()
  (interactive)
  (dss/init-elpa)
  (clojure-slime-config))

(provide 'dss-clojure)
