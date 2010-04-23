(defun vc-ediff ()
  (interactive)
  (require 'ediff)
  (require 'vc)
  (vc-buffer-sync)
  (ediff-load-version-control)
  (setq ediff-split-window-function 'split-window-horizontally)
  (ediff-vc-internal "" ""))

(if (file-exists-p (concat dss-vendor-dir "dvc/++build/dvc-load.el"))
    (load-file (concat dss-vendor-dir "dvc/++build/dvc-load.el")))

(provide 'dss-vc)
