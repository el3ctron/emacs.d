(require 'dss-paths)
(require 'ediff)
(require 'ediff-vers)
(require 'vc)

(setq dvc-tips-enabled nil)
(setq vc-follow-symlinks t)

(defun vc-ediff ()
  (interactive)
  (vc-buffer-sync)
  (ediff-load-version-control)
  (setq ediff-split-window-function 'split-window-horizontally)
  (ediff-vc-internal "" ""))

(provide 'dss-vc)
