(require 'imenu)
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list.
Comes from http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-defuns.el"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

;; ; http://nflath.com/2009/07/imenu/
;; (require 'imenu)
;; (setq imenu-auto-rescan t)
;; (defun ido-goto-symbol ()
;;   "Will update the imenu index and then use ido to select a symbol to navigate to"
;;   (interactive)
;;   (imenu--make-index-alist)
;;   (let ((name-and-pos '())
;;         (symbol-names '()))
;;     (flet ((addsymbols (symbol-list)
;;                        (when (listp symbol-list)
;;                          (dolist (symbol symbol-list)
;;                            (let ((name nil) (position nil))
;;                              (cond
;;                               ((and (listp symbol) (imenu--subalist-p symbol))
;;                                (addsymbols symbol))
;;                               ((listp symbol)
;;                                (setq name (car symbol))
;;                                (setq position (cdr symbol)))
;;                               ((stringp symbol)
;;                                (setq name symbol)
;;                                (setq position (get-text-property 1 'org-imenu-marker symbol))))
;;                              (unless (or (null position) (null name))
;;                                (add-to-list 'symbol-names name)
;;                                (add-to-list 'name-and-pos (cons name position))))))))
;;       (addsymbols imenu--index-alist))
;;     (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
;;               (position (cdr (assoc selected-symbol name-and-pos))))
;;       (if (markerp position)
;;             (goto-char position) (goto-char (overlay-start position))))))


(defvar  dss/major-column-face 'dss/major-column-face
  "major column grid marker")
(defface dss/major-column-face '((t (:background "#484848")))
  "major column grid marker"
  :group 'faces)

(defvar  dss/minor-column-face 'dss/minor-column-face
  "minor column grid marker")
(defface dss/minor-column-face '((t (:background "#2c2c2c")))
  "minor column grid marker"
  :group 'faces)

(column-marker-create dss/column-marker-1 dss/minor-column-face)
(column-marker-create dss/column-marker-2 dss/major-column-face)
(column-marker-create dss/column-marker-3 dss/minor-column-face)
(column-marker-create dss/column-marker-4 dss/major-column-face)
(column-marker-create dss/column-marker-5 dss/minor-column-face)
(column-marker-create dss/column-marker-6 dss/major-column-face)
(column-marker-create dss/column-marker-7 dss/minor-column-face)
(column-marker-create dss/column-marker-8 dss/major-column-face)

(defun dss/column-grid ()
  (interactive)
  ;; col-highlight-face
  (hl-line-mode -1)
  (dss/column-marker-1 10)
  (dss/column-marker-2 20)
  (dss/column-marker-3 30)
  (dss/column-marker-4 40)
  (dss/column-marker-5 50)
  (dss/column-marker-6 60)
  (dss/column-marker-7 70)
  (dss/column-marker-8 80))

(defun dss/column-grid-off ()
  (interactive)
  (dss/column-marker-1 -1)
  (hl-line-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-codenav-helpers)
