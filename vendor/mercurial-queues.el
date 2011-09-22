;;; mercurial-queues.el --- commands for working with Mercurial patch queues

;; Copyright (C) 2009 Jim Blandy

;; Author: Jim Blandy <jimb@red-bean.com>
;; Version: 0.1

;; mercurial-queues.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.
;;
;; mercurial-queues.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides commands for working with Mercurial patch
;; queues, described here:
;;
;; http://mercurial.selenic.com/wiki/MqExtension
;; http://hgbook.red-bean.com/read/managing-change-with-mercurial-queues.html
;;
;; Briefly, a patch queue is a sequence of patches meant to apply to
;; some source tree under Mercurial's control. The 'hg qpush' command
;; applies the next patch in the series; 'hg qpop' un-applies the last
;; applied patch; and 'hg qrefresh' integrates any changes you've made
;; to the working files into the top patch.
;; 
;; Patch queues are useful for maintaining a set of patches you intend
;; to submit for review: instead of merging others' changes into your
;; latest sources, you pop all your patches, update, and then re-push,
;; adjusting and refreshing the patches as appropriate. This way, the
;; patches stay applicable to the latest sources, ready for review.
;;
;; This package provides:
;;
;; - Emacs commands for pushing, refreshing, and popping patches,
;;   creating new patches, deleting patches, and incorporating patches
;;   into the ordinary Mercurial history, and
;;
;; - An Emacs mode for viewing and editing the patch series, showing
;;   which patches are currently applied.
;;
;; Installation
;; ============
;;
;; To use this package, place this file in a directory listed in your
;; load-path, and then put the following in your .emacs file:
;;
;;   (require 'mercurial-queues)
;;   (add-to-list 'auto-mode-alist '(".hg/patches/series$" . mq-series-mode))
;;
;; Usage
;; =====
;; With this package loaded, the following commands are available:
;;
;;   C-x q n --- Apply the next patch in the series (as by 'hg qpush').
;;   C-x q p --- Un-apply the last applied patch (as by 'hg qpop').
;;   C-x q r --- Incorporate the current changes to the working files into the
;;               top patch (as by 'hg qrefresh').
;;   C-x q s --- Visit the series file in a Series Mode buffer. Series Mode
;;               shows which patches are currently applied, and provides
;;               commands to push/pop up to a given patch, visit patches, and
;;               so on. You can edit the seris with the normal Emacs editing
;;               commands. Visit the Series Mode documentation with `C-h m
;;               series-mode RET' for details.
;;   C-x 4 q s --- Visit the series file in another window.
;;
;; This package takes over the global binding of `C-x q' as a prefix
;; for its commands. This is normally bound to `kbd-macro-query', but
;; that command is also available as `C-x C-k q', under the common
;; `C-x C-k' prefix for all macro-related commands.
;;
;; Ideas
;; =====
;;
;; qdelete
;; mark pushed patches as read-only
;;
;; font-locking highlighting for series file comments.
;;
;; When pushing a patch produces a conflict, it would be nice to
;; gather up the .rej files into a diff-mode buffer. Users could use
;; the diff-mode commands to visit the erstwhile context, etc.

(require 'cl)


;;; Customizable things.

(defface mq-applied-patch '((t :weight bold))
  "Face for applied patches in a Mercurial Queues `series' file.")

(defface mq-condition-positive-selected
  '((((class color)) :foreground "#00c011"))
  "Face for positive guard conditions (#+) whose guards are selected.")

(defface mq-condition-negative-selected
  '((((class color)) :foreground "#b60000"))
  "Face for negative guard conditions (#+) whose guards are selected.")


;;; Utility functions.

(defun mq-hg-root-directory ()
  "Return the root of the Mercurial tree containing the currently visited file.
This is the directory containing the `.hg' subdirectory.
If the current buffer's default directory is `ROOT/.hg/patches',
then we return `ROOT', even if there is a `ROOT/.hg/patches/.hg'
directory."
  (let ((dir default-directory))
    (unless dir
      (error "Selected buffer has no default directory"))
    (setq dir (expand-file-name dir))

    ;; It can be useful for the ROOT/.hg/patches directory itself to be a
    ;; Mercurial root, with its own metadata in ROOT/.hg/patches/.hg. Calling
    ;; this function in such a directory should arguably return
    ;; ROOT/.hg/patches.
    ;; 
    ;; However, it doesn't seem very useful to have queues of patches to
    ;; patches (metaqueues!), and it does seem useful for the global series
    ;; commands --- mq-visit-series, etc. --- to operate using ROOT when
    ;; invoked in ROOT/.hg/patches, regardless of whether that directory is
    ;; itself a root. So we do that.
    (if (string-match "\\`\\(.*/\\)\\.hg/patches/\\'" dir)
        (match-string 1 dir)
      (while (not (file-directory-p (expand-file-name ".hg" dir)))
        (let ((parent (file-name-directory (directory-file-name dir))))
          ;; We've reached the root when the above hands us back the
          ;; same thing we gave it.
          (when (string-equal dir parent)
            (error "directory not managed by mercurial: %s" default-directory))
          (setq dir parent)))
      dir)))

(defun mq-patch-directory-name (root)
  "Return the patch directory for the Mercurial root directory ROOT."
  (file-name-as-directory (expand-file-name ".hg/patches" root)))

(defun mq-root-for-patch-directory (dir)
  "If DIR is a patch directory, return its root.  Otherwise, return nil."
  (when (string-match "\\`\\(.*/\\)\\.hg/patches/\\'" dir)
    (match-string 1 dir)))

(defun mq-metadata-file-name (root name)
  "Return the name of the MQ metadata file NAME for the Mercurial tree ROOT."
  (expand-file-name name (mq-patch-directory-name root)))

(defun mq-series-file-name (root) (mq-metadata-file-name root "series"))
(defun mq-status-file-name (root) (mq-metadata-file-name root "status"))
(defun mq-guards-file-name (root) (mq-metadata-file-name root "guards"))

(defun mq-check-for-queue ()
  "Raise an error if the current directory has no associated patch queue."
  (let* ((root (mq-hg-root-directory))
         (patch-directory (mq-patch-directory-name root))
         (series-file (mq-series-file-name root)))
    (unless (file-directory-p patch-directory)
      (error "Mercurial tree has no patch directory: %s" root))
    (unless (file-exists-p series-file)
      (error "Mercurial tree has no series file: %s" root))))

(defun mq-parse-status-file (filename)
  "Parse the contents of the MQ status file named FILENAME.
The return value is a list of the applied patches, from last to earliest.
If there is no status file, this returns nil."
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (let (patches)
          (while (< (point) (point-max))
            (unless (looking-at "[0-9a-f]*:\\(.*\\)$")
              (error "unrecognized line in MQ status file"))
            (push (match-string 1) patches)
            (forward-line 1))
          patches))
    nil))

(defun mq-parse-guards-file (filename)
  "Parse the contents of the MQ guards file.
The return value is a list of the selected guards, as symbols.
If there is no 'guards' file, return nil."
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (let (guards)
          (while (< (point) (point-max))
            (let ((start (point)))
              (end-of-line)
              (let ((guard (intern (buffer-substring start (point)))))
                (push guard guards)
                (forward-line 1))))
          guards))
    nil))

(defun mq-parse-guard-conditions ()
  "Parse the current patch's guard conditions.
This assumes point is on the patch's line in the series file.
The return value is a list (ANYPOS TABLE), where:
- ANYPOS is true (non-nil) if there are any positive guard conditions, and
- TABLE is a hash table mapping guard names (as interned symbols)
  to the symbols `+' (for positive guards) or `-' (for negative
  guards).
For example, parsing the line:

   printer-param.patch                     #+foo-bar -baz -quux

yields `(t TABLE)', where TABLE maps `foo-bar' to `+', and `baz' and
`quux' to `-'."
  (let (any-positive
        (table (make-hash-table :test (function eq))))
    (save-excursion
      (end-of-line)
      (let ((end (point)))
        (beginning-of-line)
        (while (re-search-forward "#\\([-+]\\)\\([^#[:space:]]+\\)" end t)
          (let ((state (intern (match-string 1)))
                (guard (intern (match-string 2))))
            (puthash guard state table)
            (when (eq state '+)
              (setq any-positive t))))))
    (list any-positive table)))

(defun mq-patch-enabled-p (guards conditions)
  "Return true if GUARDS satisfies CONDITIONS.
GUARDS is a list of the currently selected guards, as interned symbols.
CONDITIONS is a (ANYPOS . TABLE) list representing a set of guard
conditions, of the type returned by mq-parse-guard-conditions.

Mercurial seems to enable a patch if:
- it has no negative conditions whose guards are selected, and
- it has either:
  - no positive guard conditions, or
  - one of the positive conditions' guards is selected."
  (let ((anypos (car conditions))
        (table (cadr conditions))
        found-negative
        found-positive)
    (mapcar (lambda (g) (case (gethash g table)
                          ((+) (setq found-positive t))
                          ((-) (setq found-negative t))))
            guards)
    (and (not found-negative)
         (or (not anypos) found-positive))))

(defun mq-all-condition-guards ()
  "Return a list of all guards mentioned in the current buffer's conditions.
This scans the current buffer, presumed to be a series file, for
guard conditions, and returns a list of all the guards mentioned,
as symbols."
  (let ((guards (make-hash-table :test (function eq))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#[-+]\\([^#[:space:]]+\\)" nil t)
        (let ((guard (intern (match-string 1))))
          (puthash guard t guards))))
    (let (guard-list)
      (maphash (lambda (g v) (push g guard-list))
               guards)
      guard-list)))

(defun mq-parse-series-line ()
  "Parse the current series file line, returning the patch name and conditions.
The return value is nil if the current line contains no patch, or 

 a list of the form (NAME CONDITIONS) where
NAME is the name of the patch file, and CONDITIONS represents the
guard conditions for enabling the patch. CONDITIONS is a value of
the sort returned by mq-parse-guard-conditions."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[[:blank:]]*\\([^#[:space:]]+\\)")
        (list (match-string 1) (mq-parse-guard-conditions)))))

(defun mq-shell-command (string &rest args)
  "Display and run the shell command COMMAND, in an appropriate directory.
If the current default-directory is `ROOT/.hg/patches', then run
the command with ROOT as its default directory. Otherwise, use
default-directory unchanged."
  (let ((command (apply (function format) string args)))
    (message "Running command: %s" command)
    (let ((default-directory
            (or (mq-root-for-patch-directory default-directory)
                default-directory)))
      (shell-command command))))

(defun mq-top-patch-name (root)
  "Return the name of the top patch, or nil if none are pushed."
  (car (mq-parse-status-file (mq-status-file-name root))))

(defun mq-next-patch-name (root)
  "Return the name of the next patch to be applied, or nil if all are pushed."
  ;; If calling 'hg qnext' becomes a pain, we could probably emulate
  ;; this in lisp, as we do have the code to parse series lines and
  ;; evaluate guard conditions.
  (let ((output 
         ;; hg qnext is unhelpful when run in a patches directory with its
         ;; own .hg subdirectory.
         (let ((default-directory root))
           (shell-command-to-string "hg qnext"))))

    ;; Omit any trailing newline.
    (if (string-equal (substring output -1) "\n")
        (setq output (substring output 0 -1)))
    
    ;; This is not great.
    (if (string-equal output "all patches applied")
        nil
      ;; Check that the output is a filename; otherwise, assume it's
      ;; an error message.
      (let ((patch-directory (mq-patch-directory-name root)))
        (unless (file-exists-p (expand-file-name output patch-directory))
          (error "hg qnext: %s" output))
        output))))

(defun mq-suggest-unapplied-patch (root)
  "Choose a helpful default for prompts for the name of an unapplied patch.
ROOT is the repository root of whose series the patch should be
an unapplied member. We return the patch's file name relative to
the patches directory.
If the current buffer is a series-mode buffer, return the patch
on the line containing point, if that patch is unapplied.
If the current buffer is a diff-mode buffer visiting an unapplied
patch in the patches directory, return its name.
Otherwise, return nil."
  ;; I thought it would be nice to return the first unapplied patch if
  ;; neither of the above yield results, but 1) that might be slow,
  ;; and 2) if there isn't a likely choice, I'd rather not put
  ;; something in the minibuffer that people have to delete.
  (let (status
        (patch
         (or 
          ;; If we're in a series-mode buffer, try to parse the
          ;; current line.
          (and (eq major-mode 'mq-series-mode)
               (let ((line (mq-parse-series-line)))
                 ;; Use the status data we've already collected.
                 (setq status mq-status)
                 (car line)))
          ;; If we're in a diff-mode buffer, visiting a patch in our
          ;; patches directory, then use the buffer name.
          (and (eq major-mode 'diff-mode)
               buffer-file-name
               (let ((patch-directory (mq-patch-directory-name root)))
                 (when (string-equal patch-directory
                                     (file-name-directory buffer-file-name))
                   (file-name-nondirectory buffer-file-name)))))))
    (unless status
      (setq status (mq-parse-status-file (mq-status-file-name root))))
    (unless (member patch status)
      patch)))


;;; Series Mode, for editing series files.

;; Buffer-local variables.
(defvar mq-patches-directory nil
  "Name of the patches directory for this series buffer.")

(defvar mq-status-file-name nil
  "Name of the `status' file for this series buffer.")

(defvar mq-guards-file-name nil
  "Name of the `guards' file for this series buffer.")

(defvar mq-status nil
  "A list of the patches currently applied for this series buffer.
More recently pushed patches appear earlier in the list.")

(defvar mq-guards nil
  "The selected guards for this series buffer's queue.
This is a list of symbols named after the guards.")

(defvar mq-font-lock-keywords nil
  "A list of computed regular expressions for font-lock mode to highlight.
These include selected guards, applied patches, and so on.
The value of this variable changes as the user pushes and pops patches,
changes the set of selected guards, and so forth.  We abuse font-lock mode
a little bit to make this work.")

(defun mq-compute-font-lock ()
  "Compute the current buffer's font lock keywords, based on status and guards."
  (let (keywords)
    (mapc (lambda (patch)
            (let ((regexp (format "^\\s-*%s[# \t\n]" (regexp-quote patch))))
              (push `(,regexp . 'mq-applied-patch)
                    keywords)))
          mq-status)
    (mapc (lambda (guard)
            (let* ((guard-regexp (regexp-quote (symbol-name guard)))
                   (positive-regexp (concat "#\\+" guard-regexp))
                   (negative-regexp (concat "#-" guard-regexp)))
              (push `(,positive-regexp . 'mq-condition-positive-selected)
                    keywords)
              (push `(,negative-regexp . 'mq-condition-negative-selected)
                    keywords)))
          mq-guards)
    (setq mq-font-lock-keywords (nreverse keywords))
    ;; Tell font-lock mode to recompute its regexps.  This is kind of a kludge. 
    (set (make-local-variable 'font-lock-set-defaults) nil)
    (font-lock-fontify-buffer)))

(defun mq-refresh-status-and-guards ()
  "Reread the `status' and `guards' files for the current series buffer."
  (unless (eq major-mode 'mq-series-mode)
    (error "mq-refresh-status-and-guards should only run in a series buffer"))
  (setq mq-status (mq-parse-status-file mq-status-file-name))
  (setq mq-guards (mq-parse-guards-file mq-guards-file-name))
  (mq-compute-font-lock))

(defun mq-series-mode ()
  "A major mode for working with Mercurial Queues patch series files.
If the buffer visits a series file in the `.hg/patches' directory
of a Mercurial repository, then the buffer's default directory is
the top of that repository.

The following commands are available in all buffers:

  \\[mq-qpush]	Apply the next patch in the series (as by 'hg qpush').
  \\[mq-qpop]	Un-apply the top patch in the series (as by 'hg qpop').
  \\[mq-qrefresh]	Incorporate the current changes to the working files
		into the top patch (as by 'hg qrefresh').
  \\[mq-visit-top-patch]	Visit the top patch file.
  \\[mq-visit-top-patch-other-window]	Visit the top patch file in another window.
  \\[mq-visit-series]	Visit the series file relevant to the current buffer in
                a Series Mode buffer.
  \\[mq-visit-series-other-window]	As above, but visit the series file in another window.
  \\[mq-qnew]	Insert a new patch in the series, on top of the current patch.
  \\[mq-show-top-next]	Show the names of the top and next patches.
  \\[mq-qpop-all]	Un-apply all patches (as by 'hg qpop -a').
  \\[mq-qpush-all]	Apply all patches in the series (as by 'hg qpush -a').

Visiting a series file in Series Mode provides highlighting
showing the current state of the series, and special commands for
editing series files.

- Names of applied patches appear in bold (the `mq-applied-patch' face).

- Positive guard conditions (`#+foo') whose guards are selected
  appear in green (the `mq-condition-positive-selected' face).

- Negative guard conditions (`#-foo') whose guards are selected
  appear in red (the `mq-condition-negative-selected' face).

The following commands are available in the series file:
\\<mq-series-mode-map>
  \\[mq-go-to-patch]	Push or pop patches as necessary to make the patch on the
		current line the top patch.
  \\[mq-find-patch]	Visit the patch file on the current line.
  \\[mq-find-patch-other-window]	Visit the patch file on the current line in another window.
  \\[mq-qfold]	Incorporate the patch on the current line into the top patch.

Here is a complete list of the bindings available in Series Mode:

\\{mq-series-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mq-series-mode)
  (setq mode-name "Series")
  (use-local-map mq-series-mode-map)

  ;; The buffer-local variable mq-patches-directory is the directory
  ;; actually containing the series file, but for convenience we set
  ;; default-directory to the root of the repository.
  (set (make-local-variable 'mq-patches-directory) default-directory)
  (setq default-directory
        (or (mq-root-for-patch-directory default-directory)
            (mq-hg-root-directory)))

  (set (make-local-variable 'mq-status-file-name)
       (mq-status-file-name default-directory))
  (set (make-local-variable 'mq-guards-file-name)
       (mq-guards-file-name default-directory))
  (make-local-variable 'mq-status)
  (make-local-variable 'mq-guards)
  (make-local-variable 'mq-font-lock-keywords)
  (setq font-lock-defaults '(mq-font-lock-keywords))
  (mq-refresh-status-and-guards)

  (run-mode-hooks 'mq-series-mode-hook))

(defalias 'series-mode 'mq-series-mode)

(defvar mq-series-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'mq-go-to-patch)
    (define-key map "\C-c\C-f" 'mq-find-patch)
    (define-key map "\C-c\C-i" 'mq-qfold)
    (define-key map "\C-c4\C-f" 'mq-find-patch-other-window)
    map))

(defun mq-refresh-buffers (root)
  "Revert all unmodified buffers visiting changed files in ROOT.
Note that this also updates the contents of the `series' buffer, if it has
changed."
  (let ((root-regexp (concat "\\`" (regexp-quote root))))
    (loop for buffer in (buffer-list)
          do (save-excursion
               (set-buffer buffer)
               (if (and (not (buffer-modified-p))
                        buffer-file-name
                        (string-match root-regexp buffer-file-name)
                        (file-exists-p buffer-file-name)
                        (not (verify-visited-file-modtime (current-buffer))))
                   (revert-buffer t t))))))

(defun mq-refresh ()
  "Refresh Emacs's state after pushing or popping patches.
This reverts all unmodified buffers visiting files in the the
current mercurial tree, if the visited file seems to have changed."
  (let ((root (mq-hg-root-directory)))
    (mq-refresh-buffers root)
    ;; The above will catch changes to the status file. But if the
    ;; operation changed the status or guard files without changing
    ;; the series file, we need to catch those explicitly.
    (let* ((series (mq-series-file-name root))
           (buffer (get-file-buffer series)))
      (when buffer
        (save-excursion
          (set-buffer buffer)
          (mq-refresh-status-and-guards))))))

(defun mq-go-to-patch ()
  "Make the patch on the current line the top, by pushing or popping as needed."
  (interactive)
  (save-some-buffers)
  (let ((line (mq-parse-series-line)))
    (unless line
      (error "no patch name on current line"))
    (let ((patch (car line)))
      ;; If the patch is already the top one, say something helpful.
      (if (equal patch (car mq-status))
          (message "patch is already the top patch: %s" patch)
        ;; If the patch is currently applied, we assume we should pop.
        ;; Otherwise, we assume we should push.
        (let ((operation (if (member patch mq-status) "qpop" "qpush")))
          (mq-shell-command "hg %s '%s'" operation patch)
          (mq-refresh))))))

(defun mq-find-patch ()
  "Visit the patch on the current line."
  (interactive)
  (let* ((root (mq-hg-root-directory))
         (patch-directory (mq-patch-directory-name root))
         (line (mq-parse-series-line))
         (patch (car line)))
    (unless line
      (error "no patch name on current line"))
    (find-file (expand-file-name patch patch-directory))))

(defun mq-find-patch-other-window ()
  "Visit the patch on the current line in another window."
  (interactive)
  (let* ((root (mq-hg-root-directory))
         (patch-directory (mq-patch-directory-name root))
         (line (mq-parse-series-line))
         (patch (car line)))
    (unless line
      (error "no patch name on current line"))
    (find-file-other-window (expand-file-name patch patch-directory))))

(defun mq-point-to-patch (patch)
  "Move point to the series buffer line for PATCH.
If PATCH is nil, or there is no such patch, move to the top patch.
If there is no applied patch, move point to the top of the buffer"
  (goto-char (point-min))
  (cond
   ((and patch
         (re-search-forward (format "^\\s-*%s\\([#[:space:]]\\|\\'\\)"
                                    (regexp-quote patch))
                            nil t))
    (goto-char (match-beginning 0)))
   ((and mq-status
         (re-search-forward (format "^\\s-*%s\\([#[:space:]]\\|\\'\\)"
                                    (regexp-quote (car mq-status)))
                            nil t))
    (goto-char (match-beginning 0)))
   (t (goto-char (point-min)))))


;;; Global commands, available in all files.

(defun mq-push-pop-command (command force)
  "Subroutine for Mercurial Queues push/pop commands.
Check that the current buffer is visiting a file to which some
queue applies; run the command `hg COMMAND'; and refresh Emacs's
state (buffer contents, series file markup).
If FORCE is non-nil, pass the `--force' flag to command as well."
  (mq-check-for-queue)
  (save-some-buffers)
  (when force (setq command (concat command " --force")))
  (mq-shell-command command)
  (mq-refresh))

(defun mq-qpush ()
  "Apply the next patch in the current buffer's Mercurial Queues patch series."
  (interactive)
  (mq-push-pop-command "hg qpush" nil))

(defun mq-qpush-all ()
  "Apply all patches in the current buffer's Mercurial Queues patch series."
  (interactive)
  (mq-push-pop-command "hg qpush -a" nil))

(defun mq-qpop (force)
  "Un-apply the last applied patch in the current buffer's Mercurial patch series.
With a prefix argument, pass the '--force' flag, to pop even if there are
local changes."
  (interactive "P")
  (mq-push-pop-command "hg qpop" force))

(defun mq-qpop-all (force)
  "Un-apply all patches in the current buffer's Mercurial Queues patch series.
With a prefix argument, pass the '--force' flag, to pop even if there are
local changes."
  (interactive "P")
  (mq-push-pop-command "hg qpop -a" force))

(defun mq-qrefresh ()
  "Incorporate uncommitted changes into the top Mercurial Queues patch."
  (interactive)
  (mq-check-for-queue)
  (save-some-buffers)
  (mq-shell-command "hg qrefresh")
  ;; If we have a buffer visiting that patch, try to refresh it.
  (let* ((root (mq-hg-root-directory))
         (patch-directory (mq-patch-directory-name root))
         (status (mq-parse-status-file (mq-status-file-name root))))
    (unless status
      (error "No patch currently applied; cannot refresh."))
    (let* ((top (expand-file-name (mq-top-patch-name root) patch-directory))
           (buffer (get-file-buffer top)))
      (if buffer
          (save-excursion
            (set-buffer buffer)
            (if (and (not (buffer-modified-p))
                     (not (verify-visited-file-modtime (current-buffer))))
                (revert-buffer t t)))))))

(defun mq-qnew (name &optional force)
  "Insert a new patch into the current Mercurial Queues patch series.
The new patch follows the current top in the series, and is initially empty.
Normally, there must not be any uncommitted changes in the
working directory. With a prefix argument, create a new patch
anyway, incorporating all such changes."
  (interactive
   (let* ((root (mq-hg-root-directory))
          (patch-directory (mq-patch-directory-name root)))
     (list (read-file-name "New patch name: " patch-directory "" nil
                           ".patch")
           current-prefix-arg)))
  (cond
   ((string-equal name "")
    (message "No name for new patch provided; no new patch created"))
   ((file-exists-p name)
    (error "Patch already exists: %s" name))
   (t
    (let* ((root (mq-hg-root-directory))
           (patch-directory (mq-patch-directory-name root))
           (relative-name (file-relative-name name patch-directory)))
      (mq-shell-command "hg qnew%s '%s'"
                        (if force " -f" "")
                        relative-name)
      (mq-refresh)))))

(defun mq-visit-series (&optional finder)
  "Visit the series file for the current buffer's Mercurial Queues patch series.
If FINDER is non-nil, use that as the function to use to visit the file."
  (interactive)
  (let* ((root (mq-hg-root-directory))
         (series (mq-series-file-name root))
         (move-to
          (if (and buffer-file-name
                   (string-match ".*\\.hg/patches/\\'" default-directory))
              (file-name-nondirectory buffer-file-name))))
    (funcall (or finder 'find-file) series)
    (mq-point-to-patch move-to)))

(defun mq-visit-series-other-window ()
  "Visit the Mercurial Queues series file for current buffer in another window."
  (interactive)
  (mq-visit-series 'find-file-other-window))

(defun mq-visit-top-patch (&optional finder)
  "Visit the top applied patch in the current buffer's Mercurial Queue.
If FINDER is non-nil, use that as the function to visit the file."
  (interactive)
  (mq-check-for-queue)
  (let* ((root (mq-hg-root-directory))
         (patch-directory (mq-patch-directory-name root))
         (status (mq-parse-status-file (mq-status-file-name root))))
    (unless status
      (error "No patches currently applied in this buffer's source tree."))
    (funcall (or finder 'find-file)
             (expand-file-name (car status) patch-directory))))

(defun mq-visit-top-patch-other-window ()
  "Visit the top applied patch for the current buffer in another window."
  (interactive)
  (mq-visit-top-patch 'find-file-other-window))

(defun mq-show-top-next ()
  "Show the names of the top and next patch for this buffer's Mercurial Queue."
  (interactive)
  (mq-check-for-queue)
  (let* ((root (mq-hg-root-directory))
         (top (mq-top-patch-name root))
         (next (mq-next-patch-name root)))
    (if (not (or top next))
        (message "no patches applied or enabled")
      (let ((top-message (if top (format "top: %s" top)
                           "no patches applied"))
            (next-message (if next (format "next: %s" next)
                            "all enabled patches applied")))
        (message "%s; %s" top-message next-message)))))

(defun mq-qfold (patch delete)
  "Incorporate an unapplied patch into the top patch, as by `hg qfold'.
With a prefix argument, delete the incorporated patch."
  (interactive
   (let* ((root (mq-hg-root-directory))
          (patch-directory (mq-patch-directory-name root))
          (suggested (mq-suggest-unapplied-patch root)))
     (list (let ((default-directory patch-directory))
             (file-relative-name
              (read-file-name "Fold patch into top: " nil suggested t
                              suggested)))
           current-prefix-arg)))
  (mq-shell-command "hg qfold%s '%s'"
                    (if delete "" " --keep")
                    patch)
  (mq-refresh))


;;; Global key bindings.

(defvar mq-global-map
  (let ((map (make-keymap)))
    (define-key map "n" 'mq-qpush)
    (define-key map "p" 'mq-qpop)
    (define-key map "r" 'mq-qrefresh)
    (define-key map "s" 'mq-visit-series)
    (define-key map "t" 'mq-visit-top-patch)
    (define-key map "o" 'mq-qnew)
    (define-key map "=" 'mq-show-top-next)
    (define-key map "<" 'mq-qpop-all)
    (define-key map ">" 'mq-qpush-all)
    (define-key map "i" 'mq-qfold)
    map))

(defvar mq-global-other-window-map
  (let ((map (make-keymap)))
    (define-key map "s" 'mq-visit-series-other-window)
    (define-key map "t" 'mq-visit-top-patch-other-window)
    map))

(global-set-key "\C-xq" mq-global-map)
(global-set-key "\C-x4q" mq-global-other-window-map)


(provide 'mercurial-queues)
