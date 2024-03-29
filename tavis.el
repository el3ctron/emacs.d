(require 'org)
(require 'org-clock)
(require 'org-remember)
(require 'org-agenda)
(require 'org-icalendar)
(require 'org-timer)
(require 'calendar)
;;(require 'timezone)
(require 'solar)
(require 'appt)

(setq user-mail-address "tavis@damnsimple.com")
(setq user-full-name "Tavis Rudd")
(setq calendar-latitude 49.29)
(setq calendar-longitude -123.144)
(setq calendar-location-name "Vancouver, BC")
(setq calendar-standard-time-zone-name "PST")
(setq calendar-daylight-time-zone-name "PDT")

(setq sentence-end-double-space nil)
(setq suggest-key-bindings nil)

;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
(defun dss-popup-notify (title msg &optional icon sound)
  (interactive)
  (let ((sound (if (and (numberp sound) (not (< sound 0)))
                   (or sound "/usr/share/sounds/phone.wav"))))
    (when sound (call-process-shell-command
                 (concat "ssh vb3 aplay " sound " 2> /dev/null &")
                 nil 0))
    (call-process-shell-command
     (concat "ssh vb3 \"DISPLAY=:0 notify-send "
             (if icon (concat "-i " icon) "")
             " --expire-time=3500 -u critical"
             " '" title "' '" msg "'"
             "\"") nil 0)
    (call-process-shell-command
     (concat
      "echo '" title "' | prowl_tavis.sh -1 'Emacs notification'") nil 0)
    (message (concat title ": " msg))))

(defvar *dss-org-notification-hooks* nil)
(defun dss/org-notify (msg)
  (interactive "sMsg: ")
  (dss-popup-notify "org-mode" msg)
  (dss/org-set-task-state msg)
  (run-hooks '*dss-org-notification-hooks*))

(setq org-show-notification-handler
      (lambda (msg)
        (dss/org-notify msg)))

(setq dss-org-timer-message "timer done")
(defun dss/org-set-timer-message (message)
  (interactive "sMessage:")
  (setq dss-org-timer-message message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *dss-annoying-modeline-timer* nil)

(defun dss/cancel-annoying-modeline ()
  (interactive)
  (if *dss-annoying-modeline-timer*
      (cancel-timer *dss-annoying-modeline-timer*))
  (run-with-timer 1 nil (lambda () (set-face-inverse-video-p 'modeline nil))))

(defvar *dss-nag-timer-hook* nil)
(defun dss/osx-nag ()
  (interactive)
  (if (> (random 2) 0)
      (dss/org-set-task-state "FOCUS !!!")
    (dss/org-set-task-state "!!! FOCUS")))

(add-hook '*dss-nag-timer-hook* 'dss/osx-nag)

(defun dss/start-annoying-modeline ()
  (interactive)
  (if *dss-annoying-modeline-timer*
      (cancel-timer *dss-annoying-modeline-timer*))
  (setq *dss-annoying-modeline-timer*
        (run-with-timer 0 4
                        (lambda ()
                          (if (face-inverse-video-p 'modeline)
                              (set-face-inverse-video-p 'modeline nil)
                            (set-face-inverse-video-p 'modeline t))
                          (run-hooks '*dss-nag-timer-hook*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://osdir.com/ml/emacs-orgmode-gnu/2009-06/msg00144.html

(defun dss/org-clock-increase-effort-estimate (add-effort)
  "Add time to the effort estimate.
Update Effort property of currently clocked item.
Update mode line."
  (interactive "sHow much to add? (hh:mm or mm)? ")
  (if (and (dss/org-clock-is-active) org-clock-effort)
      (let ((add-effort-minutes (dss/org-string-to-minutes add-effort)))
        (progn (setq new-effort (org-minutes-to-hh:mm-string
                                 (+ add-effort-minutes
                                    (org-hh:mm-string-to-minutes
                                     org-clock-effort))))
               (dss/org-clock-set-effort new-effort)))))

(defun dss/org-clock-set-effort (effort-string)
  "Increase effort estimate PROPERTY for the currently clocked item.
Jump to the correct buffer, increace the PROPERTY, jump back."
  (interactive "sHow much to add? (hh:mm or mm)? ")
  (if (dss/org-clock-is-active)
      (save-window-excursion
        (setq org-clock-effort effort-string)
        (org-clock-update-mode-line)
        (org-clock-goto)
        (org-set-property "Effort" effort-string)
        (message "Effort was increased."))))


(defun dss/org-string-to-minutes (string)
  "Recognizes two formats:
1:30 - converted to minutes
30 - interpreted as minutes."
  (case (length (split-string string ":"))
    (2 (org-hh:mm-string-to-minutes string))
    (1 (string-to-int string))))

(defun dss/org-clock-is-active ()
  "Return true if clock is currently running.
nil otherwise."
  (if (marker-buffer org-clock-marker)
      t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dss/org-clock-heading ()
  (interactive)
  (if (org-clocking-p)
      (let ((heading org-clock-heading))
        (if heading (set-text-properties 0 (length heading) nil heading))
        heading)))

(defun dss/org-insert-link-to-clock ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-clock-goto)
      (org-id-get-create)
      (call-interactively 'org-store-link)))
  (let* ((last-stored (car org-stored-links))
         (link (car last-stored))
         ;; (desc (cdr last-stored))
         (desc (dss/org-clock-heading)))
    (insert (org-make-link-string link desc))))

(defun dss/org-clock-in-hook ()
  (dss/org-set-timer-message "take a break")
  (org-id-get-create)
  (dss/cancel-annoying-modeline)
  (org-timer-set-timer '(16)))

(defun dss/org-clock-out-hook ()
  (org-timer-cancel-timer)
  (dss/org-set-timer-message "get back to work")
  (org-timer-set-timer 5))

(defun dss/org-timer-done-hook ()
  (dss-popup-notify "org-mode" dss-org-timer-message)
  (dss/start-annoying-modeline))

(add-hook 'org-clock-in-hook 'dss/org-clock-in-hook)
(add-hook 'org-clock-out-hook 'dss/org-clock-out-hook)
(setq org-timer-done-hook 'dss/org-timer-done-hook)

(setq org-timer-default-timer 25)
(setq org-clock-idle-time 30)

(defun dss/org-set-default-timer (time)
  (interactive "nDefault: ")
  (setq org-timer-default-timer time))





(defun dss-appt-display (min-to-app new-time msg)
  (dss-popup-notify (format "Appointment in %s minute(s)" min-to-app)
                    msg "/usr/share/icons/gnome/32x32/status/appointment-soon.png")
  (appt-disp-window min-to-app new-time msg))
(setq appt-disp-window-function (function dss-appt-display))

(appt-activate 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode config
;; see http://doc.norang.ca/org-mode.html

;(require 'org-babel-init)
;(require 'org-protocol)

(setq org-directory "~/org_mode/")
(setq org-clock-persist-file (concat org-directory "/.org-clock-save.el"))
(setq org-icalendar-include-todo t)
(setq org-use-speed-commands t)

(defun dss/babel-no-confirm ()
  (interactive)
  (setq org-confirm-babel-evaluate nil))

(setq org-src-fontify-natively t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tasks and states (norang sec 3)
(setq org-todo-keywords (quote (
  (sequence "TODO(t)" "TODAY(y!)" "|" "STARTED(s!)" "|" "DONE(d!/!)")
  (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
 ;(sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
 )))

(setq org-todo-keyword-faces (quote (
 ("TODO" :foreground "red" :weight bold)
 ("TODAY" :foreground "color-27" :weight bold)
 ("STARTED" :foreground "color-27" :weight bold)
 ("DONE" :foreground "forest green" :weight bold)
 ("WAITING" :foreground "orange" :weight bold)
 ("SOMEDAY" :foreground "magenta" :weight bold)
 ("CANCELLED" :foreground "forest green" :weight bold)
 ;("QUOTE" :foreground "red" :weight bold)
 ;("QUOTED" :foreground "magenta" :weight bold)
 ;("APPROVED" :foreground "forest green" :weight bold)
 ;("EXPIRED" :foreground "forest green" :weight bold)
 ;("REJECTED" :foreground "forest green" :weight bold)
 ;("OPEN" :foreground "blue" :weight bold)
 )))
(setq org-use-fast-todo-selection t)

;; allow S-left/right state changes that don't set timestamps; useful for state fixups
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t) ("NEXT"))
              ("SOMEDAY" ("WAITING" . t))
              (done ("NEXT") ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("STARTED" ("WAITING") ("NEXT" . t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-remember (norang sec 4)
; http://orgmode.org/manual/Storing-notes.html

; in org-agenda view type 'k r' to remember something with timestamps set relative to current agenda line
; use org-remember-mode-map for extra key-bindings

; see http://emacs-fu.blogspot.com/2009/04/remember.html
; for ways to pop remember via emacsclient from outside of emacs

; see org-remember-mode-hook
; see org-remember-finalize-hook

(setq org-default-notes-file (concat org-directory "/refile.org"))

(setq org-remember-clock-out-on-exit nil)
(setq org-remember-store-without-prompt t)

;; http://orgmode.org/manual/Remember-templates.html
;(setq org-remember-default-headline "Tasks")

;(setq org-remember-templates (quote (
;  ("todo" ?t "* TODO %?
;  %U
;  %a" nil bottom nil)
;  ("note" ?n "* %?                                        :NOTE:
;  %U
;  %a" nil bottom nil)
;  ("working on" ?w "* STARTED %?
;  %U
;
;  %a" nil bottom nil)
;  )))

;; http://orgmode.org/manual/Remember-templates.html
; (desc ?key "<template>" <file> <heading> <context where avail, can be left out>)

;; (setq org-remember-templates
;;       '(
;;         ("Todo" ?t "* TODO %?\n  ADDED:%U" nil "Tasks")
;;         ("Started" ?s "* STARTED %?\n  ADDED:%U\n :CLOCK-IN:" nil "Tasks")
;;         ("Journal" ?j "* %U %?" nil "Journal")
;;         ("Idea" ?i "* %?\n  %i\n  ADDED:%U" nil "Ideas")
;;         ("Note" ?n "* %?\n  %i\n  ADDED:%U" nil "Notes")
;;         ("Snippet" ?k "* %?\n  %^C\n  ADDED:%U" nil "Snippets")
;;         ("URL" ?u "* %? %(dss/moz-get-url)\n ADDED:%U" nil "Notes")

;;         ;; ("link" ?l " %? %(dss/moz-get-url)\n ADDED:%U" nil "Notes")

;;         ;; (?w "* %:description\n\n  Source: %u, %:link %c\n\n  %i" nil "Notes")
;;         ))

;;; http://orgmode.org/manual/Template-elements.html#Template-elements
(defun dss/org-capture-current-location ()
  "NOTE: this does not work at present"
  (interactive)
  (widen)
  ;; (call-interactively 'org-mark-subtree)
  ;; (exchange-point-and-mark)
  ;; (k2-toggle-mark)
  (if (not (org-at-heading-p))
      (outline-previous-visible-heading 1)))
;;  (goto-char (point-max))

(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/org_mode//refile.org" "Tasks")
         "* TODO %?")
        ("T" "Todo right here" entry
         (function dss/org-capture-current-location)
         "* TODO %?" :unnarrowed)
        ("s" "Started" entry
         (file+headline "~/org_mode//refile.org" "Tasks")
         "* STARTED %?"
         :clock-in)
        ("j" "Journal" entry
         (file+headline "~/org_mode//refile.org" "Journal")
         "* %U %?")
        ("i" "Idea" entry
         (file+headline "~/org_mode//refile.org" "Ideas")
         "* %?\n  %i")
        ("n" "Note" entry
         (file+headline "~/org_mode//refile.org" "Notes")
         "* %?\n  %i")

        ("d" "dentalle task" entry
         (file "~/org_mode/dentalle-current.org")
         "* TODO %?")

        ("k" "Snippet" entry
         (file+headline "~/org_mode//refile.org" "Snippets")
         "* %?\n  %^C\n")

        ("l" "Link" entry
         (file+headline "~/org_mode//refile.org" "Notes")
         "* %?%(dss/moz-get-title) %(dss/moz-get-url)")

        ;; just playing around:
        ;; ("l" "Link" item
        ;;  (id "4bcfaca6-d6ad-4f10-8325-17f77f54006c")
        ;;  " %? %(dss/moz-get-url) %(or (org-capture-get :foo) \"\")"
        ;;  :foo "asdfasdf"
        ;;  :before-finalize (org-set-property "foo" "999"))

        ;; just playing around:
        ;; ("T" "tbl" table-line
        ;;  (id "4bcfaca6-d6ad-4f10-8325-17f77f54006c")
        ;;  "| # | %T | %^{weight} | |" :table-line-pos "II-1"
        ;;  )

        ))


(defun dss/org-current-timestamp ()
  (let ((fmt (concat
              "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")))
    (format-time-string fmt)))

(defun dss/org-capture-before-finalize-hook ()
  (eval (org-capture-get :before-finalize))
  (org-id-get-create)
  (org-set-property "ADDED" (dss/org-current-timestamp)))

(add-hook 'org-capture-before-finalize-hook 'dss/org-capture-before-finalize-hook)

;; (defun dss/org-capture-after-finalize-hook ()
;;   (interactive)
;;   )
;; (add-hook 'org-capture-after-finalize-hook 'dss/org-capture-after-finalize-hook)

;; see http://sachachua.com/wp/2008/07/20/emacs-smarter-interactive-prompts-with-org-remember-templates/
;; for a way to do something like this
;;         ("Ticket" ?T "* [[T:%^{Number}][%^{Number}/%^{Description}]] %?\n  %U %a" nil "Tasks")
;; without having to repeat the Number part twice
; (setq org-link-abbrev-alist
;       '(("T" . "https://secure.dentalle.com/ticket_redirector/?ticket_id=")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; norang sec 5 refiling

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
;(setq org-refile-targets (quote ((org-agenda-files . (:maxlevel . 5)) (nil :maxlevel . 5))))
;(setq org-refile-targets `((org-agenda-files . (:maxlevel . 2)) (nil :maxlevel . 2)))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2)) (nil . (:maxlevel . 2))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t) ; seems buggy when off
(setq org-completion-use-ido t)
; Targets start with the file name - allows creating level 1 tasks
;(setq org-refile-use-outline-path (quote file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; norang sec 6 custom agenda views
(setq org-agenda-custom-commands
      (quote (("S" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("y" "Today's Tasks" todo "TODAY")
              ("w" "Tasks waiting on something" tags "WAITING/!" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "LEVEL=2+REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("N" "Notes" tags "NOTE" nil)
              ("n" "Next" tags "NEXT-WAITING-CANCELLED/!" nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; norang sec 7 clocking
;; Resume clocking tasks when emacs is restarted
(setq org-clock-persist t)
(org-clock-persistence-insinuate)
(setq org-clock-history-length 35)
(setq org-clock-persist-query-resume nil)
(setq org-clock-in-resume t)
;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
(setq org-clock-into-drawer "CLOCK")
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-time-stamp-rounding-minutes (quote (1 5)))

;; Agenda clock report parameters (no links, 2 levels deep)
(setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see norang sec 8.2
; Set default column view headings: Task Effort Clock_Summary
;(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
(setq org-columns-default-format "%70ITEM %7TODO(To Do) %6Effort{:} %6CLOCKSUM{Total} %10Bill{+}")

; global Effort estimate values
;(setq org-global-properties
;      (quote (("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 8:00"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; norang 9.1
; ; Tags with fast selection keys
; (setq org-tag-alist (quote ((:startgroup)
;                             ("@InTown" . ?t)
;                             ("@Work" . ?w)
;                             ("@Home" . ?h)
;                             ("@Farm" . ?f)
;                             (:endgroup)
;                             ("QUOTE" . ?q)
;                             ("NEXT" . ?N)
;                             ("GSOC" . ?g)
;                             ("WAITING" . ?W)
;                             ("FARM" . ?F)
;                             ("HOME" . ?H)
;                             ("ORG" . ?O)
;                             ("PLAY" . ?p)
;                             ("CANCELLED" . ?C))))
;
; ; Allow setting single tags without the menu
; (setq org-fast-tag-selection-single-key (quote expert))
;
; ; For tag searches ignore tasks with scheduled and deadline dates
; (setq org-agenda-tags-todo-honor-ignore-options t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-agenda norang sec 15
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
(setq org-agenda-todo-ignore-with-date nil)
(setq org-agenda-skip-deadline-if-done nil)
(setq org-agenda-skip-scheduled-if-done nil)

(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
(setq org-enforce-todo-dependencies t)

; 15.8.3 handling blank lines
(setq org-cycle-separator-lines 0)
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item))))

(setq org-insert-heading-respect-content t)
(setq org-return-follows-link nil)

; 15.8.7
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings nil)

; 15.8.8
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

; 15.13 logging
;(setq org-log-done (quote time))
(setq org-log-done nil)
(setq org-log-into-drawer "LOGBOOK")
