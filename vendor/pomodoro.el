;;; pomodoro.el --- Pomodoro Technique for emacs

;;; Commentary:

;;; THANKS:

;;; BUGS:

;;; INSTALLATION:

;;; Code:

(defvar pomodoro-work-time 25
  "Time in minutes of work")

(defvar pomodoro-short-break 5
  "Time in minute of short break")

(defvar pomodoro-long-break 15
  "Time in minute of long break")

(defvar pomodoro-set-number 4
  "Number of sets until a long break")

(defvar pomodoro-buffer-name "*pomodoro*"
  "Name of the pomodoro buffer")

(defvar pomodoro-display-string "")
(defvar pomodoro-minute)
(defvar pomodoro-set)
(defvar pomodoro-timer nil)
(defvar pomodoro-state 'work)

;; (setq pomodoro-set 1)
;;;###autoload
(defun pomodoro ()
  (interactive)
  (if pomodoro-timer (cancel-timer pomodoro-timer))
  (or (memq 'pomodoro-display-string global-mode-string)
      (setq global-mode-string
            (append global-mode-string '(pomodoro-display-string))))
  (setq
   pomodoro-minute pomodoro-work-time
   pomodoro-set 1
   pomodoro-state 'work
   pomodoro-timer (run-at-time t 60 'pomodoro-timer))
  (pomodoro-update-modeline))

;; (setq pomodoro-state 'work pomodoro-minute 1 pomodoro-set 1)

(defun pomodoro-timer ()
  (setq pomodoro-minute (- pomodoro-minute 1))
  (if (<= pomodoro-minute 0)
      (cond ((eq pomodoro-state 'long-break)
             (setq pomodoro-state 'work
                   pomodoro-minute pomodoro-work-time)
             (pomodoro-message "Work"))
            ((eq pomodoro-state 'short-break)
             (setq pomodoro-state 'work
                   pomodoro-minute pomodoro-work-time)
             (setq pomodoro-set (+ pomodoro-set 1))
             (pomodoro-message "Work"))
            ((eq pomodoro-state 'work)
             (if (>= pomodoro-set pomodoro-set-number)
                 (progn
                   (setq pomodoro-minute pomodoro-long-break
                         pomodoro-state 'long-break
                         pomodoro-set 1)
                   (pomodoro-message "Long break"))
               (setq pomodoro-minute pomodoro-short-break
                     pomodoro-state 'short-break)
               (pomodoro-message "Short break")))))
  (pomodoro-update-modeline))

(defun pomodoro-update-modeline ()
  (setq pomodoro-display-string
        (cond ((eq pomodoro-state 'work)
               (format "W%d-%d" pomodoro-set pomodoro-minute))
              ((eq pomodoro-state 'short-break)
               (format "B%d-%d" pomodoro-set pomodoro-minute))
              (t
               (format "LB-%d" pomodoro-minute))))
  (force-mode-line-update))

(defun pomodoro-message (msg)
  (let ((this-window (selected-window)))
    (raise-frame (selected-frame))
    (with-current-buffer (get-buffer-create pomodoro-buffer-name)
      (erase-buffer)
      (insert msg))
    (pop-to-buffer pomodoro-buffer-name)
    (fit-window-to-buffer)
    (select-window this-window)))

;; Local Variables:
;; compile-command: "make"
;; End:

;; Copyright (C) 2010 Ivan Kanis
;; Author: Ivan Kanis
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
