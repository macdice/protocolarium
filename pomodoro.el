;;; pomodoro.el --- A simple-minded attempt at time management

;;; Commentary:
;;
;; This is an attempt to make a tomato-shaped egg timer out of some
;; old parentheses and a growling computer.  See
;; http://en.wikipedia.org/wiki/Pomodoro_Technique for more
;; information.
;;
;; BRIEF DESCRIPTION
;;
;; M-x pomodoro-work -- report that you have started working on a job
;; M-x pomodoro-break -- report that you have stopped working on a job
;; M-x pomodoro-done -- report that you have completed a job
;; M-x pomodoro-later -- create a job for later and get it off the screen
;;
;; SLIGHTLY LONGER
;; 
;; 1. Enter one or more jobs using lines beginning with a star and a
;;    space.  Ore more than one star.  Like org-mode.  I'm not really
;;    sure how well this will work with org-mode yet.
;;
;; 2. Put your cursor on top of any job line, and run M-x
;;    pomodoro-work to indicate that you have started working on that
;;    job.  Your 25 minute pomodoro time box starts now.  Try to avoid
;;    any interruptions during the pomodoro time.  If you manage to
;;    finish the job before the end of the time box, then run M-x
;;    pomodoro-done to record this, and reward yourself somehow.
;;
;; 3. If not done when your time box ends, you will be reminded to
;;    take a break.  You should indicate that you are stopping by
;;    running M-x pomodoro-break.  The break should last 5 minutes,
;;    and can be used to phone people back, follow up on subproblems
;;    discovered during the pomodoro time, make a cup of tea, etc.
;;
;; 4. If you really need to interrupt your pomodoro time before it is
;;    break time, you should indicate this using M-x pomodoro-break,
;;    just as above, but if you do so before the end of the time box,
;;    you will be asked to log a reason for the interruption.  If
;;    possible you should avoid having to do this: instead, register a
;;    job for later by running M-x pomodoro-later and entering a brief
;;    description, such as 'return call to Fred', or 'put out flames'.

;;; History:
;; 

(require 'alert)

;;; Code:

(defvar pomodoro-state nil
  "The current state, one of nil, :work, :break.")

(defvar pomodoro-work-duration (* 25 60.0)
  "How long we concentrate on one task before taking a break.")

(defvar pomodoro-break-duration (* 5 60.0)
  "How long we break for between tasks.")

(defvar pomodoro-start-time nil
  "When the most recent state was entered.")

(defvar pomodoro-next-end-time nil
  "When the current state is due to end.")

(defvar pomodoro-notified nil
  "Whether we have generated a visual notification.")

(defun pomodoro-notify (text)
  "Show TEXT using Mumble, Growl etc (via ALERT)."
  (message "Pomodoro: %s" text)
  (alert-send "Emacs Pomodoro" text))

(defun pomodoro-epoch->time-string (time)
  "Convert floating point epoch TIME to a short and sweet date/time string."
  (format-time-string "%m/%d %H:%M" (seconds-to-time time)))

(defun pomodoro-message ()
  "Check the state of things, and decide whether to message/notify the user."
  (let ((now (float-time)))
    (case pomodoro-state
      ((:work)
       (cond ((> now pomodoro-next-end-time)
              ;; TODO update countdown display
              (unless pomodoro-notified
                (pomodoro-notify "Take a break!")
                (setq pomodoro-notified t)))
             (t
              ;; TODO update countdown display
              (message "Pomodoro -- take a break in %d minutes"
                       (/ (- pomodoro-next-end-time now) 60)))))
      ((:break)
       (cond ((> now pomodoro-next-end-time)
              (message "Pomodoro -- break over")
              (unless pomodoro-notified
                (pomodoro-notify "Break over, back to work!")))
             (t
              ;; TODO update countdown display
              (message "Pomodoro -- take a new task in %d minutes"
                       (/ (- pomodoro-next-end-time now) 60))))))))

(defun pomodoro-append-log (text)
  "Append TEXT to the end of the current job paragraph."
  (save-excursion
    (end-of-line)
    (unless (search-backward-regexp "^\\** " (point-min) t)
      (error "Pomodoro -- expected to find job paragraph starting with a star"))
    (end-of-line)
    (unless (search-forward-regexp "\\(^ *$\\|^\\** \\)" (point-max) t)
      (goto-char (point-max))
      (insert "\n"))
    (beginning-of-line)
    (insert "  ")
    (insert (pomodoro-epoch->time-string (float-time)))
    (insert " ")
    (insert text)
    (insert "\n")))

(defun pomodoro-timer-handler ()
  "Self-reinstalling timer handler."
  (pomodoro-message)
  (when pomodoro-state
    (run-at-time 60 nil 'pomodoro-timer-handler)))

(defun pomodoro-work ()
  "Command to run to indicate that you are working on the job at point."
  (interactive)
  (let ((now (float-time)))
    (pomodoro-append-log "started work")
    (setq pomodoro-state :work)
    (setq pomodoro-start-time now)
    (setq pomodoro-next-end-time (+ now pomodoro-work-duration))
    (setq pomodoro-notified nil)
    (pomodoro-notify "You work starts -- now!")
    (run-at-time 60 nil 'pomodoro-timer-handler)))

(defun pomodoro-done ()
  "Command to run to indicate that you have finished the job at point."
  (interactive)
  (pomodoro-append-log "done")
  (setq pomodoro-state nil)
  (pomodoro-notify "Missing accomplished"))

(defun pomodoro-break ()
  "Command to indicate that you are taking a break, or are interrupted."
  (interactive)
  (let ((now (float-time)))
    (if (and pomodoro-next-end-time (< now pomodoro-next-end-time))
        (let ((reason (read-from-minibuffer "Reason for interruption: ")))
          (pomodoro-append-log (format "*INTERRUPTED* %s" reason))
          (setq pomodoro-state :break)
          (setq pomodoro-start-time now)
          (setq pomodoro-next-end-time (+ now pomodoro-break-duration))
          (pomodoro-notify "Work interuppted!"))
      (progn
        (pomodoro-append-log (format "<break for %d minutes of other tasks>"
                                     (/ pomodoro-break-duration 60.0)))
        (setq pomodoro-state :break)
        (setq pomodoro-start-time now)
        (setq pomodoro-next-end-time (+ now pomodoro-break-duration))
        (pomodoro-notify "Taking a break")))))

(defun pomodoro-later ()
  "Command to register a subjob to be done later."
  (interactive)
  (let ((text (read-from-minibuffer "Defer task: ")))
    (save-excursion
      (end-of-buffer)
      (insert "\n* ")
      (insert text)
      (insert " (deferred at ")
      (insert (pomodoro-epoch->time-string (float-time)))
      (insert ")\n")))
  (pomodoro-notify "Task deferred for later"))

(provide 'pomodoro)

;;; pomodoro.el ends here
