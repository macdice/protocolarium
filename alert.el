;;; alert.el --- Minimalist abstraction of notification services
;; Copyright (c) 2010 Thomas Munro, Ewan Higgs

;; Author: Thomas Munro <munro@ip9.org>
;; Keywords: notification, mumbles, growl, libnotify

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is the bare-bones minimum abstraction I wanted so that I could
;; write Emacs code that would notify me via the appropriate mechanism
;; on various computers I use (Nokia N900, Mac, various GNU/Linux
;; boxes, all with different mechanisms).

;;; History:
;;
;; * created by Thomas Munro in March 2010
;; * libnotify support added by Ewan Higgs

;;; Code:

(defvar alert-function 'alert-send-default
  "The function to send notifications to.
Set this variable to alert-send-growl, etc as appropriate.")

(defvar alert-notify-program "notify-send"
  "If you use libnotify, you may need to set this to the libnotify path.")

(defvar alert-growl-program "growlnotify"
  "If you use Growl, you may need to set this to the grownnotify path.")

(defvar alert-mumbles-program "mumbles-send"
  "If you use Mumbles, you may need to set this to the mumbles-send path.")

(defun alert-send-default (title body)
  "Show TITLE and BODY in the minibuffer.
A default mechanism for showing alerts."
  (message "alert: [%s] %s" title body))

(defun alert-send (title body)
  "Show TITLE and BODY to the user (using the installed function)."
  (funcall alert-function title body))

(defun alert-send-notify (title body)
  "Show TITLE and BODY to a libnotify user."
  (start-process "notify-send"
		 "notify-send"
		 alert-notify-program
		 title
		 body))

(defun alert-send-growl (title body)
  "Show TITLE and BODY to a Growl user."
  (start-process "growl"
                 "growl"
                 alert-growl-program
                 "-t"
                 title
                 "-m"
                 body))

(defun alert-send-mumbles (title body)
  "Show TITLE and BODY to a Mumble user."
  (start-process "mumbles"
                 "mumbles"
                 alert-mumbles-program
                 title
                 body))

(defun alert-send-n900 (title body)
  "Show TITLE and BODY to a Nokia N900 user."
  ;; TODO we should be able to do this directly using Emacs Dbus support        
  (start-process "dbus-send"
                 "dbus-send"
                 "dbus-send"
                 "--type=method_call"
                 "--dest=org.freedesktop.Notifications"
                 "/org/freedesktop/Notifications"
                 "org.freedesktop.Notifications.SystemNoteInfoprint"
                 (format "string:%s\n%s" title body)))

(provide 'alert)

;;; alert.el ends here
