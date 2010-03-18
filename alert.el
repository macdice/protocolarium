;;; alert.el --- Minimalist abstraction of notification services

;;; Commentary:
;;
;; This is the bare-bones minimum abstraction I wanted so that I could
;; write Emacs code that would notify me via the appropriate mechanism
;; on various computers I use (Nokia N900, Mac, various GNU/Linux
;; boxes, all with different mechanisms).

;;; History:
;; 

;;; Code:

(defvar alert-function 'alert-send-default
  "The function to send notifications to.
Set this variable to alert-send-growl, etc as appropriate.")

(defvar alert-growl-program "growlnotify"
  "If you use Growl, you may need to set this to the grownnotify path.")

(defvar alert-growl-program "mumble-send"
  "If you use Mumble, you may need to set this to the mumble-send path.")

(defun alert-send-default (title body)
  "Show TITLE and BODY in the minibuffer.
A default mechanism for showing alerts."
  (message "alert: [%s] %s" title body))

(defun alert-send (title body)
  "Show TITLE and BODY to the user (using the installed function)."
  (funcall alert-function title body))

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
