;;; omnibuf.el --- Shared buffers and kill-ring using Spread messaging
;; Copyright (c) 2009 Thomas Munro <munro@ip9.org>

;;; Commentary:
;; 
;; This is a simple hack for sharing data between Emacs process that
;; are connected to a Spread cluster (ie one or more Spread daemons).
;; It is a small step towards something along the lines of
;; 'SubEthaEdit', 'Gobby' or 'Rudel'.  It is a toy.
;; 
;; Some rainy day I would like to try to implement something like the
;; 'operational transformation' algorithm which Google Wave uses to
;; allow lock-free optimistic multi-master editing (see Xerox PARC
;; paper [1]).
;;
;; 1.  HOW TO USE
;;
;; To connect to the Spread daemon:
;;
;;   M-x omnibuf-connect host group
;;
;; Currently, you can only have one connection at a time for your
;; whole Emacs process.  If you want to disconnect:
;;
;;   M-x omnibuf-disconnect
;;
;; To kill/copy your currently selected region, so that anyone can yank it:
;;
;;   M-x omnibuf-copy-region
;;   M-x omnibuf-kill-region
;;
;; To yank (insert) text that was killed or copied by someone else who
;; is connected to the same Spread group:
;;
;;   M-x omnibuf-yank
;;
;; To make the current buffer 'findable' by anybody, you need to
;; 'share' it:
;;
;;   M-x omnibuf-share
;; 
;; To join a buffer that someone else has shared, you need to 'find' it
;; (you can use tab-autocompletion to see the names of shared buffers):
;;
;;   M-x omnibuf-find name
;; 
;; There are no keybindings defined by default, but you might like to
;; consider bindings which are similar to the standard kill/copy/yank
;; keys: maybe if you have 'super' or 'hyper' keys, you could use the
;; usual chords plus one of those.
;;
;; 2.  IMPLEMENTATION DETAILS
;;
;; The communication protocol is based on multicasting S-expressions
;; via Spread.  The following messages are multicast to the entire
;; group:
;;
;;   (copy-text <string>)
;;   (buffer-available <name-string>)
;;   (buffer-unavailable <name-string>)
;;   (update-region <name-string> <begin-pos> <length> <text-string>)
;;
;; The following messages are sent only between client and server
;; (server being the owner of a given buffer):
;;
;;   (request-buffer-contents <name-string>)
;;   (buffer-contents <name-string> <text-string>)
;;
;; 3.  FUTURE IMPROVEMENTS
;;
;; - read/write buffer clients, possibly with fancy OT algorithm
;; - one Spread group per buffer to cut down on IO to clients
;; - a kill 'ring' rather than a single kill (or just use built-in ring?)
;; - named registers for chunks of text (just use built-in registers?)
;; - abstraction to support different transport (AMQP, STOMP etc)
;;
;; 4.  REFERENCES
;;
;; [1] ftp://ftp.lambda.moo.mud.org/pub/MOO/papers/JupiterWin.ps
;; [2] http://en.wikipedia.org/wiki/The_man_on_the_Clapham_omnibus

;;; History:
;; 

(require 'cl)
(require 'spread)

;;; Code:

;; User settings

(defvar omnibuf-default-host "localhost"
  "The default host to suggest when connecting.
You should set this to something appropriate.")

(defvar omnibuf-default-group "OMNIBUF"
  "The default buffer to suggest when connecting.")

;; Global state (yes, I know, we could do better than this)

(defvar omnibuf-connection nil
  "Our connection to the Spread daemon.")

(defvar omnibuf-group nil
  "The name of the group we use.")

(defvar omnibuf-copied-text ""
  "A buffer for incoming copied regions.  Should really be a ring?")

(defvar omnibuf-buffers (make-hash-table :test 'equal)
  "The map of all available buffers we have heard about.")

;; Record types

(defstruct omnibuf-buffer
  "A record type for holding information about a shared buffer."
  (owner)       ;; the private group for talking to the owning node
  (name)        ;; the name of the buffer on the owning node
  (buffer)      ;; the Emacs buffer, if we are subscribed already
  (state))      ;; one of :unsubscribed, :requested, :subscribed

;; Private implementation details

(defun omnibuf-handle-connection (event message)
  "Event processor for Spread connections.
EVENT is the type of event, MESSAGE is an error message."
  (case event
    ((:connected)
     (spread-join omnibuf-group omnibuf-connection))
    (t
     (message "Omnibuf: %s %s" event message))))

(defun omnibuf-get-buffer (owner name)
  "Get the buffer info object for OWNER and buffer NAME."
  (let ((unique-name (format "%s@%s" name owner)))
    (gethash unique-name omnibuf-buffers)))

(defun omnibuf-set-buffer (owner name object)
  "Using OWNER and NAME as keys, store OBJECT."
  (let ((unique-name (format "%s@%s" name owner)))
    (setf (gethash unique-name omnibuf-buffers) object)))

(defun omnibuf-forget-buffer (owner name)
  "Forget the buffer information record stored under OWNER and NAME."
  (let ((unique-name (format "%s@%s" name owner)))
    (remhash unique-name omnibuf-buffers)))

(defun omnibuf-handle-message (event sender groups message-type message)
  "Process incoming Spread messages.
EVENT is the message type, SENDER is the private group of the
sending node, GROUPS is the list of groups the message was sent
to, MESSAGE-TYPE is a mystery to me, and MESSAGE is the message
data."
  (let ((object (first (read-from-string message))))
    ;(message "Received %S" object)
    (case (car object)
      ((copy-text)
       (setf omnibuf-copied-text (second object)))
      ((buffer-available)
       ;;; what if double broadcast?
       (omnibuf-set-buffer sender
                           (second object)
                           (make-omnibuf-buffer :owner sender
                                                :name (second object)
                                                :buffer nil
                                                :state :unsubscribed)))
      ((buffer-unavailable)
       ;; deactive our buffer!
       (omnibuf-forget-buffer sender (second object)))
      ((buffer-contents)
       (let ((info (omnibuf-get-buffer sender (second object))))
         (with-current-buffer (omnibuf-buffer-buffer info)
           (let ((inhibit-read-only t))
             (delete-region (point-min) (point-max))
             (insert (third object))))
         (setf (omnibuf-buffer-state info) :subscribed)))
      ((request-buffer-contents)
       (let ((buffer (get-buffer (second object))))
         (when buffer
           (with-current-buffer buffer
             (omnibuf-send `(buffer-contents ,(second object) ,(buffer-string))
                           sender)))))
      ((update-region)
       (let ((info (omnibuf-get-buffer sender (second object))))
         (if info
             (let ((buffer (omnibuf-buffer-buffer info)))
               (if buffer
                   (multiple-value-bind (begin length text)
                       (cddr object)
                     (with-current-buffer buffer
                       (save-excursion
                         (let ((inhibit-read-only t))
                           (when (> length 0)
                             (delete-region begin (+ begin length)))
                           (goto-char begin)
                           (insert text)))))))))))))

(defun* omnibuf-send (object &optional (group omnibuf-group))
  "Send a Lisp object OBJECT to the group."
  (spread-multicast (prin1-to-string object)
                    :group group
                    :connection omnibuf-connection))

(defun omnibuf-after-change (begin end previous-length)
  "Process buffer change in range BEGIN to END and PREVIOUS-LENGTH."
  (let ((new-text (buffer-substring-no-properties begin end)))
    (omnibuf-send `(update-region ,(buffer-name)
                                  ,begin
                                  ,previous-length
                                  ,new-text))))

(defun omnibuf-kill-buffer-hook ()
  "Cleanup code for when a buffer is killed."
  (when (boundp 'omnibuf-buffer)
    (omnibuf-forget-buffer (omnibuf-buffer-owner omnibuf-buffer)
                           (omnibuf-buffer-name omnibuf-buffer))))

;; Interactive functions

(defun omnibuf-disconnect ()
  "Disconnect from a Spread daemon."
  (interactive)
  (spread-disconnect omnibuf-connection)
  (setf omnibuf-connection nil))

(defun omnibuf-connect ()
  "Connect to a Spread daemon."
  (interactive)
  (when omnibuf-connection
    (omnibuf-disconnect))
  (let* ((host (read-from-minibuffer "Spread host: "
                                     omnibuf-default-host))
         (group (read-from-minibuffer "Spread group: "
                                      omnibuf-default-group)))
    (setf omnibuf-group group)
    (setf omnibuf-connection
          (spread-connect :name (format "omni-%d" (emacs-pid)) ;; TODO!
                          :host host
                          :message 'omnibuf-handle-message
                          :connection 'omnibuf-handle-connection))))

(defun omnibuf-share ()
  "Announce the current buffer as being joinable by anyone on the network."
  (interactive)
  (unless (spread-ready-p omnibuf-connection)
    (error "Omnibuf-share -- not connected"))
  (when (and (boundp 'omnibuf-shared-p) omnibuf-shared-p)
    (error "Omnibuf-share -- buffer is already shared"))
  (when (boundp 'omnibuf-slave-p)
    (error "Omnibuf-share -- cannot share a buffer that is alread a client"))
  (let ((global-after-change-functions after-change-functions))
    (set (make-local-variable 'omnibuf-shared-p) t)
    (make-local-variable 'after-change-functions)
    (add-hook 'after-change-functions 'omnibuf-after-change))
  (omnibuf-send `(buffer-available ,(buffer-name))))

(defun omnibuf-unshare ()
  "Stop sharing the current buffer."
  (interactive)
  (unless (spread-ready-p omnibuf-connection)
    (error "Omnibuf-share -- not connected"))
  (when (boundp 'omnibuf-shared-p)
    (setq omnibuf-shared-p nil)
    (remove-hook 'after-change-functions 'omnibuf-after-change)
    (omnibuf-send `(buffer-unavailable ,(buffer-name)))))

(defun omnibuf-find ()
  "Find and open a shared buffer."
  (interactive)
  (let ((encoded-name (completing-read "Find shared buffer: "
                                       omnibuf-buffers
                                       nil
                                       t)))
    (let ((info (gethash encoded-name omnibuf-buffers)))
      (if (omnibuf-buffer-buffer info)
          (switch-to-buffer (omnibuf-buffer-buffer info))
        (let ((buffer (get-buffer-create (omnibuf-buffer-name info))))
          (with-current-buffer buffer
            (setf buffer-read-only t)
            (set (make-local-variable 'omnibuf-slave-p) t)
            (set (make-local-variable 'omnibuf-buffer) info)
            (setf (omnibuf-buffer-buffer info) buffer)
            (setf (omnibuf-buffer-state info) :initial)
            (omnibuf-send `(request-buffer-contents 
                            ,(omnibuf-buffer-name info))
                          (omnibuf-buffer-owner info))          
            (add-hook 'kill-buffer-hook 'omnibuf-kill-buffer-hook)
            (switch-to-buffer buffer)))))))

(defun omnibuf-repaint ()
  "Request a full rebuild of a remote buffer."
  (interactive)
  (when (boundp 'omnibuf-slave-p)
    (omnibuf-send `(request-buffer-contents 
                    ,(omnibuf-buffer-name omnibuf-buffer))
                  (omnibuf-buffer-owner omnibuf-buffer))))
                    
(defun omnibuf-copy-region ()
  "Copy the current region onto the network, so other users can yank it."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning)
                                              (region-end))))
    (omnibuf-send `(copy-text ,text))))

(defun omnibuf-kill-region ()
  "Kill (and copy) the current region for others to yank."
  (interactive)
  (let ((text (delete-and-extract-region (region-beginning) (region-end))))
    (omnibuf-send `(copy-text ,text))))

(defun omnibuf-yank ()
  "Yank (insert) the most recently received text from the network."
  (interactive)
  (insert omnibuf-copied-text))

(provide 'omnibuf)

;;; omnibuf.el ends here
