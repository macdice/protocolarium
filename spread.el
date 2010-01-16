;;; spread.el --- A toy Spread Toolkit client for Emacs
;; Copyright (c) 2009, 2010 Thomas Munro

;; Author: Thomas Munro <munro@ip9.org>
;; Keywords: spread, protocol, multicast

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
;; This is just a little warm-up exercise in network programming.  See
;; www.spread.org for more information about the Spread Toolkit, and
;; client libraries for C and Java.
;;
;; Here is an example of use:
;;
;;   ;; create a connection, in this case using the global default
;;   ;; connection name because we are being lazy in the REPL, but you
;;   ;; can have multiple connections and you don't need to use the
;;   ;; default connection variable at all; you can set various
;;   ;; options with keyword arguments, see documentation
;;   (setq spread-default-connection (spread-connect :host "foobox"))
;;
;;   ;; send out a message to a a particular group; defaults to the
;;   ;; connection spread-default-connection, but you can provide the
;;   ;; connection explicitly; you can also set various other options
;;   ;; with keyword arguments, see documentation
;;   (spread-multicast "Hi everyone" :group "GROUP42")
;;
;;   ;; register to receive all messages sent to a group; as above, in
;;   ;; this case we are being lazy again and using the default
;;   ;; connection; we are also using the default callback which
;;   ;; simply prints incoming messages out in the *Messages* buffer
;;   ;; and mini-buffer, but you can provide your own callback which
;;   ;; will receive all mesages for groups that you join
;;   (spread-join "NEWS_FLASHES")
;;
;; Please see the Spread Toolkit documentation for the meaning of
;; service types, message types, priority, membership, self-discard,
;; groups and other Spread concepts.
;;
;; TODO - what is MESSAGE-TYPE actually for?
;; TODO - find out about how to support the new Spread 4.x protocol
;; TODO - implement SPREAD-CONNECT-DEFAULT so that you don't have to
;;        use SETQ to create it yourself
;; TODO - a nice WITH- macro for scoped connections

;;; History:
;; 

(require 'bindat)
(require 'bio)
(require 'cl)

;;; Code:

;; This is the Spread version that we claim to be.  This is kind of bogus,
;; but the protocol doesn't really distinguish between protocol versions
;; and Spread versions, so we have to make this bogus claim.
(defconst spread-major-version 3)
(defconst spread-minor-version 17)
(defconst spread-patch-version 4)
(defconst spread-min-minor-version 10
  "The minumum version we are prepared to accept from the server.")

;; Limits and sizes used in the protocol.
(defconst spread-max-group-name 32)
(defconst spread-max-private-name 10)
(defconst spread-max-auth-name 30)
(defconst spread-max-auth-methods 3)
(defconst spread-max-message-length 140000)
(defconst spread-header-size (+ spread-max-group-name (* 4 4)))

;; Magic numbers used to build and mask service numbers in the protocol.
(defconst spread-UNRELIABLE_MESS #x00000001)
(defconst spread-RELIABLE_MESS   #x00000002)
(defconst spread-FIFO_MESS       #x00000004)
(defconst spread-CAUSAL_MESS     #x00000008)
(defconst spread-AGREED_MESS     #x00000010)
(defconst spread-SAFE_MESS       #x00000020)
(defconst spread-REGULAR_MESS    #x0000003f)
(defconst spread-SELF_DISCARD    #x00000040)
(defconst spread-REG_MEMB_MESS   #x00001000)
(defconst spread-TRANSITION_MESS #x00002000)
(defconst spread-JOIN_MESS       #x00010000)
(defconst spread-LEAVE_MESS      #x00020000)
(defconst spread-KILL_MESS       #x00040000)
(defconst spread-GROUPS_MESS     #x00080000)
(defconst spread-MEMBERSHIP_MESS #x00003f00)
(defconst spread-REJECT_MESS     #x00400000)
(defconst spread-ENDIAN_MASK     #x7fffff7f) ;; to remove endian flags

;; Magic numbers used to build flags in the login message.
(defconst spread-MEMBERSHIP      #x01)
(defconst spread-PRIORITY        #x10)

;; Defaults used in this implementation.
(defconst spread-default-port 4803 "The default TCP port for Spread deamons.")
(defconst spread-default-host "localhost" "The default Spread daemon host.")

(defvar spread-default-connection nil
  "A global connection, for use by lazy people trying things out in the REPL.
Real software should probably not be using a global variable to
hold a conneciton.")

(defun spread-service-type->bitmap (symbol)
  "Convert a Lisp-friendly service type SYMBOL to a number used on the wire."
  (ecase symbol
    ((:unreliable) spread-UNRELIABLE_MESS)
    ((:reliable)   spread-RELIABLE_MESS)
    ((:fifo)       spread-FIFO_MESS)
    ((:causal)     spread-CAUSAL_MESS)
    ((:agreed)     spread-AGREED_MESS)
    ((:safe)       spread-SAFE_MESS)
    ((:join)       spread-JOIN_MESS)
    ((:leave)      spread-LEAVE_MESS)))

(defun spread-bitmap->message-type (bitmap)
  "Extract a Lisp-friendly symbol for the message type bits in BITMAP."
  (case (logand bitmap spread-ENDIAN_MASK spread-REGULAR_MESS)
    ((#x00000001) :unreliable)
    ((#x00000002) :reliable)
    ((#x00000004) :fifo)
    ((#x00000008) :casual)
    ((#x00000010) :agreed)
    ((#x00000020) :safe)
    (t            :unknown)))

(defun spread-bitmap->membership-type (bitmap)
  "Extract a Lisp-friendly symbol for the membership type bits in BITMAP."
  (case (logand bitmap spread-ENDIAN_MASK spread-MEMBERSHIP_MESS)
    ((#x0100 #x1100) :join)
    ((#x0200 #x1200) :leave)
    ((#x0400 #x1400) :disconnect)
    ((#x0800 #x1800) :network)
    (t               :unknown)))

(defun spread-regular-message-p (service-type)
  "Check if SERVICE-TYPE is a regular message."
  (> (logand spread-REGULAR_MESS service-type) 0))

(defun spread-membership-message-p (service-type)
  "Check if SERVICE-TYPE is a membership message."
  (> (logand spread-MEMBERSHIP_MESS service-type) 0))

(defun spread-transition-p (service-type)
  "Check if SERVICE-TYPE is a transition message."
  (> (logand spread-TRANSITION_MESS service-type) 0))

;;; Binary formats used in the protocol (for the BINDAT library)

(defconst spread-login-request-bindat-spec
  `((:major-version u8)
    (:minor-version u8)
    (:patch-version u8)
    (:flags u8)
    (:name-length u8)
    (:name str (:name-length)))
  "The binary format specification for outgoing login request message.")

(defconst spread-multicast-bindat-spec
  `((:service-type u32)
    (:group-name strz ,spread-max-group-name)
    (:group-count u32)
    (:message-type u32)
    (:data-length u32)
    (:groups repeat (:group-count) (:name strz ,spread-max-group-name))
    (:data str (:data-length)))
  "The binary format specification for outgoing messages.")

(defconst spread-header-be-bindat-spec
  `((:service-type u32)
    (:group-name strz ,spread-max-group-name)
    (:group-count u32)
    (:message-type u32)
    (:data-length u32))
  "The binary format specification for big-endian incoming headers.")

(defconst spread-header-le-bindat-spec
  `((:service-type u32r)
    (:group-name strz ,spread-max-group-name)
    (:group-count u32r)
    (:message-type u32r)
    (:data-length u32r))
  "The binary format specification for little-endian incoming headers.")

;; Stuctures

(defstruct spread-connection
  "A struct representing a connection to a Spread daemon."
  (socket)
  (private-name)
  (state)
  (message-callback)
  (connection-callback)
  (membership-callback))

;; Private implementation code

(defun spread-send (socket binary-spec data)
  "Pack binary data using `bindat' and send it.
Write to SOCKET the result of filling in BINARY-SPEC with DATA."
  (process-send-string socket (bindat-pack binary-spec data)))

(defun spread-trim (string)
  "Strip control characters from the end of STRING, ruturning a new string."
  (if (string-match "^\\([[:print:]]*\\)[[:cntrl:]]*$" string)
      (match-string 1 string)
      string))

(defun spread-make-filter (connection)
  "Make a lexical closure that can process data arriving for CONNECTION."
  (lexical-let ((connection connection))
    (lambda (socket data)
      ;; append the incoming data to our buffer
      (with-current-buffer (process-buffer socket)
        (goto-char (point-max))
        (insert data)
        (let ((last-tick nil))
          ;; keep processing the buffer until SPREAD-HANDLE-DATA runs
          ;; without consuming any data from it
          (while (not (eql last-tick (buffer-chars-modified-tick)))
            (setf last-tick (buffer-chars-modified-tick))
            (spread-handle-data connection socket)))))))

(defun spread-make-sentinel (connection)
  "Make a lexical closure that can process status changes for CONNECTION."
  (lexical-let ((connection connection))
    (lambda (process event)
      (when (spread-connection-connection-callback connection)
        (funcall (spread-connection-connection-callback connection)
                 (case (process-status process)
                   ((open)    :connected)
                   ((closed)  :disconnected)
                   ((connect) :connecting)
                   ((failed)  :failed)
                   (t         :unknown))
                 nil)))))

(defun spread-little-endian-p (byte)
  "Check the magic first BYTE from an incoming message header."
  (not (zerop (logand byte #x80))))

(defun spread-handle-data (connection socket)
  "Process incoming data event, after buffer has been updated.
This is where all of our incoming protocol logic logic and state
machine is implemented.  CONNECTION holds the current state, and
SOCKET can be used to access the buffer of incoming data."
  (let ((buffer (process-buffer socket)))
    (ecase (spread-connection-state connection)
      ((:error)
       (message "TODO -- in error state, received data"))
      ((:expect-authentication)
       (when (bio-data-ready-p buffer)
         (let ((auth-length (bio-peek-u8 buffer)))
           (when (> (bio-bytes-available buffer) auth-length)
             (bio-read-u8 buffer)
             (bio-read-bytes buffer auth-length)
             (process-send-string socket
                                  (format "NULL%s" (make-string 86 0)))
             (setf (spread-connection-state connection)
                   :expect-acceptance)))))
      ((:expect-acceptance)
       (when (bio-data-ready-p buffer)
         (let ((acceptance (bio-read-u8 buffer)))
           (if (= acceptance 1)
               (setf (spread-connection-state connection) :expect-version)
             (setf (spread-connection-state connection) :error)
             (when (spread-connection-connection-callback connection)
               (funcall (spread-connection-connection-callback connection)
                        :error
                        (format "Received error message %d while logging in"
                                acceptance)))))))
      ((:expect-version)
       (when (>= (bio-bytes-available buffer) 3)
         (let ((major-version (bio-read-u8 buffer))
               (minor-version (bio-read-u8 buffer))
               (patch-version (bio-read-u8 buffer)))
           (cond ((and (=  major-version 3)
                       (>= minor-version spread-min-minor-version))
                  (setf (spread-connection-state connection) :expect-group))
                 (t
                  (setf (spread-connection-state connection) :error)
                  (when (spread-connection-connection-callback connection)
                    (funcall (spread-connection-connection-callback connection)
                             :error
                             (format "Server %d.%d.%d incompatible with client %d.%d.%d"
                                     major-version
                                     minor-version
                                     patch-version
                                     spread-major-version
                                     spread-minor-version
                                     spread-patch-version))))))))
      ((:expect-group)
       (when (bio-data-ready-p buffer)
         (let ((group-length (bio-peek-u8 buffer)))
           (when (>= (bio-bytes-available buffer) (1+ group-length))
             (bio-read-u8 buffer)
             (setf (spread-connection-private-name connection)
                   (bio-read-bytes buffer group-length))
             (setf (spread-connection-state connection) :connected)
             (when (spread-connection-connection-callback connection)
               (funcall (spread-connection-connection-callback connection)
                        :connected
                        nil))))))
      ((:connected)
       (when (>= (bio-bytes-available (process-buffer socket))
                 spread-header-size)
         (let* ((first-byte (bio-peek-u8 buffer))
                (bindat-spec (if (spread-little-endian-p first-byte)
                                 spread-header-le-bindat-spec
                               spread-header-be-bindat-spec))
                (header (bio-peek-bytes buffer spread-header-size))
                (fields (bindat-unpack bindat-spec header))
                (service-type (bindat-get-field fields :service-type))
                (sender (bindat-get-field fields :group-name))
                (group-count (bindat-get-field fields :group-count))
                (message-type (bindat-get-field fields :message-type))
                (data-length (bindat-get-field fields :data-length)))
           ;; note: we have not consumed any data yet, just peeked at
           ;; the header; we will only consume data if all of the message
           ;; has arrived
           (when (>= (bio-bytes-available buffer)
                     (+ spread-header-size
                        (* group-count spread-max-group-name)
                        data-length))
             (bio-skip-bytes buffer spread-header-size)
             (let ((groups (loop repeat group-count collect
                                 (spread-trim
                                  (bio-read-bytes buffer
                                                  spread-max-group-name))))
                   (message (bio-read-bytes buffer data-length)))
               ;; TODO: should we use bindat rather than the awful
               ;; trim function above, to read these fixed-size
               ;; null-padded strings?
               (cond ((spread-regular-message-p service-type)
                      (when (spread-connection-message-callback connection)
                        (funcall (spread-connection-message-callback connection)
                                 (spread-bitmap->message-type service-type)
                                 sender
                                 groups
                                 message-type
                                 message)))
                     ((spread-membership-message-p service-type)
                      (when (spread-connection-membership-callback connection)
                        (funcall (spread-connection-membership-callback
                                  connection)
                                 (spread-bitmap->membership-type service-type)
                                 (spread-transition-p service-type)
                                 sender
                                 groups)))
                     (t (message "Ignoring unknown service type %d"
                                 service-type)))))))))))
  
(defun spread-make-sensible-name ()
  "Generate a unique name for this client session."
  "emacs") ;; TODO work harder -- need unique name 1-11 characters long

(defun spread-default-connection-callback (event message)
  "A default callback for connection events.
This serves as an example and documentation for writing connection callbacks.
EVENT is one of the following symbols:

  :connected -- connection establish
  :disconnected -- connection lost, or disconnected by server
  :failed -- network failure
  :network-unknown -- network unknown error
  :error -- error occurred (see message)"
  (message "Spread connection event: %s, %s" event message))

(defun spread-default-membership-callback (event transition sender groups)
  "A default callback for membership events.
This serves as an example and documentation for the interface
required of membership callbacks.

EVENT is a symbol from the set described below.  TRANSITION is a
boolean value (see Spread documentation to understand what this
means).  SENDER is the name of the node that generated the
membership event, and GROUPS is the list of affected groups.
EVENT is one of:

  :join -- a node has joined
  :leave -- a node has left
  :disconnect -- network disconnection/split between daemons
  :network -- network problem between daemons"
  (message "Spread membership event: %s, %s, %s, %s"
           event
           transition
           sender
           groups))

(defun spread-default-message-callback (event sender groups message-type
                                              message)
  "A default callback which simply prints messages out.
EVENT is the event type, SENDER is the name of the sender, GROUPS
is a list of destination groupnames as strings, MESSAGE-TYPE is
an integer classifying the message which was provided by the
client, and MESSAGE is the body of the incoming message as a
string.  This callback is provided as an example and a default,
but you can provide your own when calling SPREAD-CONNECT.  EVENT
is one of the following:

  :unreliable -- unreliable message received
  :reliable -- reliable message received
  :fifo -- fifo message received
  :casual -- casual message received
  :agreed -- agreed message received
  :safe -- safe message received"
  (message "Spread: event = %s sender = %s groups = %s type = %s message = %s"
           event
           sender
           groups
           message-type
           message))

;; Public API begins here

(defun* spread-connect (&key (host spread-default-host)
                             (port spread-default-port)
                             (name (spread-make-sensible-name))
                             (priority nil)
                             (membership nil)
                             (connection 'spread-default-connection-callback)
                             (message 'spread-default-message-callback))
  "Connect to a Spread daemon on HOST and PORT, with unique name NAME.
All of the arguments are optional and have sensible defaults.

There are three types of event callback, all of which have a
default implementation, and all of which can optionally be set to
nil to prevent delivery.  Their keys are are:

  :membership -- events caused by joining or leaving of groups
  :connection -- events caused by the network or disconnection
  :message -- incoming messages

See the documentation for SPREAD-DEFAULT-MEMBERSHIP-CALLBACK,
SPREAD-DEFAULT-CONNECTION-CALLBACK and
SPREAD-DEFAULT-MESSAGE-CALLBACK to read about the function
interfaces expected.

If PRIORITY is true, the connection will be a high priority
connection (see Spread documentation, I don't believe this does
anything in the current version of the Spread daemon).

The object that is returned can be
used to send messages with SPREAD-MULTICAST, and can be
disconnected with SPREAD-DISCONNECT."
  (when (> (length name) spread-max-private-name)
    (error "Spread-connect -- name too long"))
  (let ((buffer-name (format "*spread:%s:%d*" host port)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (set-buffer-multibyte nil))
    (let* ((connection (make-spread-connection
                        :message-callback message
                        :membership-callback membership
                        :connection-callback connection
                        :state :expect-authentication))
           (filter (spread-make-filter connection))
           (sentinel (spread-make-sentinel connection))
           (socket (make-network-process :host host
                                         :service port
                                         :coding 'binary
                                         :filter filter
                                         :filter-multibyte nil
                                         :sentinel sentinel
                                         :name buffer-name
                                         :buffer buffer-name)))
      (setf (spread-connection-socket connection) socket) ;; circular, TODO
      (spread-send socket
                   spread-login-request-bindat-spec
                   `((:major-version . ,spread-major-version)
                     (:minor-version . ,spread-minor-version)
                     (:patch-version . ,spread-patch-version)
                     (:flags . ,(logior (if membership spread-MEMBERSHIP 0)
                                        (if priority   spread-PRIORITY 0)))
                     (:name-length . ,(length name))
                     (:name . ,name)))
      connection)))

(defun* spread-ready-p (&optional (connection spread-default-connection))
  "Test if a connection is ready for multicasting."
  (eq (spread-connection-state connection) :connected))

(defun* spread-multicast (message
                          &key
                          (connection spread-default-connection)
                          (service :reliable)
                          (groups nil)
                          (group nil)
                          (message-type 0)
                          (self-discard t))
  "Multicast MESSAGE to all subscribers."
  ;; we want to allow either :group (a single group) or :groups (a list)
  ;; so we might as well allow both to be used at the same time by
  ;; CONSing
  (unless (spread-ready-p connection)
    (error "Spread-multicast -- not ready"))
  (let ((use-groups (if group (cons group groups) groups))
        (service-number (logior (spread-service-type->bitmap service)
                                (if self-discard spread-SELF_DISCARD 0))))
    (spread-send (spread-connection-socket connection)
                 spread-multicast-bindat-spec
                 `((:service-type . ,service-number)
                   (:group-name . ,(spread-connection-private-name connection))
                   (:group-count . ,(length use-groups))
                   (:message-type . ,message-type)
                   (:data-length . ,(length message))
                   (:groups . ,(mapcar (lambda (group)
                                         `((:name . ,group)))
                                       use-groups))
                   (:data . ,message)))))

(defun* spread-join (group &optional (connection spread-default-connection))
  "Join GROUP (a string naming the group) using CONNECTION."
  (spread-multicast ""
                    :connection connection
                    :service :join
                    :group group))

(defun* spread-leave (group &optional (connection spread-default-connection))
  "Leave GROUP (no longer receive messages for that group) using CONNECTION."
  (spread-multicast ""
                    :connection connection
                    :service :leave
                    :group group))

(defun* spread-disconnect (&optional (connection spread-default-connection))
  "Disconnect from the Spread daemon CONNECTION."
  (delete-process (spread-connection-socket connection))) ;; too subtle?

(provide 'spread)

;;; spread.el ends here
