;;; memcache.el --- A toy implementation of the Memcache client protocol
;; Copyright (c) 2009 Thomas Munro

;; Author: Thomas Munro <munro@ip9.org>
;; Keywords: memcache, network, protocol

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
;; This is a toy implementation of the Memcache protocol[1].  It is
;; very, very pre-alpha, and probably doesn't handle many situations
;; correctly, it's just a minimal implementation which helped me out
;; when I was doing some testing and wanted to interact with my Memcache
;; daemon from the IELM REPL.  Might grow into something more solid
;; some future rainy day...

;; The following systems apparently support the Memcache protocol:
;;
;; * memcached (the original)
;; * Tokyo Tyrant
;; * LightCloud
;; * MemcacheDB
;;
;; and probably more.

;; Examples of interactive usage:
;;
;;  ELISP> (memcache-connect-default "my-other-machine")
;;  #<process stream>
;;  ELISP> (memcache-set "test" "boo")
;;  "STORED"
;;  ELISP> (memcache-get "test")
;;  "boo"
;;  ELISP> (memcache-stats)
;;  (("pid" "1404")
;;   ("uptime" "1960")
;;   ("time" "1263643028")
;;   ("version" "1.2.2")
;;   ("pointer_size" "64")
;;   ... blah blah ...
;;
;;  ELISP> (memcache-close-default)
;;  nil
;;
;; Example of usage in a program that doesn't want to hold onto
;; a connection for a long time, using a scoped connection:
;;
;;   (memcache-with-connection ("my-other-machine")
;;     (memcache-set "foo" "bar"))
;;
;; You can also provide connection objects explicitly to MEMCACHE-SET
;; and MEMCACHE-GET, useful for talking to multiple Memcache daemons
;; at the same time.

;; TODO:
;; * fix the blocking reads!  not really stable without this
;; * support various other operations
;; * proper error handling
;; * learn about bytes versus characters in modern Emacsen
;; * support 'flags'
;; * support non-blocking callback style operations (more Emacsy)
;;
;; [1] http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt

;;; History:
;; 

;;; Code:

(require 'cl)

(defvar memcache-default-host "localhost"
  "The default host used by connect functions if you don't provide one.")

(defvar memcache-default-port 11211
  "The default port used by connect functions if you don't provide one.")

(defvar memcache-current-connection nil
  "A dynamic variable for the default connection.
This is optional -- you can provide a CONNECTION argument to all
functions, or you can leave it off if you want to use this
default argument.  The idea is to manage this variable are with
MEMCACHE-CONNECT-DEFAULT, MEMCACHE-CLOSE-DEFAULT and (for
programs) MEMCACHE-WITH-CONNECTION.")

(defconst memcache-key-pattern ".*"  ;; TODO
  "The regular expression for testing if keys are valid.")

(defvar memcache-timeout 2
  "Time to wait for a response from server before signalling an error.")

(defun* memcache-connect (&optional (host memcache-default-host)
                                    (port memcache-default-port))
  "Create a connection to a Memcache server (or similar) on HOST and PORT."
  ;; TODO buffer name should be unique for each connection!
  (let ((buffer-name (format "*memcache-stream*")))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (set-buffer-multibyte nil))
    (make-network-process :host host
                          :service port
                          :coding 'binary
                          :name buffer-name
                          :buffer buffer-name)))

(defun memcache-close (connection)
  "Close CONNECTION, opened with MEMCACHE-CONNECT."
  (delete-process connection))

(defun* memcache-connect-default (&optional (host memcache-default-host)
                                            (port memcache-default-port))
  "Connect to HOST on port PORT, setting the default connection.
This is mainly intended for interactive use in the REPL, as programs
probably shouldn't be using this global connection."
  (setq memcache-current-connection (memcache-connect host port)))

(defun memcache-close-default ()
  "Close the default connection that was created by MEMCACHE-CONNECT-DEFAULT."
  (memcache-close memcache-current-connection))

(defmacro* memcache-with-connection ((&optional (host memcache-default-host)
                                                (port memcache-default-port))
                                     &rest rest)
  "A syntax for creating a lexically scoped default connection and making
it the default while executing the body."
  `(progn
     (let ((memcache-current-connection (memcache-connect ,host ,port)))
       (unwind-protect
           (progn ,@rest)
         (memcache-close memcache-current-connection)))))

(defun memcache-read-line (connection)
  "Read one line synchronously from CONNECTION."
  ;; TODO the timeout code here is wrong, need to redesign this so that
  ;; it loops until MEMCACHE-TIMEOUT is reached, potentially after multiple
  ;; reads
  (with-current-buffer (process-buffer connection)
    (goto-char (point-min))
    (if (or (search-forward "\n" nil t)
            (and (accept-process-output connection memcache-timeout)
                 (goto-char (point-min))
                 (search-forward "\n" nil t)))
        (let ((result (buffer-substring-no-properties (point-min)
                                                      (- (point) 2))))
          (delete-region (point-min) (point))
          result)
      (error "Error reading line"))))

(defun memcache-read-bytes (connection bytes)
  "Read from CONNECTION a block of BYTES contiguous bytes, into a string."
  ;; TODO this is rubbish, needs a loop in case the data takes multiple
  ;; reads before it arrives
  (with-current-buffer (process-buffer connection)
    (goto-char (point-min))
    (if (or (>= (- (point-max) (point-min)) bytes)
            (and (accept-process-output connection memcache-timeout)
                 (goto-char (point-min))
                 (>= (- (point-max) (point-min)) bytes)))
        (let ((result (buffer-substring-no-properties (point-min)
                                                      (+ (point-min) bytes))))
          (delete-region (point-min) (+ (point-min) bytes))
          result)
      (error "Error reading data"))))

(defun memcache-check-key (key)
  "Check if KEY is a valid key."
  (unless (string-match memcache-key-pattern key)
    (error "Memcache-check-key -- invalid key")))

(defun* memcache-set (key
                      data
                      &optional
                      (flags 0)
                      (expiry 0)
                      (connection memcache-current-connection))
  "Stores KEY and DATA in the server."
  (memcache-check-key key)
  (process-send-string connection
                       (format "set %s %d %d %d\r\n"
                               key
                               flags
                               expiry
                               (length data)))
  (process-send-string connection data)
  (process-send-string connection "\r\n")
  ;; we return the raw line coming back from the daemon -- maybe we
  ;; should return T if the result is "STORED", or raise an error
  ;; if the result is anything else?
  (memcache-read-line connection))

(defun* memcache-get (key &optional (connection memcache-current-connection))
  "Retrieves the value associated with KEY from CONNECTION."
  (memcache-check-key key)
  (process-send-string (or connection memcache-current-connection)
                       (format "get %s\r\n" key))
  (let ((response (memcache-read-line connection)))
    (cond ((string-match "^VALUE \\([^ ]+\\) \\([0-9]+\\) \\([0-9]+\\)"
                         response)
           (if (string= key (match-string 1 response))
               (let* ((size (string-to-number (match-string 3 response)))
                      (data (memcache-read-bytes connection size)))
                 (memcache-read-line connection) ;; blank line
                 (let ((response (memcache-read-line connection)))
                   (unless (string= response "END")
                     (error "Expected END, got %s" response))
                   data))))
          ((string-match "^END$" response)
           nil)
          (t
           (error "Unexpected response %s" response)))))

(defun* memcache-stats (&optional (connection memcache-current-connection))
  "Retreive server statistics from CONNECTION."
  (process-send-string connection "stats\r\n")
  (loop for line = (memcache-read-line connection)
        while (string-match "^STAT \\([^ ]+\\) \\([^ ]+\\)" line)
        collect (list (match-string 1 line) (match-string 2 line))))

(provide 'memcache)

;;; memcache.el ends here
