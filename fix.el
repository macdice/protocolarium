;;; fix.el --- A toy implementation of the FIX protocol for Emacs Lisp
;;
;; Copyright (c) 2004, 2009 Thomas Munro
;;
;; Author: Thomas Munro <munro@ip9.org>
;; Created: 2004
;; Version: 0.0004
;; Keywords: FIX protocol
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;; 
;; Here's a toy I cooked up at home in my underground Parenthesis Lab
;; to study FIX repeating groups and to learn more about Emacs.  It
;; models FIX messages as nested lists.  It is a toy and supports just
;; enough FIX for testing and amusement.
;;
;; More information about the FIX protocol:
;;   * http://www.fixprotocol.org/
;;   * http://en.wikipedia.org/wiki/FIX_protocol
;;
;;; 1.  CONNECTIONS, SENDING AND RECEIVING FIX MESSAGES
;;
;; You can initiate a new connection like this:
;;
;;   (fix-connect "localhost" 9999 "FIX.4.2" "US" "THEM" <function>)
;;
;; Then you can send a standard logon message like this:
;;
;;   (fix-logon <connection>)
;;
;; Or you can start a server that accepts one connection like this:
;;
;;   (fix-start-server "localhost" 9999 "FIX.4.2" "US" "THEM" <function>)
;;
;; Then you can send messages like this (the message type is passed
;; separately, not in the list of fields, because it's part of the
;; header which is automatically added during conversion to wire
;; format).
;;
;;   (fix-send <connection> "S" '((55 "GOOG") (131 "12345")))
;;
;; Incoming messages will be delivered to the function supplied when
;; connection, in the form of a list.  You can process the message
;; using any of the usual Lisp list processing hammers, or use these
;; supplied functions:
;;
;;   (fix-type message)
;;   => "R"
;;
;;   (fix-get message 55)
;;   => "GOOG"
;;
;; Hint: When setting up a connection in an interactive Emacs session,
;; it can be fun to use a symbol rather than a procedure object (that
;; is, 'my-handler rather than #'my-handler or (lambda ...)), because
;; that way you can change your handler function by reevaluating a
;; defun form as often as you like without having to restart the
;; existing connection.
;;
;;; 2.  SUGAR
;;
;; A string-literal case syntax is provided to make switching on
;; message type easier:
;;
;;   (fix-string-case (fix-type message)
;;     (("R")
;;      (message "A QuoteRequest arrived"))
;;     (("Z")
;;      (message "A QuoteCancel arrived"))
;;     (t
;;      (message "An message of type %s arrived" (fix-type message))))
;;
;; Symbols can be used in place of integer for fix field identifiers,
;; if a dictionary is provided to convert them.  See the example
;; dictionary definition in fix-fields-44.el covering the standard
;; FIX.4.4 names - typically these would need to be extended for local
;; customisations.
;;
;;   (require 'fix-fields-44)
;;   (defvar dict (fix-dictionary fix-fields-44))
;;
;;   (fix-message-names dict '((35 "R") (132 "100.00")))
;;   => ((MsgType "R") (BidPx "100.00"))
;;
;;   (fix-message-numbers dict '((MsgType "R") (BidPx "100.00")))
;;   => ((35 "R") (132 "100.00"))
;;
;;; 3.  REPEATING GROUPS
;;
;; Messages containing repeating groups are normally represented in
;; 'flat' format (as they appear in real FIX message), but there is an
;; optional nested sub-lists format, where the 'NoXXX' field contains
;; a sub-list.  To transform incoming messages to nested format, you
;; have to process incoming messages explicitly with:
;; 
;;   (fix-extrude <message> <definitions>)
;;
;; ... where <definitions> is a list of repeated group definitions
;; from the appropriate FIX specification, such as:
;;
;;   ((146 55 65 48 22 454 460 461 167 ...)     ; NoRelatedSymbols ...
;;    (454 455 456 ...))                        ; NoSecurityAltID ...
;;
;; This contrived example message, in flat format (as it would arrive)
;;
;;   ((11 "AAA")
;;    (22 "BBB")
;;    (33 "2")
;;    (10 "XXX")
;;    (20 "YYY")
;;    (666 "3")
;;    (777 "x")
;;    (777 "y")
;;    (777 "z")
;;    (10 "FOO")
;;    (20 "BAR")
;;    (44 "CCC"))
;;
;; when extruded with the group definitions
;;
;;   ((33 10 20 666)
;;    (666 777))))
;;
;; becomes
;;
;;   ((11 "AAA")
;;    (22 "BBB")
;;    (33 (((10 "XXX")
;;          (20 "YYY")
;;          (666 (((777 "x"))
;;                ((777 "y"))
;;                ((777 "z")))))
;;         ((10 "FOO")
;;          (20 "BAR"))))
;;    (44 "CCC")).
;;
;; Such messages can be explicitly flattened to reverse this
;; translation with
;;
;;   (fix-flatten <message>)
;;
;; ... but this is done automatically when sending messages so you
;; don't normally need to flatten messages explicitly.  Whether or not
;; it's actually useful to work with nested format lists is another
;; question.  I wrote the extrude algorithm just as a kind of exercise
;; to understand the FIX specification better.
;;
;;; History:
;; 
;; I first hacked this stuff up a long time ago in Scheme, a pleasing
;; and precise language, but having it in my editor was an
;; irresistable prospect.  I've tweaked it over the years, ported it
;; to Python, various Lisps, but ultimately found the Emacs version to
;; be the most useful and abandoned the others (those other languages
;; can access real FIX implementations anyway, so no need for this
;; toy).

(require 'bio)
(require 'cl)

;;; Code:

(defconst fix-delimiter 1
  "The standard delimiter character for FIX messages, the ASCII code SOH.")

(defconst fix-delimiter-string (make-string 1 fix-delimiter)
  "The standard delimiter characterfor FIX messages, as a string.")

(defconst fix-end-pattern (concat "\\("
                                  fix-delimiter-string 
                                  "10=[0-9]+"
                                  fix-delimiter-string
                                  "\\)")
  "The regular expression marking the end of a message.")

(defstruct fix-connection
  "A struct for an active connection."
  (socket)
  (version)
  (sender-comp-id)
  (target-comp-id)
  (sequence)
  (handler)
  (buffer "")
  (client-name))

(defun fix-make-filter (connection)
  "Private constructor for process filter function for CONNECTION."
  (lexical-let ((connection connection))
    (lambda (process data)
      (bio-append (process-buffer process) data)
      (loop for data = (bio-read-until-regex (process-buffer process) 
                                             fix-end-pattern)
            while data
            do (funcall (fix-connection-handler connection)
                        (fix-string->list data))))))

(defun fix-make-sentinel (connection)
  "Private constructor for process sentinal function for CONNECTION."
  (lexical-let ((connection connection))
    (lambda (process event)
      (funcall (fix-connection-handler connection)
               `((35 "CONNECTION_STATUS")
                 (0 ,(process-status process)))))))

(defun fix-connect (host port version sender-comp-id target-comp-id f)
  "Initiate a network session to a remote FIX server and return a connection.
HOST and PORT specify the network endpoint; VERSION should be a string
like FIX.4.4, and SENDER-COMP-ID and TARGET-COMP-ID are the logon
credentials.  Incoming messages will be passed to your function F as lists."
  (let ((buffer-name (format "*%s:%s:%s*" 
                             version 
                             sender-comp-id 
                             target-comp-id)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (set-buffer-multibyte nil))
    (let* ((connection (make-fix-connection :version version
                                            :sender-comp-id sender-comp-id
                                            :target-comp-id target-comp-id
                                            :sequence 0
                                            :handler f))
           (filter (fix-make-filter connection))
           (sentinel (fix-make-sentinel connection))
           (socket (make-network-process :host host
                                         :service port
                                         :coding 'binary
                                         :filter filter
                                         :filter-multibyte nil
                                         :sentinel sentinel
                                         :name buffer-name
                                         :buffer buffer-name)))
      (setf (fix-connection-socket connection) socket)
      connection)))
      
(defun fix-start-server (host port version sender-comp-id target-comp-id f)
  "Create a server which will allow incoming connections.
The server will listen on interface HOST PORT, using a VERSION (eg FIX.4.4),
with credentials SENDER-COMP-ID and TARGET-COMP-ID.  Incoming messages
will be passed to your function F in the form of a list.  For now, only one
session can be accepted per server (but you can create more than one server
with different ports).
TODO: This was removed, it got out of date, I need to come back and redo this!"
  (error "fix-start-server -- no longer implemented (TODO, redo)"))

(defun fix-stop-server (connection)
  "Stop a CONNECTION that was created with 'fix-start-server'."
  (delete-process (fix-connection-socket connection)))

(defun fix-time ()
  "Build a FIX 'UTCTimestamp' string for the current time."
  (format-time-string "%Y%m%d-%H:%M:%S" (current-time) t))

(defun fix-logon (connection)
  "Send a standard logon message to CONNECTION."
  (fix-send connection "A" `((52 ,(fix-time))
                             (98 "0")
                             (108 "30")
                             (141 "Y"))))

(defun fix-logoff (connection)
  "Sends a logof message to CONNECTION."
  (fix-send connection "5" `((52 ,(fix-time))
                             (58 "Goodbye"))))

(defun fix-list->string (list)
  "Given a message LIST such as ((1 \"X\") (2 \"Y\")), build a FIX string."
  (mapconcat (lambda (pair)
               (concat (number-to-string (first pair))
                       "="
                       (second pair)
                       fix-delimiter-string))
             list
             ""))

(defun fix-string->list (s)
  "Convert a FIX message S into a list of (TAG VALUE) lists."
  ;; TODO detect malformed messages
  ;; TODO handle messages with '=' in the value
  ;; TODO handle presence or absence of trailng 
  (mapcar (lambda (tag=value)
            (let ((pair (split-string tag=value "=")))
              (list (string-to-number (first pair)) (second pair))))
          (split-string (substring s 0 -1) ; remove trailing delimiter
                        fix-delimiter-string)))

(defun fix-funny-string->list (s)
  "Message constructor taking string S with line breaks and mangled delimiters.
Might be useful for dealing with copy-and-pasted mangled messages from
a QuickFIX log when viewed with 'less' or similar.  But probably not."
  (let* ((joined (apply #'concat (split-string s "\n")))
         (tidied (replace-regexp-in-string "\\^A"
                                           fix-delimiter-string
                                           joined)))
    (fix-string->list tidied)))

(defun fix-maybe-get (message tag)
  "Search list MESSAGE and return value for TAG if it is present, or nil."
  (cond ((null message) nil)
        ((= (caar message) tag) (cadar message))
        (t (fix-maybe-get (cdr message) tag))))

(defun fix-get (message tag)
  "Search list MESSAGE and return value for TAG if it is present, or error."
  (or (fix-maybe-get message tag)
      (error "Fix-get -- cannot find tag %s" tag)))

(defun fix-type (message)
  "Return the FIX message type value from list MESSAGE."
  (fix-maybe-get message 35))

(defun fix-flatten (message)
  "Flatten any repeating groups in MESSAGE."
  (cond ((null message) nil)
        ((stringp (cadar message))
         (cons (car message) (fix-flatten (cdr message))))
        ((consp (cadar message))
         (append (list (list (caar message)
                             (number-to-string (length (cadar message)))))
                 (apply #'append (mapcar #'fix-flatten (cadar message)))
                 (fix-flatten (cdr message))))
        (t (error "Fix-flatten -- unexpected type: %s" (cadar message)))))

(defun fix-extrude (message definitions)
  "Take a flat message, as received from the wire, and convert it into a 
nested structure which represents repeating groups as sub-lists.  In order
to do this, a list of groups definitions must be supplied, so that the
beginning of a repeated group and the beginning and end of instances
can be detected.

The format of this definition is:
  ((<counter-field> <first-field> <field> ...)
   (<counter-field> <first-field> <field> ...)
    ...)"
  (labels ((read-instances (message instances definition)
             "Reads all the instances in a group limited by DEFINITION."
             (message "(read-instances %s %s %s)" message instances definitions)
             (multiple-value-bind (unconsumed instance) 
                 (read-group message nil (cdr definition))
               (if (null instance)
                   (read-group (cons (list field (reverse instances))) 
                               fields
                               definition)
                   (read-instances unconsumed 
                                   (cons instance instances)
                                   definition))))
           (read-group (message results valid-fields)
             "Read in a group, and return the unconsumed and processed fields."
             (message "(read-group %s %s %s)" message results valid-fields)
             (if (null message)
                 (values message (reverse results))
                 (let* ((field (first (first message)))
                        (definition (assoc field definitions)))
                   (message "got field %s" field)
                   (cond
                    ;; is this the start of a valid nested repeating group?
                    ((and definition
                          (or (null valid-fields)
                              (member field valid-fields)))
                       (read-instances (cdr message) nil definition))
                    ;; is this the start of a new instance?
                    ((and (not (null results))
                          (not (null valid-fields))
                          (= (first valid-fields) field))
                     (values message (reverse results)))
                    ;; is this a valid field for the current instance?
                    ((or (null valid-fields)
                         (member field valid-fields))
                     (read-group (cdr message)
                                 (cons (car message) results)
                                 valid-fields))
                    ;; we must be past the end of the repeating group
                    (t
                     (values message (reverse results))))))))
    (multiple-value-bind (unconsumed instance) 
        (read-group message nil nil)
      instance)))

(defun fix-list->string-with-header-and-footer (connection type message)
  "For a given CONNECTION, generate the string for TYPE and MESSAGE.
MESSAGE should be a list of (number string) lists, not including
any of the standard header or footer tags, which are
automatically supplied in the result."
  (let* ((body `((35 ,type)
                 (49 ,(fix-connection-sender-comp-id connection))
                 (56 ,(fix-connection-target-comp-id connection))
                 (34 ,(number-to-string (fix-connection-sequence connection)))
                 ,@message))
         (body-string (fix-list->string body))
         (header+body (format "8=%s%s9=%d%s%s"
                              (fix-connection-version connection)
                              fix-delimiter-string
                              (length body-string)
                              fix-delimiter-string
                              body-string))
         (header+body+footer (format "%s10=%03d%s"
                                     header+body
                                     (% (reduce #'+ header+body) 256)
                                     fix-delimiter-string)))
    header+body+footer))

(defun fix-send (connection type message)
  "Using CONNECTION and TYPE, send list MESSAGE to the counterparty.
MESSAGE should contain just user body tags, and may be in nested or
flat form (it will be flattened automatically), and may use tag numbers
or symbols (these will be converted to numbers automatically)."
  (incf (fix-connection-sequence connection))
  (process-send-string 
   (fix-connection-socket connection)
   (fix-list->string-with-header-and-footer connection
                                            type
                                            (fix-flatten message))))

;   (fix-list->string (fix-add-computed-fields connection
;                                              type
;                                              (fix-flatten message)))))

(defun fix-pretty-print (message &optional depth)
  "Print MESSAGE in a human friendly format indented to DEPTH spaces."
  (cond ((null message) nil)
        ((stringp (second (first message)))
         (message "%s%s = [%s]"
                  (make-string (or depth 0) 32)
                  (number-to-string (first (car message)))
                  (second (car message)))
         (fix-pretty-print (cdr message) depth))
        (t
         (fix-pretty-print (cdr message) depth))))

(defstruct fix-map
  "A two-way dictionary allowing for translation between symbols and numbers."
  (number->symbol (make-hash-table))
  (symbol->number (make-hash-table)))

(defun fix-dictionary (&rest lists)
  "Create a fast two-way map for symbol<->number translation.
Arguments in LISTS should contain (NUMBER STRING TYPE) lists.
For now TYPE is ignored, but could be used in future for magic conversions
of field values."
  (let ((map (make-fix-map)))
    (mapc (lambda (list)
            (mapc (lambda (entry)
                    (puthash (first entry)
                             (second entry)
                             (fix-map-number->symbol map))
                    (puthash (second entry)
                             (first entry)
                             (fix-map-symbol->number map)))
                  list))
          lists)
    map))

(defun fix-tag-name (dictionary number)
  "Given a DICTIONARY and a NUMBER, return the tag name as a symbol."
  (let ((result (gethash number (fix-map-number->symbol dictionary))))
    (or result number)))

(defun fix-tag-number (dictionary name)
  "Given a DICTIONARY and a symbol NAME, return the tag number."
  (let ((result (gethash name (fix-map-symbol->number dictionary))))
    (or result name)))

(defun fix-message-names (dictionary message)
  "Use DICTIONARY to convert all tag numbers in MESSAGE to names if possible."
  (mapcar (lambda (pair)
            (list (fix-tag-name dictionary (first pair))
                  (if (consp (second pair))
                      (fix-message-names dictionary (second pair))
                      (second pair))))
          message))

(defun fix-message-numbers (dictionary message)
  "Use DICTIONARY to convert all tag names in MESSAGE to numbers if possible."
  (mapcar (lambda (pair)
            (list (fix-tag-number dictionary (first pair))
                  (if (consp (second pair))
                      (fix-message-names dictionary (second pair))
                      (second pair))))
          message))

(defmacro fix-string-case (expression &rest clauses)
  "A simple string-case macro, which is good for dispatching on message type.
STRING should be a string expression, and CLAUSES
should be of the form ((<string> ...) <expression> ...) or optionally
in final position only (t <expression> ...).  This is similar to the
built-in 'case' form, except that it uses 'string=' instead of 'eql'."
  (let ((value (gensym "value")))
    `(let ((,value ,expression))
       (cond ,@(mapcar (lambda (clause)
                      (if (eq (first clause) 't)
                          `(t ,@(cdr clause))
                          `((or ,@(mapcar (lambda (key)
                                         (if (stringp key)
                                             `(string= ,key ,value)
                                             (error "fix-type-case -- expected string literals")))
                                       (first clause)))
                            ,@(cdr clause))))
                    clauses)))))

(provide 'fix)

;;; fix.el ends here
