;;; bio.el --- A set of buffer IO routines
;; Copyright (c) 2009 Thomas Munro

;; Author: Thomas Munro <munro@ip9.org>
;; Keywords: buffer, binary, io

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
;; This is my experimental library of input routines based on buffers.
;; The general contract is that you should check how much data is
;; available in the buffer before attempting to read (or peek) from it.
;; This is intented to support event-based input (ie 'reactor' style),
;; which is the natural way of processing input (from processes or network)
;; in Emacs.
;;
;; TODO naming -- BIO sounds like it involves output too; ehiggs suggested
;; bile (buffer input library for Emacs), following that line of thinking
;; could take me to bike (k = ?)...  search continues
;;
;; TODO tidy
;;
;; TODO this is quite inefficient code, I like the API but the
;; implementation needs some work

;;; History:
;; 

(require 'cl)

;;; Code:

(defun bio-read-u8 (buffer)
  "Read one byte from BUFFER.  A byte must be available, or an error is raised."
  (with-current-buffer buffer
    (when (= (point-max) (point-min))
      (error "Bio-read-u8 -- no data (programming error)"))
    (goto-char (point-min))
    (let ((result (char-after)))
      (delete-char 1)
      result)))

(defun bio-peek-u8 (buffer &optional start)
  "Peek one byte from BUFFER (read without consuming), beginning at START."
  (with-current-buffer buffer
    (goto-char (+ (point-min) (or start 0)))
    (char-after)))

(defun bio-peek-u16 (buffer &optional start)
  "Peek one unsigned 16 bit word from BUFFER, beginning at START."
  (with-current-buffer buffer
    (goto-char (+ (point-min) (or start 0)))
    (let ((a (char-after)))
      (forward-char)
      (+ (* 256 a) (char-after)))))

(defun bio-read-u16 (buffer)
  "Read one unsigned 16 bit word from BUFFER."
  (let* ((a (bio-read-u8 socket))
         (b (bio-read-u8 socket)))
    (+ (* 256 a) b)))

(defun bio-peek-u32 (buffer &optional start)
  "Peek on unsigned 32 bit word from BUFFER, beginning at START.
Will not return correct values above a certain limit (2^28?) on
32 bit systems."
  ;; TODO rewrite me
  (with-current-buffer buffer
    (goto-char (+ (point-min) (or start 0)))
    (let ((a (char-after)))
      (forward-char)
      (let ((b (char-after)))
        (forward-char)
        (let ((c (char-after)))
          (forward-char)
          (let ((d (char-after)))
            (message "%d %d %d %d" a b c d)
            (+ (* 16777216 a) (* 65536 b) (* 256 c) d)))))))

(defun bio-data-ready-p (buffer)
  "Check if at least one byte can be read from BUFFER."
  (with-current-buffer buffer
    (not (= (point-max) (point-min)))))

(defun bio-bytes-available (buffer)
  "Check how many bytes are available to be read from BUFFER."
  (with-current-buffer (process-buffer socket)
    (- (point-max) (point-min))))

(defun bio-read-bytes (buffer bytes)
  "Read from BUFFER a string of BYTES contiguous bytes, into a string.
The data must be ready, or an error will be raised."
  (with-current-buffer buffer
    (unless (> (point-max) bytes)
      (error "Bio-read-bytes -- data not available (programming error)"))
    (let ((result (buffer-substring-no-properties (point-min)
                                                  (+ (point-min) bytes))))
      (delete-region (point-min) (+ (point-min) bytes))
      result)))

(defun bio-peek-bytes (buffer bytes)
  "Read data from BUFFER without consuming it.
Read from the buffer associated with BUFFER a string of BYTES
contiguous bytes into a string.  The data must be ready, or an
error will be raised."
  (with-current-buffer buffer
    (unless (> (point-max) bytes)
      (error "Bio-read-bytes -- data not available (programming error)"))
    (let ((result (buffer-substring-no-properties (point-min)
                                                  (+ (point-min) bytes))))
      result)))

(defun bio-skip-bytes (buffer bytes)
  "Skip over data in BUFFER for BYTES bytes.
That number of bytes must exist in the buffer, or an error will
be raised."
  (with-current-buffer buffer
    (unless (>= (point-max) bytes)
      (error "Bio-skip-bytes -- data not available (programming error)"))
    (delete-region (point-min) (+ (point-min) bytes))))

(provide 'bio)

;;; bio.el ends here
