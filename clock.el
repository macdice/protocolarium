;;; clock.el --- Time algorithms for Emacs Lisp
;; Copyright (c) 2009, 2010 Thomas Munro

;;
;; UNDER CONSTRUCTION -- MAY GIVE INCORRECT RESULTS, I'M STILL
;; WORKING ON UNIT TESTS TO TEST THIS CODE, AND THERE ARE SOME
;; CASES WHERE THE ZONEINFO PARSER FAILS
;;

;; Author: Thomas Munro <munro@ip9.org>
;; Keywords: time, clock, zoneinfo, timezone

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; THE SHORT VERSION
;;
;; You can use this library to find the current time in far away
;; places, with handy auto-completion:
;;
;;   M-x clock-what-time-is-it-in
;;   Pacific/Auckland
;;   => The time in Pacific/Auckland is 2009-11-08 14:20:01 NZDT.
;;
;; It does this by reading Zoneinfo[0] timezone files (as shipped with
;; most Unix systems), where are usually kept up to date as part of
;; your operating system.  Zoneinfo files catalogue the timezones of
;; the world, with large tables of daylight savings transition times
;; and sometimes also leap-second insertions.  The present library
;; provides a decoder for the binary file format, and implements a set
;; of basic algorithms for working with the dataset, and has a
;; built-in leap second table for timescale conversion.
;;
;; THE LONG VERSION
;;
;; This library is the fruit of my on-going struggle to understand
;; time systems and Zoneinfo.  It provides conversions between the
;; following time representations:
;;
;; 1. POSIX 1003 seconds since the Unix epoch (skipping UTC leap seconds)
;; 2. SI seconds since the Unix epoch (counting leap seconds)
;; 3. POSIX-like seconds since the Common Lisp epoch (skipping leap seconds)
;; 4. TAI time elements and strings
;; 5. UTC time elements and strings (at offset 0 or any timezone)
;; 6. GPS time elements and strings
;;
;; The available time conversion are shown on the following graph.
;; The time representations on the left are seconds since a 'start of
;; epoch' moment in the past (varying in the point of origin and in
;; the treatment of UTC leap seconds), and are represented in Emacs
;; Lisp with a double-precision float.  The time representations on
;; the right are 'decoded' times (year, month, date, hour, minute,
;; second, microsecond), and are represented as structs of type
;; CLOCK-TIME, or as ISO 8601 strings.
;;
;;   SECONDS SINCE AN EPOCH                     TIME ELEMENTS OR STRINGS
;;
;;   (3) 'cl-seconds'                           (5) 'zulu-time', 'local-time'
;;   (seconds since the Common                  (time elements on the UTC time
;;   Lisp epoch began at        --------------- scale, either at offset zero
;;   1900-01-01T00:00:00Z        \           /  or at a selected timezone or
;;   not counting leap seconds)   \\       //   offset)
;;           |                     \ \   / /
;;           |                      \  X  /
;;   (1) 'posix-seconds'             X   X      (6) 'gps-time'
;;   (seconds since the            /  \ /  \    (time elements on the Global
;;   Unix epoch began at        <------X------> Positioning Sytem time
;;   1970-01-01T00:00:00Z          \  / \  /    scale)
;;   not counting leap seconds)      X   X
;;           |                      /  X  \
;;           |                     / /   \ \
;;   (2) 'si-seconds'             //       \\   (4) 'tai-time'
;;   (seconds since the          /           \  (time elements on the Temps
;;   Unix epoch began at        --------------- Atomique International time
;;   1970-01-01T00:00:00Z                       scale)
;;   counting leap seconds)
;;
;; OVERVIEW OF FUNCTIONS
;;
;; 1. Getting the current system time in seconds since an epoch
;;
;;   (clock-posix-seconds)* -> seconds since Unix epoch ignoring leap sec.
;;   (clock-si-seconds)     -> seconds since Unix epoch counting leap sec.
;;   (clock-cl-seconds)     -> seconds since CL epoch ignoring leap seconds
;;
;;   * this is the same as Emacs (float-time) and matches POSIX time_t
;;     values from POSIX.1 time(3) and gettimeofday, and it is the
;;     usual representation of time on Unix-like systems
;;
;; 2. Loading timezones from Zoneinfo files
;;
;;   (clock-load-timezone <name>) -> timezone
;;   (clock-find-timezone-names)  -> list of available timezone names
;;
;; 3. Time scale conversions (different epochs or leap second policies)
;;
;;   (clock-posix-seconds->si-seconds <posix-seconds>)*
;;   (clock-posix-seconds->cl-seconds <posix-seconds>)
;;   (clock-si-seconds->posix-seconds <si-seconds>)**
;;   (clock-si-seconds->cl-seconds    <si-seconds)**
;;   (clock-cl-seconds->posix-seconds <cl-seconds>)
;;   (clock-cl-seconds->si-seconds    <cl-seconds>)*
;;
;;   * these cannot result in output values that are leap seconds
;;   ** these collapse each leap second into a single point in time
;;
;; 4. Converting from seconds since an epoch into time elements
;;
;; 4.1. UTC time scale, at offset zero
;;
;;   (clock-posix-seconds->zulu-time  <posix-seconds>)*
;;   (clock-si-seconds->zulu-time     <si-seconds>)
;;   (clock-cl-seconds->zulu-time     <cl-seconds>)
;;
;;   * this corresponds to POSIX.1 gmtime(3)
;;
;; 4.2. UTC time scale, at the appropriate local offset
;;
;;   (clock-posix-seconds->local-time <posix-seconds> [<timezone>])*
;;   (clock-si-seconds->local-time    <si-seconds>    [<timezone>])
;;   (clock-cl-seconds->local-time    <cl-seconds>    [<timezone>])
;;
;;   * this corresponds to POSIX.1 localtime(3)
;;
;; 4.3. TAI time scale
;;
;;   (clock-posix-seconds->tai-time   <posix-seconds>)
;;   (clock-si-seconds->tai-time      <si-seconds>)
;;   (clock-cl-seconds->tai-time      <cl-seconds>)
;;
;; 4.4. GPS time scale
;;
;;   (clock-posix-seconds->gps-time   <posix-seconds>)
;;   (clock-si-seconds->gps-time      <si-seconds>)
;;   (clock-cl-seconds->gps-time      <cl-seconds>)
;;
;; 5. Converting time elements into seconds since an epoch
;;
;; * this corresponds to POSIX tzset + mktime, and is what most
;;   time libraries provide
;;
;;   (clock-time->posix-seconds  <time> [<timezone>])*
;;   (clock-time->posix-seconds* <time> <policy> [<timezone>])**
;;   (clock-time->si-seconds     <time> [<timezone>])
;;   (clock-time->si-seconds*    <time> <policy> [<timezone>])**
;;   (clock-time->cl-seconds     <time> [<timezone>])
;;   (clock-time->cl-seconds*    <time> <policy> [<timezone>])**
;; 
;;   * this corresponds to POSIX.1 mktime(3) and gmtime(3)
;;   ** The starred variants of the functions take a policy for
;;      resolving daylight savings ambiguities
;;
;; 6.  Converting between time elements and strings
;;
;;   (clock-string->time <string>) -> decoded-time
;;   (clock-time->string <time>)   -> ISO 8601 string
;;
;; AMBIGUITY WHEN MOVING BETWEEN REPRESENTATIONS
;;
;; When moving between POSIX and SI time since the Unix epoch,
;; information is lost around the leap seconds, as the following table
;; shows:
;;
;;   TAI time               UTC time               POSIX seconds SI seconds
;;   2009-01-01 00:00:32.0  2008-12-31 23:59:59.0  1230767999.0  1230768022.0
;;   2090-01-01 00:00:32.5  2008-12-31 23:59:59.5  1230767999.5  1230768022.5
;;   2009-01-01 00:00:33.0  2008-12-31 23:59:60.0  ?             1230768023.0
;;   2009-01-01 00:00:33.5  2008-12-31 23:59:60.5  ?             1230768023.5
;;   2009-01-01 00:00:34.0  2009-01-01 00:00:00.0  1230768000.0  1230768024.0
;;
;; In the examples shown with question marks here, there is no real
;; answer because POSIX 1003 doesn't address leap seconds, but in
;; order to provide a reasonable answer the function
;; clock-si-time->posix-time 'stops' during the leap
;; second, and therefore returns 1230768000.0 for both question marks.
;; The opposite conversion, clock-posix-time->si-time, TODO
;;
;; When encoding times that are expressed in local time at a given
;; Zoneinfo timezone without an explicit offset or abbreviation, the
;; time can be ambiguous during daylight savings 'fall back' hours;
;; the ambiguity can be resolved by specifying a policy, or you can
;; ask for all possible encodings (see documentation).
;;
;; LIMITATIONS
;;
;; * UTC time was defined at the beginning of 1972; Temps Atomique
;;   International was synchronised with Universal Time in 1958; this
;;   library naively projects TAI and UTC timescales into the past
;;   before those events (note that the Unix epoch start time is
;;   retrospectively defined by POSIX in terms of the UTC time scale
;;   even though it didn't exist then!)
;;
;; * supports Zoneinfo version 0 (this means that it ignores the
;;   64-bit information that comes in the second parts of version 2
;;   files, so it can't see projected DST transitions after 2038)
;;   (FIXME)
;;
;; * uses whole numbers but floating point type (because Emacs integer
;;   type is only 29 bits wide on 32 bit systems, not enough for
;;   working with epoch times); TODO say more on the implications
;;
;; * leap second support is based on a hard-coded leap second table,
;;   which will need to be updated if there any more leap seconds are
;;   added, because Zoneinfo tzdata distributions do not always
;;   include this information (although there is talk of abandoning
;;   leap seconds in favour of leap hours, which will make a
;;   hard-coded table more appropriate)
;;
;; ZONEINFO TERMINOLOGY
;;
;; abbreviation = a short uppercase nickname for an offset; for
;;      example, EST (Eastern Standard Time), EDT (Eastern
;;      Daylight-Savings Time), BST (British Summer Time), NZDT (New
;;      Zealand Daylight-Savings Time); these are not really as useful
;;      as a Zoneinfo timezone name because (1) they are ambiguous
;;      (IST = Indian Standard Time, Israeli Standard Time; also many
;;      abbreviations have been used historically to indicate
;;      different offsets even in the same place) and (2) it's
;;      complicated to decide which one applies in a given city at a
;;      given moment expressed as local time, which is why Zoneinfo
;;      supports the concept of timezones which package up the rules
;;
;; offset = the number of (POSIX?) seconds of difference between UTC
;;      Zulu time and a particular local time
;;
;; timezone = a collection of offsets (and their abbreviations), and a
;;      list of time segments in which these offsets apply; this
;;      allows for annual daylight savings changes and historical
;;      policy changes; timezones are identified by continent/city
;;      names like Europe/London, Australia/Sydney, America/New_York
;;  
;; INTERESTING BUT UNSUPPORTED TIME SCALES
;;
;; The following time scales are based on the rotation of the earth,
;; as measured in different ways.  The earth does not rotate at a
;; perfectly constant speed, so the elapsed time between adjacent
;; seconds on these time scales is not exactly constant.  These time
;; scales are of interest to scientists and standards bodies.
;;
;; UT0 = a timescale based on astronomical observations at Greenwich,
;;       England; one second in UT0 time is the time taken for the
;;       earth to rotate by one 86400th (which is not constant)
;;
;; UT1 = UT0 with technical corrections relating to 'polar motion'
;;
;; GMT = a timescale based on solar observations at Greenwich,
;;       England, which was largely superseded in 1972 by UTC
;;       (confusingly, GMT is also an abbreviation for the zero offset
;;       used in the UK and Ireland during the winter; in this usage
;;       it is synonymous with UTC Zulu time and implies the UTC time
;;       scale)
;;
;; SUPPORTED TIME SCALES
;;
;; The following time scales are based on the Systeme International
;; d'Unites which defines one second using atomic clocks.  They vary
;; in their approach to the mismatch between atomic time and the
;; earth's rotations (respectively: continuous unpredictable
;; adjustments, smoke and mirrors, denial, one-off adjustment).
;;
;; UTC = a 'coordinated' universal time; in order to keep UTC days in
;;       phase with the earth's rotation (within 0.9 seconds of UT1),
;;       some UTC minutes have 61 seconds because a "leap second" is
;;       inserted (and there could theoretically be a 59 second minute
;;       but this has not happened yet); most UTC days therefore have
;;       86400 seconds but some have 86401 and theoretically 86399 is
;;       possible; this is the time scale used for civil time
;;       everywhere in the world (with a complex system of offsets
;;       controlled by politicians on top to create the local time for
;;       each city/region); people might also say 'UTC' to refer to
;;       the UTC offset of zero, rather than the time scale itself
;;
;; POSIX or Unix time = a compromise based on the UTC timescale which
;;       ignores leap seconds and defines that there are always 60
;;       seconds per minute and 86400 seconds per day, but cannot
;;       address or count leap seconds; software like 'ntpd' helps
;;       computer systems to pretend that leap seconds do not exist
;;       while allowing them to tell the right time almost all of the
;;       time; when expressed as time elements, all expressible POSIX
;;       times match their UTC equivalents, but it is not possible to
;;       express leap second times (second element = 60); when
;;       expressed as seconds since the epoch, POSIX times do not
;;       match their UTC equivalents as all the leap seconds that
;;       occurred since the epoch are not counted in the POSIX model
;;
;; TAI = a time scale based on SI seconds like UTC, but which does not
;;       have leap seconds and therefore its midday is gradually
;;       falling out of phase with the earth's rotations and UTC; its
;;       minutes are always 60 seconds long and its days are always
;;       86400 seconds long
;;
;; GPS = a time scale that incorporated all UTC leap seconds at the
;;       time it was designed in 1980, but none since, resulting in a
;;       fixed offset from TAI [2]
;;
;; In the vast majority of cases, computer software only needs to deal
;; with POSIX epoch time and UTC time elements (at appropriate local
;; time or at Zulu time).  I implemented the other systems in this
;; library as an exercise, and they allow for theoretically correct
;; answers to questions like 'how many second elapsed between noon
;; 2008-12-31T12:00:00Z and noon 2009-01-01T12:00:00Z?' (the answer is
;; 86401, because there was a leap second at 2008-12-31T23:59:60Z).

;; SIMILAR LIBRARIES IN LISPS
;;
;; * Common Lisp http://common-lisp.net/project/local-time/ -- Uses
;;   Zoneinfo, but does not handle TAI or UTC ambiguities or leap
;;   seconds (I started writing clock.el after submitting a patch to
;;   local-time to improve handling of timezone transitions, but that
;;   proved controversial -- it was eventually accepted, but got me
;;   thinking about the problem, and this library is my new prototype)
;;
;; * Scheme SRFI-19 -- Supports UTC and TAI time conversions,
;;   understands leap seconds, but does not provide timezone rule
;;   support (user supplied offsets only)
;;
;; * Timezone.el in GNU Emacs -- Supports local and Zulu timezone
;;   conversions and many string representations of dates, but does
;;   not know about timezone rules (ie it handles user supplied
;;   offsets only) or leap seconds
;;
;; FUTURE PLANS AND OPEN QUESTIONS
;;
;; * use binary searches in various places for potential performance
;;   increase
;;
;; * cache clock-timezone objects that have been loaded from disk
;;
;; * should we instead focus on better support for Zoneinfo-supplied
;;   leap second tables (even though they are not commonly installed)?

;; REFERENCES
;;
;; * http://www.cliki.net/Documentation%20Tool
;; * http://www.iers.org/MainDisp.csl?pid=36-25788&prodid=16 [5]
;; * http://naggum.no/lugm-time.html
;; * http://en.wikipedia.org/wiki/Leap_second                [4]
;; * http://en.wikipedia.org/wiki/Unix_time                  [6]
;; * http://www.cis.udel.edu/~mills/leap.html
;; * http://gpsinformation.net/main/gpstime.htm              [2]
;; * http://www.tondering.dk/claus/cal/calendar29.txt        [1]
;; * http://www.twinsun.com/tz/tz-link.htm                   [0]
;; * http://www.hermetic.ch/cal_stud/jdn.htm                 [3]
;; * http://www.lispworks.com/documentation/HyperSpec/Body/f_dec_un.htm
;; * http://www.lispworks.com/documentation/HyperSpec/Body/f_encode.htm

;; BUGS
;;
;; * does not support leap second deletion (ie if the Earth Rotation
;;   Service in Paris ever decides to declare a 'missing' second, ie a
;;   UTC minute that wraps after second number 58, then this library
;;   will not give correct results); from what I can tell that it
;;   currently unlikely, and the whole concept of leap seconds may
;;   soon go away, so perhaps it's not worth doing this unless it
;;   happens...

;; Improvements, suggestions etc gladly received.  I have no doubt
;; that I have overlooked something or confused some subtle or glaring
;; detail.

;;; History:
;;

(require 'cl)                                 ; for loop, defstruct, setf

;;; Code:

(defvar clock-search-path '("/usr/share/zoneinfo" "/usr/share/lib/zoneinfo")
  "The list of paths that are searched for compiled Zoneinfo files.")

(defvar clock-default-timezone-path "/etc/localtime"
  "The path used to load the default timezone.")

(defvar clock-default-timezone nil
  "The default timezone used in all functions taking an optional timezone.
This is lazily loaded on first use.")

(defconst clock-leap-table-bulletin 38
  "The latest IERS Bulletin C announcement that is incorporated
into our leap table.")

(defconst clock-leap-table-posix
  [78796800.0
   94694400.0
   126230400.0
   157766400.0
   189302400.0
   220924800.0
   252460800.0
   283996800.0
   315532800.0
   362793600.0
   394329600.0
   425865600.0
   489024000.0
   567993600.0
   631152000.0
   662688000.0
   709948800.0
   741484800.0
   773020800.0
   820454400.0
   867715200.0
   915148800.0
   1136073600.0
   1230768000.0]
  "The POSIX times of the leap seconds that have been inserted into UTC time.
This data is computed from the values in
  CLOCK-LEAP-TABLE-STRING.  Since POSIX time does not
  actually count leap seconds, these numbers are the address of
  the moment in time where the leap second happened but was not
  counted.")

(defconst clock-leap-table-si
  [78796801.0
   94694402.0
   126230403.0
   157766404.0
   189302405.0
   220924806.0
   252460807.0
   283996808.0
   315532809.0
   362793610.0
   394329611.0
   425865612.0
   489024013.0
   567993614.0
   631152015.0
   662688016.0
   709948817.0
   741484818.0
   773020819.0
   820454420.0
   867715221.0
   915148822.0
   1136073623.0
   1230768024.0]
  "The SI/UTC times (ie SI seconds since the start of the Unix
epoch) of the end of each leap second that has been inserted
  into UTC time.  This data is computed from the values in
  CLOCK-LEAP-TABLE-STRING.")

(defun clock-get-default-timezone ()
  "Lazy-load the default timezone."
  (when (null clock-default-timezone)
    (setf clock-default-timezone
          (clock-load-timezone-from-file clock-default-timezone-path)))
  clock-default-timezone)

(defstruct clock-timezone
  "An object representing a timezone loaded from a compiled Zoneinfo file."
  ;; Note that we don't currently use the leap second table from here
  ;; because it seems to be blank on many systems, we have our own
  ;; leap second tables
  (transitions)       ; vector of offset transition times
  (leap-seconds)      ; vector of POSIX times where leap seconds occur
  (leap-counts)       ; vector of leap seconds
  (transition-types)  ; vector of time types at each transition
  (all-types))        ; vector of all time types

(defstruct clock-timetype
  "An object representing a Zoneinfo time type."
  (abbreviation)      ; "EST", "PDT", "GMT" etc
  (offset)            ; offset from UTC Zulu time in seconds
  (daylight-savings)) ; whether this is daylight savings time

(defstruct clock-time
  "An object representing a decoded time, AKA time elements.  This is
equivalent to the POSIX struct tm, or the list returned by Common Lisp's
DECODE-UNIVERSAL-TIME."
  (year 0)            ; 1900 for 1900
  (month 0)           ; 1 for January
  (day 0)
  (hour 0)
  (minute 0)
  (second 0)
  (microsecond 0)
  (daylight-savings nil)
  (offset)
  (abbreviation)
  (timescale))

;; Code for reading Zoneinfo files

(defun clock-read-uint8 ()
  "Read the character at the point.  Used for treating buffers like streams."
  (let ((result (char-after)))
    (if (null result)
        (error "End of file while reading TZif file"))
    (forward-char)
    result))

(defun clock-read-uint32 ()
  "Read the next four bytes as a big endian unsigned 32 bit integer."
  (+ (lsh (clock-read-uint8) 24)
     (lsh (clock-read-uint8) 16)
     (lsh (clock-read-uint8) 8)
     (clock-read-uint8)))

(defun clock-read-uint32-as-float ()
  "Read the next four bytes as a big endian unsigned 32 bit integer,
and return it as a float so that Emacs can handle large numbers even
on 32 bit systems (where Emacs integers are limited to 29 bits)."
  (+ (* (float (clock-read-uint8)) 16777216.0)
     (* (float (clock-read-uint8)) 65536.0)
     (* (float (clock-read-uint8)) 256.0)
     (clock-read-uint8)))

(defun clock-read-int32-as-float ()
  "Read the next four bytes as a big endian signed 32 bit integer,
and return it as a float."
  (let ((unsigned (clock-read-uint32-as-float)))
    (if (> unsigned 2147483648.0)
        (- unsigned 4294967296.0)
        unsigned)))

(defun clock-read-uint32-vector (size)
  "Read SIZE uint32 values and return them in a new vector of floats."
  (vconcat (loop repeat size collect (clock-read-uint32-as-float))))

(defun clock-read-uint8-vector (size)
  "Read SIZE uint8 values and a return them in a vector of integers."
  (vconcat (loop repeat size collect (clock-read-uint8))))

(defun clock-read-string (size)
  "Read a string of length SIZE."
  (concat (loop repeat size collect (clock-read-uint8))))

(defun clock-nt-substring (string offset)
  "Return the substring of STRING beginning at OFFSET terminated by null."
  (first (split-string (substring string offset) "\000")))

(defun clock-load-timezone-from-file (path)
  "Load a timezone object from the specified PATH."
  ;; The format of TZif files is described in the Man page TZFILE(5) and
  ;; the header "tzinfo.h" on Unix systems.
  (with-temp-buffer
    (insert-file-contents path)
    ;; Check magic file header.
    (unless (string= (buffer-substring 1 5) "TZif")        ; tzh_magic
      (error "Not a TZif file"))
    ;; Skip that and the 16 bytes reserved for future use.
    (loop repeat 20 do (clock-read-uint8))       ; tzh_reserved
    ;; TODO check that the first reserved byte is 0 or 2?
    ;; Read the header which tells us how much data follows.
    (let ((number-of-utc-indicators (clock-read-uint32))  ; tzh_ttisgmtcnt
          (number-of-std-indicators (clock-read-uint32))  ; tzh_ttissdnt
          (number-of-leap-seconds   (clock-read-uint32))  ; tzh_leapcnt
          (number-of-transitions    (clock-read-uint32))  ; tzh_timecnt
          (number-of-time-types     (clock-read-uint32))  ; tzh_typecnt
          (size-of-abbrev-string    (clock-read-uint32))) ; tzh_chrcnt
      ;; Read the data tables.
      (let ((transitions (clock-read-uint32-vector number-of-transitions))
            (type-indexes (clock-read-uint8-vector number-of-transitions))
            (ttinfos (loop repeat number-of-time-types
                           collect (list (clock-read-int32-as-float)
                                         (clock-read-uint8)
                                         (clock-read-uint8))))
            (abbreviations (clock-read-string size-of-abbrev-string))
            (leap (loop repeat number-of-leap-seconds
                        collect (list (clock-read-uint32-as-float)
                                      (clock-read-uint32-as-float))))
            (stdwall (clock-read-uint8-vector number-of-std-indicators))
            (utc-table (clock-read-uint8-vector number-of-utc-indicators)))
        ;; Resolve the indexes to pointers to objects
        (let* ((all-types (map 'vector
                               (lambda (ttinfo)
                                 (make-clock-timetype
                                  :offset (first ttinfo)
                                  :daylight-savings (= 1 (second ttinfo))
                                  :abbreviation (clock-nt-substring
                                                 abbreviations
                                                 (third ttinfo))))
                               ttinfos))
               (types (map 'vector
                           (lambda (index)
                             (aref all-types index))
                           type-indexes)))
          (make-clock-timezone
           :transitions transitions
           :transition-types types
           :all-types all-types
           :leap-seconds (map 'vector #'first leap)
           :leap-counts (map 'vector #'second leap)))))))

(defun clock-find-files (path prefix)
  "Return a list of all files below a path."
  (apply #'append
         (mapcar
          (lambda (filename)
            (cond ((or (string= filename ".") (string= filename "..")) nil)
                  ((file-directory-p (concat path "/" prefix filename))
                   (clock-find-files (concat path "/" filename)
                                        (concat prefix filename "/")))
                  (t (list (concat prefix filename)))))
          (directory-files path))))

(defun clock-find-timezone-names ()
  "Return a list of available Olson timezone names."
  (sort
   (apply #'append
          (loop for path in clock-search-path
                if (file-directory-p path)
                collect (clock-find-files path "")))
   #'string<))

(defun clock-load-timezone (name)
  "Load a timezone object from the Zoneinfo file identified by NAME.
This function searches for a subdirectory/file matching NAME in all paths
defined in CLOCK-SEARCH-PATH."
  (loop for base in clock-search-path do
        (let ((path (format "%s/%s" base name)))
          (if (file-exists-p path)
              (return (clock-load-timezone-from-file path))))
        finally
        (error "Cannot find Zoneinfo file for %s" name)))

;; Fuctions for getting the current time in seconds since an epoch from
;; the operating system

(defun clock-posix-seconds ()
  "Return the number of SI seconds since the Unix epoch without leap seconds.
This can be used to decode POSIX time into its elements (but cannot be used
to decode TAI, UTC or GPS time)."
  ;; GNU Emacs uses gettimeofday (see editfns.c and systime.h) which
  ;; returns POSIX time
  (float-time))

(defun clock-si-seconds ()
  "Return the number of SI seconds since the Unix epoch with leap seconds."
  ;; Since we don't have access to such a clock, we use the POSIX time
  ;; and then adjust for leap seconds; this means that you get the right
  ;; result almost all of the time, just not during a leap second (time
  ;; may appear to stand still for one second, or some other wierdnes,
  ;; depending on how your operating system or ntpd decide to handle this)
  (clock-posix-seconds->si-seconds (clock-posix-seconds)))

(defun clock-cl-seconds ()
  "Return the number of seconds since the Lisp epoch without leap seconds.
The Common Lisp epoch for universal time purposes is 1900-01-01 0:00:00 Zulu
time on the UTC scale."
  (clock-posix-seconds->cl-seconds (clock-posix-seconds)))

(defconst clock-posix-universal-delta 2208988800.0
  "The number of seconds between 1900-01-01 (where Common Lisp universal time
starts) and 1970-01-01 (where the Unix epoch starts).")

(defconst clock-julian-number-of-unix-epoch 2440588
  "The Julian day number for 1970-01-01.")

(defconst clock-gps-tai-offset 19
  "The fixed difference between TAI and GPS timescales.")

(defun clock-find-timetype (posix-time timezone)
  "Find the timetype that applies at POSIX-TIME in TIMEZONE."
  ;; TODO this could be a binary search
  (let ((transitions (clock-timezone-transitions timezone)))
    (loop for i from (1- (length transitions))
        downto -1 do
        (cond ((< i 0)
               (return (aref (clock-timezone-all-types timezone) 0)))
              ((<= (aref transitions i) posix-time)
               (return (aref (clock-timezone-transition-types timezone) i)))))))

(defun clock-decode-raw (time)
  "Gregorian calendar decoder which other decode functions are built from.
Assumes 86400 seconds per day, with 0 at Unix epoch.  Other decoders can be
built on top of this with adjustments for skew, leap seconds, timezone."
  (let* ((days-since-epoch (floor (/ time 86400.0)))
         (seconds-since-midnight (- time (* days-since-epoch 86400.0)))
         (h (floor (/ seconds-since-midnight 3600)))
         (min (floor (mod (/ seconds-since-midnight 60) 60)))
         (s (floor (mod seconds-since-midnight 60)))
         (u (floor (* 1000000 (- time (ffloor time)))))
         (julian-day (+ days-since-epoch clock-julian-number-of-unix-epoch))
         (a (+ julian-day 32044))               ; }
         (b (/ (+ (* 4 a) 3) 146097))           ; }
         (c (- a (/ (* b 146097) 4)))           ; }
         (d (/ (+ (* 4 c) 3) 1461))             ; }
         (e (- c (/ (* 1461 d) 4)))             ; }- JDN -> Gregorian [1]
         (f (/ (+ (* 5 e) 2) 153))              ; }
         (y (+ (* b 100) d -4800 (/ f 10)))     ; }
         (m (+ f 3 (- (* 12 (/ f 10)))))        ; }
         (d (+ e (- (/ (+ (* 153 f) 2) 5)) 1))) ; }
    (make-clock-time :year y :month m :day d
                     :hour h :minute min :second s :microsecond u)))

(defun clock-cl-seconds->local-time (cl-seconds &optional timezone)
  "Convert UNIVERSAL-TIME to decoded time elements at timezone TIMEZONE.
UNIVERSAL-TIME should be a number of POSIXoid seconds (ie SI seconds not
counting UTC leap seconds) since 1900-01-01 0:00:00."
  (clock-posix-seconds->local-time
   (clock-cl-seconds->posix-seconds cl-seconds)))

(defun clock-cl-seconds->zulu-time (cl-seconds)
  (clock-posix-seconds->zulu-time
   (clock-cl-seconds->posix-seconds cl-seconds)))

(defun* clock-si-seconds->tai-time (si-time)
  "Convert SI-TIME (seconds since the Unix epoch) to TAI elements."
  (let* ((posix-time (+ si-time 10))
         (result (clock-decode-raw posix-time)))
    (setf (clock-time-timescale result) 'TAI)
    result))

(defun clock-filter (predicate list)
  "Return a new list containing the values that satisfy PREDICATE from LIST."
  (cond ((null list) nil)
        ((funcall predicate (car list))
         (cons (car list) (clock-filter predicate (cdr list))))
        (t
         (clock-filter predicate (cdr list)))))

(defun clock-filter-by-daylight-savings (value posix-times timezone)
  "Return a new list of POSIX times at which DST is VALUE in TIMEZONE."
  (clock-filter
   (lambda (posix-time)
     (let ((timetype (clock-find-timetype posix-time timezone)))
       (eq value (clock-timetype-daylight-savings timetype))))
   posix-times))

(defun clock-encode-posix-time-raw (time)
  "Assumes 86400 seconds per day, with 0 at Unix epoch.  Other encoders can be
built on top of this with adjustments for skew, leap seconds, timezone."
  ;; Note when porting to Scheme or Common Lisp: this is based on
  ;; integer arithmetic (in particular integer division) which needs
  ;; to be done with a different operator in those languages
  (let* ((y (clock-time-year time))
         (m (clock-time-month time))
         (d (clock-time-day time))
         (h (clock-time-hour time))
         (minute (clock-time-minute time))
         (s (clock-time-second time))
         (usec (clock-time-microsecond time))
         (a (+ y 4800 (/ (- m 14) 12)))                      ; }
         (b (/ (* a 1461) 4))                                ; }
         (c (/ (* 367 (- m 2 (* 12 (/ (- m 14) 12)))) 12))   ; }-> JDN [3]
         (x (+ y 4900 (/ (- m 14) 12)))                      ; }
         (e (/ (* 3 (/ x 100)) 4))                           ; }
         (julian-day (+ (- (+ b c) e) d -32075))             ; }
         (days-since-epoch (- julian-day clock-julian-number-of-unix-epoch))
         (seconds-since-midnight (+ (* 3600 h)
                                    (* 60 minute)
                                    s
                                    (/ usec 1000000.0))))
    (+ (* 86400.0 days-since-epoch) seconds-since-midnight)))

(defun clock-tai-time->posix-seconds (tai-time)
  "Compute the number of POSIX seconds since te Unix epoch at TAI-TIME."
  (clock-si-seconds->posix-seconds (clock-tai-time->si-seconds tai-time)))

(defun clock-tai-time->si-seconds (tai-time)
  "Compute the number of SI seconds since the Unix epoch at TAI-TIME.
Time should be a CLOCK-TIME structure using time scale TAI."
  (let* ((posix-seconds (clock-encode-posix-time-raw tai-time))
         (si-seconds (- posix-seconds 10)))
    si-seconds))

(defun* clock-utc-time->si-seconds
    (utc-time &optional (timezone (clock-get-default-timezone)))
  (let ((posix-seconds (clock-utc-time->posix-seconds utc-time timezone)))
    (if (= (clock-time-second utc-time) 60)
        (- (clock-posix-seconds->si-seconds posix-seconds) 1)
        (clock-posix-seconds->si-seconds posix-seconds))))

(defun* clock-utc-time->si-seconds*
    (utc-time policy &optional (timezone (clock-get-default-timezone)))
  (clock-posix-seconds->si-seconds
   (clock-utc-time->posix-seconds utc-time policy timezone)))

(defun* clock-string->cl-seconds
    (string-time &optional (timezone (clock-get-default-timezone)))
  "Convert a string representation of a time into the number of
POSIX seconds since the Common Lisp 'universal' epoch."
  (clock-time->cl-seconds (clock-string->time string-time) timezone))

(defun* clock-string->posix-seconds
    (string-time &optional (timezone (clock-get-default-timezone)))
  "Convert a string representation of a time into the number of
POSIX seconds since the POSIX/Unix epoch."
  (clock-time->posix-seconds (clock-string->time string-time) timezone))

(defun* clock-string->si-seconds
    (string-time &optional (timezone (clock-get-default-timezone)))
  "Convert a string representation of a time into the number of
POSIX seconds since the POSIX/Unix epoch."
  (clock-time->si-seconds (clock-string->time string-time) timezone))

(defun* clock-time->si-seconds
    (time &optional (timezone (clock-get-default-timezone)))
  "Compute the number of SI seconds since the POSIX/Unix epoch at TIME.
TIME may be in the UTC, TAI or GPS time scales.  TIMEZONE is only
used for UTC times."
  (ecase (clock-time-timescale time)
    ((UTC) (clock-utc-time->si-seconds time timezone))
    ((TAI) (clock-tai-time->si-seconds time))
    ((GPS) (clock-gps-time->si-seconds time))))

(defun* clock-time->posix-seconds
    (time &optional (timezone (clock-get-default-timezone)))
  "Compute the number of non-leap seconds since the POSIX/Unix epoch at TIME.
TIME may be in the UTC, TAI or GPS time scales.  TIMEZONE is only
used for UTC times."
  (ecase (clock-time-timescale time)
    ((UTC) (clock-utc-time->posix-seconds time timezone))
    ((TAI) (clock-tai-time->posix-seconds time))
    ((GPS) (clock-gps-time->posix-seconds time))))

(defun* clock-time->cl-seconds
    (time &optional (timezone (clock-get-default-timezone)))
  "Compute the number of non-leap seconds since the Common Lisp epoch at TIME.
TIME may be in the UTC, TAI or GPS time scales.  TIMEZONE is only
used for UTC times."
  (ecase (clock-time-timescale time)
    ((UTC) (clock-utc-time->cl-seconds time timezone))
    ((TAI) (clock-tai-time->cl-seconds time))
    ((GPS) (clock-gps-time->cl-seconds time))))

(defun* clock-utc-time->posix-seconds-list (time timezone)
  "Convert TIME to a list of POSIX time numbers, for TIMEZONE.
If TIME contains an offset, then there can be only one
interpretation.  Otherwise, if TIME contains an abbreviation,
then there can be only one interpretation, assuming that the
abbreviation is valid and unique for TIMEZONE.  Otherwise (if
neither of the above discriminators are provided), there should
be one result for most times, but zero or two results during the
DST transition hours, assuming typical DST configuration.  If you
need just a single result, see CLOCK-UTC-TIME->POSIX-SECONDS.
TIME is interpreted using the UTC time scale (even if it contains
a different time scale component)."
  (cond ((clock-time-offset time)
         (list (- (clock-encode-posix-time-raw time)
                  (clock-time-offset time))))
        ((clock-time-abbreviation time)
         (loop for timetype across (clock-timezone-all-types timezone)
               for offset = (clock-timetype-offset timetype)
               for posix-time = (- (clock-encode-posix-time-raw time)
                                   offset)
               if (and (string= (clock-timetype-abbreviation timetype)
                                (clock-time-abbreviation time))
                       (eq timetype
                           (clock-find-timetype posix-time timezone)))
               collect posix-time))
        (t
         (loop for timetype across (clock-timezone-all-types timezone)
               for offset = (clock-timetype-offset timetype)
               for posix-time = (- (clock-encode-posix-time-raw time)
                                   offset)
               if (eq timetype (clock-find-timetype posix-time timezone))
               collect posix-time))))

(defun* clock-utc-time->posix-seconds
    (time &optional (timezone (clock-get-default-timezone)))
  "Convert TIME to a POSIX time number using TIMEZONE.  If TIME
is partially specified (ie does not include an offset or an
offset abbreviation) and is ambiguous (ie could refer to a DST or
a non-DST time) then an arbitrary choice will be made (ie
policy :any).  This interface is designed for simple cases where
such ambiguous cases are not considered important.  To control
how ambiguities are resolved explicitly, see
CLOCK-UTC-TIME->POSIX-SECONDS* which takes a policy as an
argument, or CLOCK-UTC-TIME->POSIX-SECOND-LIST which returns all
possible interpretations as a list."
  (clock-utc-time->posix-seconds* time :any timezone))

(defun* clock-utc-time->posix-seconds*
    (time policy &optional (timezone (clock-get-default-timezone)))
  "Convert TIME to a POSIX time number, disambiguated with POLICY,
at TIMEZONE.
TIME must be of the timescale UTC.  If TIME contains an offset,
then the mapping to a POSIX time is unambiguous.  Because of the
way clocks roll back and forward for daylight savings and changes
for unusual political and sporting events, partially specified
times (that is, times that do not include an offset component)
can be ambiguous.  See CLOCK-UTC-TIME->POSIX-SECONDS-LIST if
you would like all possible interpretations of a partially
specified time, but for many use cases, users want to resolve
such times to a single point in time, so this function allows you
to specify an optional policy for resolving ambiguity.

The available policies are:
  :any -- just pick one, I don't care
  :first -- use the first time type listed in Zoneinfo
  :last -- use the last time type listed in Zoneinfo
  :earliest -- use the interpretation that gives the earliest time
  :latest -- use the interpretation that gives the latest time
  :prefer-daylight-savings -- interpret as DST if possible
  :prefer-not-daylight-savings -- interpret as non-DST if possible
  :daylight-savings -- interpret as DST only, or raise an error
  :not-daylight-savings -- interpret as non-DST, or raise an error
  :error -- raise an error if the time is ambiguous

You can avoid ambiguity by specifying an offset in TIME.  You can
usually avoid ambiguity by specifying an abbreviation in TIME, or
requesting daylight-savings or non-daylight-savings time (these
disambiguation policies rely on the Zoneinfo configuration not
providing multiple overlapping offsets that do not differ in
daylight savings or abbreviation, although a Zoneinfo file could
be contrived where this is not the case -- in this case we pick
the first result).  Not that :prefer-daylight-savings means that
we use a DST interpretation of the time only if necessary to
resolve an ambiguity, whereas :daylight-savings means that we
interpret the time as DST or fail."
  (let ((times (clock-utc-time->posix-seconds-list time timezone)))
    (when (null times)
      (error "clock-utc-time->posix-seconds* -- time is not valid"))
    (ecase policy
      ((:any :first)
       (first times))
      ((:last)
       (first (reverse times)))
      ((:earliest)
       (first (sort times #'<)))
      ((:latest)
       (first (sort times #'>)))
      ((:prefer-daylight-savings)
       (if (> (length times) 1)
           (first (clock-filter-by-daylight-savings t times timezone))
           (first times)))
      ((:prefer-not-daylight-savings)
       (if (> (length times) 1)
           (first (clock-filter-by-daylight-savings nil times timezone))
           (first times)))
      ((:daylight-savings)
       (let ((x (clock-filter-by-daylight-savings t times timezone)))
         (when (null x)
           (error "clock-utc-time->posix-seconds* -- not valid as DST"))
         (first x)))
      ((:not-daylight-savings)
       (let ((x (clock-filter-by-daylight-savings nil times timezone)))
         (when (null x)
           (error "clock-utc-time->posix-seconds* -- not valid as non-DST"))
         (first x)))
      ((:error)
       (if (> (length times) 1)
           (error "clock-utc-time->posix-seconds* -- time was ambiguous")
           (first times))))))

(defun* clock-utc-time->cl-seconds*
    (time policy &optional (timezone (clock-get-default-timezone)))
  (clock-posix-seconds->cl-seconds
   (clock-utc-time->posix-seconds* time policy timezone)))

(defun* clock-utc-time->cl-seconds
    (time &optional (timezone (clock-get-default-timezone)))
  (clock-posix-seconds->cl-seconds
   (clock-utc-time->posix-seconds time timezone)))

(defun clock-tai-time->cl-seconds (time)
  (clock-posix-seconds->cl-seconds (clock-tai-time->posix-seconds time)))

(defun clock-gps-time->si-seconds (time)
  (- (clock-tai-time->si-seconds time) clock-gps-tai-offset))

(defun clock-gps-time->cl-seconds (time)
  (- (clock-tai-time->cl-seconds time) clock-gps-tai-offset))

(defun clock-gps-time->posix-seconds (time)
  (- (clock-tai-time->posix-seconds time) clock-gps-tai-offset))

(defun clock-in-posix-leap-second-p (posix-time)
  "Check if a particular point in POSIX time is a leap second.  This requires
some qualification: it is utter bollocks."
  ;; TODO binary search
  (loop for time across clock-leap-table-posix
        if (< (abs (- time posix-time)) 0.000001)
        return t
        finally return nil))

(defun clock-in-si-leap-second-p (si-time)
  "Check if SI-TIME falls inside a leap second."
  ;; TODO binary search
  (loop for time across clock-leap-table-si
        if (and (>= si-time (- time 1)) (< si-time time))
        return t
        finally return nil))

(defun* clock-si-seconds->zulu-time (si-seconds)
  "Con..."
  (let* ((adjust (clock-in-si-leap-second-p si-seconds))
         (posix-seconds (clock-si-seconds->posix-seconds si-seconds))
         (result (clock-decode-raw (- posix-seconds (if adjust 1 0)))))
    ;; if the conversion to POSIX time resulted in a leap-second being
    ;; compressed into a single point, we need to adjust the time
    (when adjust
      (incf (clock-time-second result) 1)
      (setf (clock-time-microsecond result)
            (floor (* 1000000 (- si-seconds (ffloor si-seconds))))))
    (setf (clock-time-timescale result) 'UTC)
    (setf (clock-time-offset result) 0)
    (setf (clock-time-daylight-savings result) nil)
    result))

(defun clock-posix-seconds->zulu-time (posix-seconds)
  (let ((result (clock-decode-raw posix-seconds)))
    (setf (clock-time-timescale result) 'UTC)
    (setf (clock-time-offset result) 0)
    (setf (clock-time-daylight-savings result) nil)
    result))

(defun* clock-posix-seconds->local-time
    (posix-seconds &optional (timezone (clock-get-default-timezone)))
  (let ((timetype (clock-find-timetype posix-seconds timezone)))
    (unless timetype
      (error "clock-posix-seconds->local-time -- %d does not map to a timetype!"
             posix-seconds))
    (let ((result (clock-decode-raw
                   (+ posix-seconds (clock-timetype-offset timetype)))))
      (setf (clock-time-timescale result) 'UTC)
      (setf (clock-time-offset result)
            (clock-timetype-offset timetype))
      (setf (clock-time-daylight-savings result)
            (clock-timetype-daylight-savings timetype))
      (setf (clock-time-abbreviation result)
            (clock-timetype-abbreviation timetype))
      result)))

(defun* clock-si-seconds->local-time
    (si-time &optional (timezone (clock-get-default-timezone)))
  "Convert SI-TIME (seconds since the Unix epoch) to UTC time elements."
  ;; Because it's simpler to work with POSIX times, we convert to POSIX time
  ;; and then adjust the result if necessary
  (let* ((posix-time (clock-si-seconds->posix-seconds si-time))
         (result (clock-posix-seconds->local-time posix-time)))
    (if (clock-in-si-leap-second-p si-time)
        ;; we assume leap seconds are only ever added (true so far in 2009)
        ;; TODO compare cummulative leap second counts to check if it's
        ;; an add or delete
        (incf (clock-time-second result)))
    result))

;(defun clock-posix-time->string (posix-time offset)
;  (let ((elements (clock-decode-posix-time-with-offset posix-time
;                                                          offset)))
;    (format "%04d-%02d-%02dT%02d:%02d:%02d.%06dZ"
;            (clock-time-year elements)
;            (clock-time-month elements)
;            (clock-time-day elements)
;            (clock-time-hour elements)
;            (clock-time-minute elements)
;            (clock-time-second elements)
;            (clock-time-microsecond elements))))

;;; STRING <-> TIME CONVERSIONS

(defun* clock-time->string (time &key
                                 (space nil)
                                 (extended t)
                                 (comma nil))
  "Convert TIME into an ISO 8601 string.
Optionally, if SPACE is true then use a space rather than a T to
separate the date and time.  If EXTENDED is true, then put dashes
between date componenets and colons between time components.  If
COMMA is true, then use a comma rather than a dot to separate the
second the sub-second parts.  For UTC times, standard ISO offset
information will appear at the end if the offset it present in
TIME, but otherwise, if an abbreviation is present, it will
appear after a space (this is not standard ISO 8601).  Otherwise,
there will be no timezone information.  For TAI and GPS times,
the timescale indicator will appear at the end (this is a common
convention but is not covered by ISO 8601 which is based on the
UTC timescale)"
  (let ((space (if space " " "T"))
        (colon (if extended ":" ""))
        (dash  (if extended "-" ""))
        (dot   (if comma "," ".")))
    (format "%04d%s%02d%s%02d%s%02d%s%02d%s%02d%s%06d%s"
            (clock-time-year time)
            dash
            (clock-time-month time)
            dash
            (clock-time-day time)
            space
            (clock-time-hour time)
            colon
            (clock-time-minute time)
            colon
            (clock-time-second time)
            dot
            (clock-time-microsecond time)
            (ecase (clock-time-timescale time)
              ((UTC)
               (cond ((and (clock-time-offset time)
                           (zerop (clock-time-offset time)))
                      "Z")
                     ((clock-time-offset time)
                      (let* ((s (floor (clock-time-offset time)))
                             (h (abs (/ s 3600)))
                             (m (% (/ (abs s) 60) 60)))
                        (format "%s%02d%s%02d"
                                (if (plusp s) "+" "-")
                                h
                                colon
                                m)))
                     ((clock-time-abbreviation time)
                      (format " %s" (clock-time-abbreviation time)))
                     (t "")))
              ((TAI) " TAI")
              ((GPS) " GPS")))))

(defun clock-cl-seconds->posix-seconds (cl-seconds)
  "Convert a number of seconds from the Common Lisp epoch to POSIX epoch time.
The Common Lisp epoch starts at 1900-01-01T00:00:00Z while the
POSIX/Unix epoch starts at 1970-01-01T00:00:00Z, and both systems
ignore leap seconds, so this is a simple offset calculation."
  (- cl-seconds clock-posix-universal-delta))

(defun clock-posix-seconds->cl-seconds (posix-seconds)
  "Convert a number of seconds from the POSIX epoch to the Common Lisp epoch.
The Common Lisp epoch starts at 1900-01-01T00:00:00Z while the
POSIX/Unix epoch starts at 1970-01-01T00:00:00Z, and both systems
ignore leap seconds, so this is a simple offset calculation."
  (+ posix-seconds clock-posix-universal-delta))

(defun clock-si-seconds->posix-seconds (si-seconds)
  "Remove leap seconds so that SI-SECONDS since the Unix epoch
becomes a POSIX seconds since the Unix epoch.  This is a lossy
function, as leap seconds map to a single point in POSIX time.
For example, here are some points around the first leap second
in June 1972 which illustrate the behaviour:

  78796799.0 -> 78796799.0
  78796799.5 -> 78796799.5
  78796800.0 -> 78796800.0  }
  78796800.5 -> 78796800.0  }-- POSIX time stands still for 1 second
  78796801.0 -> 78796800.0  }
  78796801.5 -> 78796800.5
  78796802.0 -> 78796801.0

Compare this function with CLOCK-POSIX-SECONDS->SI-SECONDS which
performs the opposite mapping."
  ;; TODO this should be a binary search
  (loop for time across clock-leap-table-si
        for count from 0
        if (> time si-seconds)
        return (if (<= (- time 1) si-seconds)
                   (- time 1.0 count)
                   (- si-seconds count))
        finally return (- si-seconds (length clock-leap-table-si))))

(defun clock-posix-seconds->si-seconds (posix-time)
  "Add leaps seconds so that POSIX-TIME since the Unix epoch
becomes the number of SI seconds since the Unix epoch.  Because
POSIX time doesn't include leap seconds, there is a discontinuous
jump at leap seconds.  For example, here are some points around
the first leap second in June 1972 which illustrate the
behaviour:

  78796799.0 -> 78796799.0
  78796799.5 -> 78796799.5
  78796799.9 -> 78796799.9
  78796800.0 -> 78796801.0   <-- SI time jumps forward one second
  78796800.5 -> 78796801.5
  78796801.0 -> 78796802.0

Compare this function with CLOCK-SI-SECONDS->POSIX-SECONDS which
performs the opposite mapping."
  ;; TODO this should be a binary search
  (loop for time across clock-leap-table-posix
        for count from 0
        if (> time posix-time)
        return (+ count posix-time)
        finally return (+ posix-time (length clock-leap-table-posix))))

(defun clock-tail->us (s)
  "Convert string of numbers following a decimal point into a number."
  (floor (* (/ (string-to-number s) (expt 10.0 (length s))) 1000000)))

(defun clock-string->time (s)
  "Convert ISO 8601 time string S into a CLOCK-TIME object.
Time should be in the form YYYY-MM-DD HH:MM:SS, with several
optional adornments.  The space between date and time can be
replaced with the letter T, and the dashes and colons are
optional (see ISO 8601).  Subseconds can appear after the
seconds, separated by a dot or comma.  Timezone information can
be omitted, or appended in ISO 8601 style where Z means GMT, and
explicit offsets are indicated with + or - HH, HHMM or HH:MM.  As
a convenient extension you may instead use a space timezone
abbreviation such as EDT or EST, which will can be used to
discrimate ambiguous time representations (note that the
abbreviation must be a valid one in the timezone you plan to use
-- it is not used to identify a timezone, but only to select an
offset available within a timezone in conversion operations).

Instead of UTC offset information or an abbreviation, the second
or sub-second field can be followed by a space and either TAI or
GPS, to create a time using those timescales rather than the
default UTC timescale.  Offsets are not supported for TAI or GPS
timescales.  Any other name is interpreted as an abbreviation
name, and implies the UTC timescale.

Examples of acceptable strings:

  2009-11-14T12:00:00.000Z   -- UTC timescale, offset 0
  20091114T120000,000Z       -- UTC timescale, offset 0
  2009-11-14 12:00:00+01     -- UTC timescale, offset +1 hour
  2009-11-14 12:00:00+01:00  -- UTC timescale, offset +1 hour
  2009-11-14 12:00:00 EST    -- UTC timescale, unknown offset, EST abbr.
  2009-11-14 12:00:00 TAI    -- TAI timescale
  2009-11-14 12:00:00 GPS    -- GPS timescale

The resulting time object contains exactly the same information
as the string, but is in a form suitable for use by other
functions."
  ;; TODO put more common formats at the top of the COND form
  (cond
   ;; GPS without sub-seconds (this is not really ISO 8601)
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\) GPS$" s)
    (make-clock-time :timescale 'GPS
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond 0))
   ;; GPS with sub-seconds (this is not really ISO 8601)
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)[.,]\\([0-9]+\\) GPS$" s)
    (make-clock-time :timescale 'GPS
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond (clock-tail->us (match-string 7 s))))
   ;; TAI without sub-seconds (this is not really ISO 8601)
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\) TAI$" s)
    (make-clock-time :timescale 'TAI
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond 0))
   ;; TAI with sub-seconds (this is not really ISO 8601)
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)[.,]\\([0-9]+\\) TAI$" s)
    (make-clock-time :timescale 'TAI
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond (clock-tail->us (match-string 7 s))))
   ;; UTC with no timezone information, no sub-seconds
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond 0))
   ;; UTC with no timezone information, with sub-seconds
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)[.,]\\([0-9]+\\)$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond (clock-tail->us (match-string 7 s))))
   ;; UTC Zulu time, without sub-seconds
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)Z$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond 0
                     :offset 0))
   ;; UTC Zulu time, with sub-seconds
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)[.,]\\([0-9]+\\)Z$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond (clock-tail->us (match-string 7 s))
                     :offset 0))
   ;; UTC with hour offset, without sub-seconds
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)\\([+-][0-9][0-9]\\)$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond 0
                     :offset (* 3600 (string-to-number (match-string 7 s)))))
   ;; UTC with hour offset, with sub-seconds
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)[.,]\\([0-9]+\\)\\([+-][0-9][0-9]\\)$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond (clock-tail->us (match-string 7 s))
                     :offset (* 3600 (string-to-number (match-string 8 s)))))
   ;; UTC with hour:minute offset, without sub-seconds
   ((string-match "^\\([0-9][0-9][0-9][0-9]+\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)\\([+-]\\)\\([0-9][0-9]\\):?\\([0-9][0-9]\\)$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond 0
                     :offset (* (+ (* 3600 (string-to-number (match-string 8 s)))
                                   (* 60 (string-to-number (match-string 9 s))))
                                (if (string= (match-string 7 s) "-") -1 1))))
   ;; UTC with hour offset:minute, with sub-seconds
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)[.,]\\([0-9]+\\)\\([+-]\\)\\([0-9][0-9]\\):?\\([0-9][0-9]\\)$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond (clock-tail->us (match-string 7 s))
                     :offset (* (+ (* 3600 (string-to-number (match-string 9 s)))
                                   (* 60 (string-to-number (match-string 10 s))))
                                (if (string= (match-string 8 s) "-") -1 1))))
   ;; UTC with abbreviation, without sub-seconds (this is not really ISO 8601)
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\) \\([A-Z]+\\)$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond 0
                     :abbreviation (match-string 7 s)))
   ;; UTC with abbreviation, with sub-seconds (this is not really ISO 8601)
   ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-?\\([0-9][0-9]\\)-?\\([0-9][0-9]\\)[T ]\\([0-9][0-9]\\):?\\([0-9][0-9]\\):?\\([0-9][0-9]\\)[.,]\\([0-9]+\\) \\([A-Z]+\\)$" s)
    (make-clock-time :timescale 'UTC
                     :year (string-to-number (match-string 1 s))
                     :month (string-to-number (match-string 2 s))
                     :day (string-to-number (match-string 3 s))
                     :hour (string-to-number (match-string 4 s))
                     :minute (string-to-number (match-string 5 s))
                     :second (string-to-number (match-string 6 s))
                     :microsecond (clock-tail->us (match-string 7 s))
                     :abbreviation (match-string 8 s)))
   (t (error "clock-string->time -- cannot parse %s" s))))

;;; Interactive Emacs commands for tinkering with.

(defvar clock-interactive-history '()
  "A history buffer for timezone names entered in the minibuffer.")

(defun clock-what-time-is-it-in ()
  "A friendly interactive command for displaying the time in any town."
  (interactive)
  (let* ((timezone-name (completing-read "What time is it in... "
                                         (clock-find-timezone-names)
                                         nil
                                         t
                                         (car clock-interactive-history)
                                         'clock-interactive-history))
         (timezone (clock-load-timezone timezone-name))
         (time (clock-posix-seconds->local-time (clock-posix-seconds) timezone)))
    (message "The time in %s is %s %s."
             timezone-name
             (clock-time->string time)
             (clock-time-abbreviation time))))

(defun clock-time-panel ()
  "Pop-up a time panel showing times in various timezones."
  (interactive)
  42)

;;; BEGIN construction tools: The following data/code is only used to
;;; compute the leap second tables clock-leap-table-posix and
;;; clock-leap-table-si; it is not really part of the library.
;;; Ideally I would figure out how to have this done automatically at
;;; compile/expansion time.

(defconst clock-leap-table-string
  '("1972-06-30T23:59:60Z"
    "1972-12-31T23:59:60Z"
    "1973-12-31T23:59:60Z"
    "1974-12-31T23:59:60Z"
    "1975-12-31T23:59:60Z"
    "1976-12-31T23:59:60Z"
    "1977-12-31T23:59:60Z"
    "1978-12-31T23:59:60Z"
    "1979-12-31T23:59:60Z"
    "1981-06-30T23:59:60Z"
    "1982-06-30T23:59:60Z"
    "1983-06-30T23:59:60Z"
    "1985-06-30T23:59:60Z"
    "1987-12-31T23:59:60Z"
    "1989-12-31T23:59:60Z"
    "1990-12-31T23:59:60Z"
    "1992-06-30T23:59:60Z"
    "1993-06-30T23:59:60Z"
    "1994-06-30T23:59:60Z"
    "1995-12-31T23:59:60Z"
    "1997-06-30T23:59:60Z"
    "1998-12-31T23:59:60Z"
    "2005-12-31T23:59:60Z"
    "2008-12-31T23:59:60Z")
  "The table of UTC leap seconds[4].  This information is
  available in some tzdata distributions, but not all, so we have
  our own table.  Since 2000, the earth's speed has increased so
  that leap seconds are not being added as frequently as before.
  There is a possibility that the leap second system will soon be
  abolished, so this list may soon be frozen, for historical time
  computations only.  The International Earth Rotation Service
  announces new leap seconds every six months in IERS 'bulletin
  C' [5].")

(defun clock-make-posix-leap-table ()
  "Generate a vector of POSIX times which identify the point in
time where a leap second has been inserted."
  (vconcat (mapcar (lambda (s)
                     (clock-encode-posix-time-raw
                      (clock-string->time s)))
                   clock-leap-table-string)))

(defun clock-make-si-leap-table ()
  "Generate a vector of SI UNIX epoch times which identify the
point at the END of all leap seconds that has been inserted to
date."
  (vconcat (loop for posix-time across (clock-make-posix-leap-table)
                 for count from 1
                 collect (+ posix-time count))))

;;; END construction tools

(provide 'clock)

;;; clock.el ends here