;;; clock-test.el --- Time algorithms for Emacs Lisp (unit tests)
;; Copyright (c) 2009 Thomas Munro <munro@ip9.org>

;; See clock.el for clues.

(require 'cl)
(require 'clock)

(defun clock-test-fuzzy-equal-p (x y)
  "A sloppy check for equality between floating point numbers."
  (< (abs (- x y)) 0.0000001))

(defmacro clock-test-assert-error (&rest rest)
  "A syntax for asserting that an error is raised by an expression."
  `(unless (condition-case exception
               (progn ,@rest nil)
             ('error t))
    (error "Expected error but no error was raised: %S" 
           ',(cons 'clock-test-assert-error rest))))

(defun clock-test-independent ()
  "Run the self-tests that do not depend on external data.
These tests cover the time scales, calendar algorithms, and leap
second logic."

  ;; [1] check our leap second mapping functions
  (labels ((si=posix (si posix)
             (let ((computed-posix (clock-si-seconds->posix-seconds si)))
               (unless (clock-test-fuzzy-equal-p posix computed-posix)
                 (error "[1] Expected %f SI seconds -> %f POSIX seconds but got %f"
                        si
                        posix
                        computed-posix))))
           (posix=si (posix si)
             (let ((computed-si (clock-posix-seconds->si-seconds posix)))
               (unless (clock-test-fuzzy-equal-p si computed-si)
                 (error "[1] Expected %f POSIX seconds -> %f SI seconds but got %f"
                        posix
                        si
                        computed-si)))))
    (si=posix 78796799.0 78796799.0)
    (si=posix 78796799.5 78796799.5)
    (si=posix 78796800.0 78796800.0) ;; }
    (si=posix 78796800.5 78796800.0) ;; }-- flat bit in codomain
    (si=posix 78796801.0 78796800.0) ;; }
    (si=posix 78796801.5 78796800.5)
    (si=posix 78796802.0 78796801.0)
    (si=posix 78796803.0 78796802.0)
    (si=posix 94694400.0 94694399.0)
    (si=posix 94694400.5 94694399.5)
    (si=posix 94694401.0 94694400.0) ;; }
    (si=posix 94694401.5 94694400.0) ;; }-- flat bit in codomain
    (si=posix 94694402.0 94694400.0) ;; }
    (si=posix 94694402.5 94694400.5)
    (si=posix 94694403.0 94694401.0)

    (posix=si 78796799.0 78796799.0)
    (posix=si 78796799.5 78796799.5)
    (posix=si 78796799.9 78796799.9)
    (posix=si 78796800.0 78796801.0) ;; <-- discontinuous jump
    (posix=si 78796800.5 78796801.5)
    (posix=si 94694399.0 94694400.0)
    (posix=si 94694399.5 94694400.5)
    (posix=si 94694399.9 94694400.9)
    (posix=si 94694400.0 94694402.0) ;; <-- discontinuous jump
    (posix=si 94694400.5 94694402.5))

  ;; [2] check some known trivial time conversions
  (labels ((posix-seconds (string n)
             (let* ((time (clock-string->time string))
                    (posix-seconds (clock-time->posix-seconds time)))
               (unless (clock-test-fuzzy-equal-p posix-seconds n)
                 (error "[2] Expected %s -> %f POSIX seconds but got %f"
                        string
                        n
                        posix-seconds))))
           (tai/si (string n)
             (let* ((time (clock-string->time string))
                    (si-seconds (clock-tai-time->si-seconds time)))
               (unless (clock-test-fuzzy-equal-p si-seconds n)
                 (error "[2] Expected %s -> %f SI seconds but got %f"
                        string
                        n
                        si-seconds)))))
    (posix-seconds "1970-01-01T00:00:00.000000Z" 0.0)     ;; unix epoch begins
    (posix-seconds "1970-01-01T00:00:00.500000Z" 0.5)     ;; + half a second
    (posix-seconds "1970-01-01T00:00:01.000000Z" 1.0)     ;; + one second
    (posix-seconds "1970-01-01T00:01:00.000000Z" 60.0)    ;; + one minute
    (posix-seconds "1970-01-01T01:00:00.000000Z" 3600.0)  ;; + one hour
    (posix-seconds "1970-01-02T00:00:00.000000Z" 86400.0) ;; + one day
    (posix-seconds "2009-02-13T23:31:30.000000Z" 1234567890.0) ;; see slashdot
    (tai/si "1970-01-01T00:00:00.000000 TAI" -10.0)    ;; TAI starts off by 10
    (tai/si "1970-01-01T00:00:10 TAI" 0.0)
    (tai/si "1972-07-01T00:00:00 TAI" 78796790.0)      ;; important fencepost
    (tai/si "1972-07-01T00:00:01 TAI" 78796791.0)
    )

  ;; [3] check that we can successfully convert TAI times to si-seconds and back
  (labels ((round-trip (tai-string)
             (let* ((tai-time (clock-string->time tai-string))
                    (si-seconds (clock-tai-time->si-seconds tai-time))
                    (new-tai-time (clock-si-seconds->tai-time si-seconds))
                    (new-tai-string (clock-time->string new-tai-time)))
               (unless (string= tai-string new-tai-string)
                 (error "[3] Expected %s but got %s in round trip test"
                        tai-string
                        new-tai-string)))))
    (round-trip "1970-01-01T00:00:00.000000 TAI")
    (round-trip "2000-01-01T00:00:00.000000 TAI"))

  ;; [4] check our TAI/GPS/UTC timescale conversion (leap second fu)
  (labels ((tai=utc (tai-string utc-string)
             "Check TAI-STRING to UTC-STRING conversion, and vice-versa."
             (let* ((tai-time (clock-string->time tai-string))
                    (si-seconds (clock-tai-time->si-seconds tai-time))
                    (utc-time (clock-si-seconds->zulu-time si-seconds))
                    (computed-utc-string (clock-time->string utc-time)))
               (unless (string= computed-utc-string utc-string)
                 (error "[4] Expected %s -> %s but got %s" 
                        tai-string 
                        utc-string
                        computed-utc-string)))
             (let* ((utc-time (clock-string->time utc-string))
                    (si-seconds (clock-utc-time->si-seconds utc-time))
                    (tai-time (clock-si-seconds->tai-time si-seconds))
                    (computed-tai-string (clock-time->string tai-time)))
               (unless (string= computed-tai-string tai-string)
                 (error "[4] Exptected %s <- %s but got %s"
                        tai-string
                        utc-string
                        computed-tai-string)))))
    (tai=utc "1970-01-01T00:00:00.000000 TAI" "1969-12-31T23:59:50.000000Z")
    (tai=utc "1970-01-01T00:00:10.000000 TAI" "1970-01-01T00:00:00.000000Z")

    (tai=utc "1971-01-01T00:00:00.000000 TAI" "1970-12-31T23:59:50.000000Z")
    (tai=utc "1971-06-01T00:00:00.000000 TAI" "1971-05-31T23:59:50.000000Z")

    ;; interesting moments near the first leap second
    (tai=utc "1972-07-01T00:00:00.000000 TAI" "1972-06-30T23:59:50.000000Z")
    (tai=utc "1972-07-01T00:00:01.000000 TAI" "1972-06-30T23:59:51.000000Z")
    (tai=utc "1972-07-01T00:00:07.000000 TAI" "1972-06-30T23:59:57.000000Z")
    (tai=utc "1972-07-01T00:00:08.000000 TAI" "1972-06-30T23:59:58.000000Z")
    (tai=utc "1972-07-01T00:00:09.000000 TAI" "1972-06-30T23:59:59.000000Z")
    (tai=utc "1972-07-01T00:00:10.000000 TAI" "1972-06-30T23:59:60.000000Z") ;L
    (tai=utc "1972-07-01T00:00:10.500000 TAI" "1972-06-30T23:59:60.500000Z") ;L  
    (tai=utc "1972-07-01T00:00:11.000000 TAI" "1972-07-01T00:00:00.000000Z")

    ;; assertions from tables in web page at [6]
    (tai=utc "2004-09-17T00:00:30.750000 TAI" "2004-09-16T23:59:58.750000Z")
    (tai=utc "1999-01-01T00:00:30.750000 TAI" "1998-12-31T23:59:59.750000Z")
    (tai=utc "1999-01-01T00:00:31.000000 TAI" "1998-12-31T23:59:60.000000Z") ;L
    (tai=utc "1999-01-01T00:00:31.500000 TAI" "1998-12-31T23:59:60.500000Z") ;L
    )
  
  ;; [5] Timezone fu 
  ;; we make our own toy New York timezone rather than loading one
  ;; from disk since we don't want to depend on the user's Zoneinfo
  ;; installation here
  (let* ((EDT (make-clock-timetype :abbreviation "EDT" 
                                   :offset -14400.0 
                                   :daylight-savings t))
         (EST (make-clock-timetype :abbreviation "EST"
                                   :offset -18000.0
                                   :daylight-savings nil))
         (NY (make-clock-timezone :transitions [1205046000.0 
                                                1225605600.0 
                                                1236495600.0 
                                                1257055200.0]
                                  :transition-types (vector EDT EST EDT EST)
                                  :all-types (vector EDT EST))))
    (labels (;(zulu=local (zulu-string local-string timezone)
             ;  (let* ((zulu-time (clock-string->time zulu-string))
             ;         (zulu-posix-seconds (clock-
             (local=zulu (local-string zulu-string policy timezone)
               (let* ((local-time (clock-string->time local-string))
                      (local-posix (clock-utc-time->posix-seconds* local-time 
                                                                   policy 
                                                                   timezone))
                      (zulu-time (clock-string->time zulu-string))
                      (zulu-posix (clock-time->posix-seconds zulu-time
                                                             timezone)))
                 (unless (clock-test-fuzzy-equal-p local-posix zulu-posix)
                   (error "[5] Expected %s == %s with policy %s but former is %d seconds later"
                          local-string
                          zulu-string
                          policy
                          (- local-posix zulu-posix))))))

      ;; now we test the interpretation of partially specified times:
      ;; times where we didn't say which offset applies

      ;; --- "fall back", November 2009 ---
      ;; warm-up exercises, for illustration
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :any NY)
      (local=zulu "2009-11-01T00:30:00" "2009-11-01T04:30:00Z" :any NY)
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T05:00:00Z" :any NY) ; amb.
      (local=zulu "2009-11-01T01:30:00" "2009-11-01T05:30:00Z" :any NY) ; amb.
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :any NY)
      (local=zulu "2009-11-01T02:30:00" "2009-11-01T07:30:00Z" :any NY)
      (local=zulu "2009-11-01T03:00:00" "2009-11-01T08:00:00Z" :any NY)
      (local=zulu "2009-11-01T03:30:00" "2009-11-01T08:30:00Z" :any NY)

      ;; assert correct policy-based conversion during unambiguous EDT
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :any NY)
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :first NY)
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :earliest NY)
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :prefer-daylight-savings NY)
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :daylight-savings NY)
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :latest NY)
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :last NY)
      (clock-test-assert-error
       (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :not-daylight-savings NY))
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :prefer-not-daylight-savings NY)
      (local=zulu "2009-11-01T00:00:00" "2009-11-01T04:00:00Z" :error NY)

      ;; assert correct policy-based conversion of ambiguous times (ie
      ;; the time could be either 01:00:00 EDT or 01:00:00 EST)

      ;; these policies map 01:00:00 to 05:00:00 Zulu
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T05:00:00Z" :any NY)
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T05:00:00Z" :first NY)
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T05:00:00Z" :earliest NY)
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T05:00:00Z" :prefer-daylight-savings NY)
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T05:00:00Z" :daylight-savings NY)
      ;; these policies map 01:00:00 to 06:00:00 Zulu
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T06:00:00Z" :latest NY)
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T06:00:00Z" :last NY)
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T06:00:00Z" :not-daylight-savings NY)
      (local=zulu "2009-11-01T01:00:00" "2009-11-01T06:00:00Z" :prefer-not-daylight-savings NY)
      ;; this policy refuses to contemplate ambiguous times
      (clock-test-assert-error
       (local=zulu "2009-11-01T01:00:00" "2009-11-01T06:00:00Z" :error NY))

      ;; assert correct policy-based conversion during unambiguous EST
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :any NY)
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :first NY)
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :earliest NY)
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :prefer-daylight-savings NY)
      (clock-test-assert-error
       (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :daylight-savings NY))
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :latest NY)
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :last NY)
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :not-daylight-savings NY)
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :prefer-not-daylight-savings NY)
      (local=zulu "2009-11-01T02:00:00" "2009-11-01T07:00:00Z" :error NY)

      ;; --- "spring forward", March 2009 ---
      ;; warm-up exercises for illustration
      (local=zulu "2009-03-08T00:00:00" "2009-03-08T05:00:00Z" :any NY)
      (local=zulu "2009-03-08T00:30:00" "2009-03-08T05:30:00Z" :any NY)
      (local=zulu "2009-03-08T01:00:00" "2009-03-08T06:00:00Z" :any NY)
      (local=zulu "2009-03-08T01:30:00" "2009-03-08T06:30:00Z" :any NY)
      (clock-test-assert-error ;; no such time as 02:00:00 in EDT or EST
       (local=zulu "2009-03-08T02:00:00" "2009-03-08T07:00:00Z" :any NY))
      (clock-test-assert-error ;; no such time as 02:30:00 in EDT or EST
       (local=zulu "2009-03-08T02:30:00" "2009-03-08T07:30:00Z" :any NY))
      (local=zulu "2009-03-08T03:00:00" "2009-03-08T07:00:00Z" :any NY)
      (local=zulu "2009-03-08T03:30:00" "2009-03-08T07:30:00Z" :any NY)
      

      ))
    
              
)

;;; clock-test.el ends here