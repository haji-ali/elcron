;;; ecron.el --- A utility library for ecron-like parsing and scheduling  -*- lexical-binding:t -*-
;;
;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; URL: https://github.com/haji-ali/ecron.el.git
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; This package provides a simple functionality to emulate the cron
;; scheduler in Emacs, using Emacs timers.
;;
;; Typical usage:
;;
;; (require 'ecron)
;;
;; (ecron-schedule
;;    "0/5 14,18,20-39,52 * * JAN,MAR,SEP MON-FRI 2002-2050"
;;    'my-function my-args)
;;

;;; Code:
(require 'cl-lib)
(require 'rx)

(defconst ecron--fields-defs
  `((second :range (0 . 59)
            :to-seconds 1
            :getter decoded-time-second)
    (minute :range (0 . 59)
            :to-seconds 60
            :reset second
            :getter decoded-time-minute)
    (hour :range (0 . 23)
          :to-seconds ,(* 60 60)
          :reset minute
          :getter decoded-time-hour)
    (day :range (1 . 31)
         :to-seconds ,(* 60 60 24)
         :reset hour
         :getter decoded-time-day)
    (month :range (1 . 12)
           :to-seconds ,(* 60 60 24 28)
           :reset day
           :values (jan feb mar apr may jun jul aug sep oct nov dec)
           :getter decoded-time-month)
    (weekday :range (0 . 6)   ;; 0 is Sunday
             :to-seconds ,(* 60 60 24)
             :reset hour
             :values (sun mon tue wed thu fri sat)
             :getter decoded-time-weekday)
    (year :range (0 . 9999)
          :to-seconds ,(* 60 60 24 365)  ;; Leap year?
          :reset  month
          :non-cyclic t
          :getter decoded-time-year))
  "List of ecron field definitions.
Each element in the list defines a ecron field with the
following properties:
- `:range' Range of acceptable values.
- `:to-seconds' Number of seconds in a single increment of the
field.
- `:reset' the name of the field with a finer time increment
than the current field.
- `:getter' Function to get the field value from a time
structure.
- `:values' List of acceptable values, whose values correspond to
`:range'.
- `:non-cyclic' if non-nil means that the field is not periodic
\(like a year).")

(defun ecron--parse-value (x field-def)
  "Parse value X according to FIELD-DEF.
Also checks that X is in the correct range."
  (let* ((field-def (ecron--field-def field-def))
         (vals (ecron--field-prop field-def :values))
         (range (ecron--field-prop field-def :range))
         pos)
    (if (and vals
             (setq pos (cl-position x vals)))
        (+ pos (car range))
      (ecron--check-range x range)
      x)))

(defun ecron--check-range (x range)
  "Check that X is a number and falls within RANGE."
  (unless (and (numberp x)
               (>= x (car range))
               (<= x (cdr range)))
    (user-error "Value outside range")))

(defun ecron--field-type-p (field-def field-type)
  "Check if the type of FIELD-DEF is equal to FIELD-TYPE."
  (eq (car field-def) field-type))

(defun ecron--time-add (decoded-time val &optional units)
  "Add VAL seconds to a DECODED-TIME structure.
Similar to `decoded-time-add' except that it updates the weekday
and does not have the warning \"obsolete timestamp with cdr 1\".
Also, if UNITS is non-nil it can be one from
`ecron--fields-defs' to specify a unit different from seconds."
  (when units
    (setq units (ecron--field-def units))
    (setq val (* val (ecron--field-prop units :to-seconds))))
  (decode-time (time-add (encode-time decoded-time) val)))

(defun ecron--time-equal-p (A B)
  "Return non-nil if A and B are equal time values.
Similar to `time-equal-p' except that it works for decoded times."
  (time-equal-p (encode-time A) (encode-time B)))

(defun ecron--time-less-p (A B)
  "Return non-nil if time value A is less than time value B.
Similar to `time-less-p' except that it works for decoded times."
  (time-less-p (encode-time A) (encode-time B)))

(defun ecron--field-def (field)
  "Get FIELD definition.
See `ecron--fields-defs' for a list of definitions. If FIELD is
already a definition simply return it."
  (if (consp field)
      field
    (cons field (alist-get field ecron--fields-defs))))

(defun ecron--too-late-p (time orig-time)
  "Return t if TIME is significantly after than ORIG-TIME.
Returns nil if there is less than 10 years between the arguments."
  (> (- (decoded-time-year time)
        (decoded-time-year orig-time))
     10))

(defun ecron--field-prop (field-def key)
  "Get property corresponding to KEY from FIELD-DEF."
  (plist-get (cdr field-def) key))

(defun ecron--set-field (time field
                             new-value
                             &optional
                             accept-past)
  "Set FIELD in TIME to NEW-VALUE.
This is done by advancing time until the value of FIELD is
correct. If ACCEPT-PAST is non-nil then the result could be in
the past."
  (let* ((field-def (ecron--field-def field))
         (getter (ecron--field-prop field-def :getter))
         (range (ecron--field-prop field-def :range))
         (to-seconds (ecron--field-prop field-def :to-seconds))
         (new-value (ecron--parse-value new-value field-def))
         (orig-time time)
         old-value)
    (cl-loop
     while (and time
                (not (eq
                      (setq old-value (funcall getter time))
                      new-value)))
     never (ecron--too-late-p time orig-time)
     do (if (or accept-past
                (< old-value new-value))
            ;; Simply advance time
            (setq time
                  (ecron--time-add time (* (- new-value old-value) to-seconds)))
          ;; Otherwise, we need to advance to "next-cycle"
          ;; (next minute, next hour ... etc)

          (setq time
                (unless (ecron--field-prop field-def :non-cyclic)
                  (let ((next-cycle (or (ecron--field-prop field-def :next-cycle)
                                        (* (1+ (- (cdr range) (car range))) to-seconds))))
                    (ecron--time-add time
                                    (+
                                     next-cycle
                                     (* to-seconds (- new-value old-value))))))))
     finally return time)))

(defun ecron--reset-field-maybe (time prev-time field
                                     &optional no-recurse)
  "Set FIELD in TIME to it's minimum value if the result is after PREV-TIME.
If the result is less than PREV-TIME, return nil."
  (let ((field-def (ecron--field-def field))
        new-time)
    (when (and (ecron--field-prop field-def :reset) (not no-recurse))
      ;; First try to reset the lesser field
      (setq time (ecron--reset-field-maybe
                  time prev-time
                  (ecron--field-def (ecron--field-prop field-def :reset)))))
    (when time
      (setq new-time (ecron--set-field
                      time
                      field-def
                      ;; set to minimum
                      (car (ecron--field-prop field-def :range))
                      t))
      (if (ecron--time-less-p prev-time new-time)
          new-time))))

(defun ecron--same-or-next-by-field (time expr field &optional reset-lesser)
  "Return the next TIME given an EXPR on FIELD.
See `ecron-same-or-next' for possible expressions.
FIELD can be one of `ecron--fields-defs' or their car's.
if RESET-LESSER is non-nil, all fields which are finer than
field-def are set to their minimum value."
  ;; https://www.netiq.com/documentation/cloud-manager-2-5/ncm-reference/data/bexyssf.html
  (let ((next-time)
        (field-def (ecron--field-def field)))
    (setq next-time
          (let* ((range (ecron--field-prop field-def :range))
                 (vals (ecron--field-prop field-def :values))
                 (n (funcall (ecron--field-prop field-def :getter) time)))
            (pcase expr
              ((pred consp)
               (pcase (car expr)
                 ('-
                  (let ((from (ecron--parse-value (cadr expr) field-def))
                        (to (ecron--parse-value (caddr expr) field-def)))
                    (if (and (>= n from) (<= n to))
                        ;; Time in between from and to
                        time
                      (ecron--set-field time field-def from))))
                 ('/
                  (let ((base (ecron--parse-value (cadr expr) field-def))
                        (period (caddr expr)))
                    (ecron--check-range period range)
                    (ecron--set-field time field-def
                                     (let ((new-value (+ base
                                                         (* (/ (+ (- n base)
                                                                  (1- period))
                                                               period) period))))
                                       (if (> new-value (cdr range))
                                           base
                                         new-value)))))
                 ((or 'L '\#)
                  (unless (ecron--field-type-p field-def 'weekday)
                    (user-error "L/# consp are only accepted for day-of-week"))
                  (let ((weekday (ecron--parse-value (cadr expr)
                                                    'weekday))
                        (nth (caddr expr)))
                    (ecron--set-day-of-month time weekday nth)))
                 ('W
                  (unless (ecron--field-type-p field-def 'day)
                    (user-error "W is only accepted for day-of-month"))
                  (if (eq (cadr expr) 'L)
                      (ecron--set-day-of-month time 'W nil)
                    (let ((day (ecron--parse-value (cadr expr) field-def)))
                      (ecron--set-closest-workday time day))))
                 (_
                  (car (cl-sort (cl-loop for subexp in expr
                                         for res = (ecron--same-or-next-by-field
                                                    time subexp field-def t)
                                         unless (null res)
                                         collect res)
                                #'ecron--time-less-p)))))
              ((guard (or (numberp expr)
                          (memq expr vals)))
               (ecron--set-field time field-def expr))
              ('L
               ;; weekday or day-of-month
               (unless (or (ecron--field-type-p field-def 'day)
                           (ecron--field-type-p field-def 'weekday))
                 (user-error "L is only accepted for day-of-month or weekday"))
               (if (ecron--field-type-p field-def 'weekday)
                   (ecron--set-field time field-def 6)
                 (ecron--set-day-of-month time nil nil)))
              ((or '* '\?) time) ;; I am not sure why ? is needed instead of *
              (_ (user-error "Unknown expression")))))
    (if (or (null next-time)
            (ecron--time-equal-p next-time time)
            (not reset-lesser))
        next-time
      ;; We've advanced the time, try to reset the other fields
      (or (when-let (reset (ecron--field-prop field-def :reset))
            (ecron--reset-field-maybe next-time time
                                     (ecron--field-def reset)))
          next-time))))

(defun ecron--set-day-of-month (time weekday nth)
  "Set day of the month in TIME to WEEKDAY.
If NTH is non-nil, set day to the n'th weekday of the month.
Otherwise, set to last. If WEEKDAY is 'W, then set the day to
Friday unless the last day of the month is a workday, then set it
to that workday. If WEEKDAY is nil, set the day to the last day
of the month."
  (let ((orig-time time)
        (weekday (if (or (null weekday)
                         (eq weekday 'W))
                     weekday
                   (ecron--parse-value weekday 'weekday)))
        cur-month
        cur-nth
        next-month
        (continue t))
    ;; Last day of the current month
    (cl-loop
     while continue
     never (ecron--too-late-p time orig-time)
     do (progn
          (setq continue nil)
          (setq cur-month (decoded-time-month time))
          (setq next-month
                (ecron--reset-field-maybe
                 (ecron--set-field time 'month
                                  (1+ (mod cur-month 12)))
                 time
                 'day))
          (setq time (ecron--time-add next-month -1 'day))
          (when weekday
            (let* ((cur (decoded-time-weekday time))
                   (wanted (or (and (eq weekday 'W)
                                    (or (and (memq cur '(0 6)) 5) cur))
                               weekday)))
              (unless (eq cur wanted)
                ;; Otherwise go backward to last day in days-of-week
                (setq time
                      (ecron--time-add
                       time
                       (- wanted cur (if (> wanted cur) 7 0))
                       'day)))))

          ;; Number of weeks to beginning of month
          (when nth
            (setq cur-nth (1+ (/ (decoded-time-day time) 7)))
            (unless (= cur-nth nth) ;; Lucky
              (if (< cur-nth nth)
                  (setq continue t
                        time next-month)
                (setq time
                      (ecron--time-add time (* (- nth cur-nth) 7) 'day))
                (if (and (eq (decoded-time-month time) (decoded-time-month orig-time))
                         (eq (decoded-time-day time) (decoded-time-day orig-time))
                         (eq (decoded-time-year time) (decoded-time-year orig-time)))
                    (setq time orig-time)
                  (when (setq continue (ecron--time-less-p time orig-time))
                    ;; Advance to next month and repeat process
                    (setq continue t
                          time next-month)))))))
     finally return time)))

(defun ecron--set-closest-workday (time day-of-month)
  "Find closest workday to DAY-OF-MONTH to TIME.
If day-of-month is a workday, set it to that day. Otherwise, find
the closest future workday within the current month is possible
or in subsequent months otherwise. If DAY-OF-MONTH is nil, set
day to last workday of month."
  (let* ((cur-weekday (decoded-time-weekday time))
         (cur-day (decoded-time-day time))
         (orig-time time)
         (continue t)
         temp)
    (unless (and ;; First check if the current time is acceptable
             ;; Must be a weekday
             (not (memq cur-weekday '(0 6)))
             (or
              ;; Either the day is already as requested
              (eq cur-day day-of-month)
              ;; Or it's a Monday and the requested day is a
              ;; Saturday or Sunday
              (and
               (eq cur-weekday 1) ;; Monday
               (or
                ;; Requested day was a Sunday
                (and
                 (> day-of-month 1)
                 (eq day-of-month (1- cur-day)))
                ;; Requested day was a Saturday 1st, so that Monday is the
                ;; closest workday
                (and (eq day-of-month 1)
                     (eq cur-day 3))))
              ;; There's no need to check for Friday (and Saturday or Sunday
              ;; being the requested day) since this will be found out in the
              ;; following algorithm which would also check the month
              ;; boundary.
              ))
      (cl-loop
       while continue
       never (ecron--too-late-p time orig-time)
       do (progn
            (setq continue nil
                  time (ecron--set-field time 'day day-of-month))
            (let* ((cur-weekday (decoded-time-weekday time))
                   (cur-day (decoded-time-day time)))
              (when (memq cur-weekday '(0 6))
                (setq temp time
                      time (ecron--time-add
                            time
                            (if  (eq cur-weekday 0) ;; Sunday
                                1 ;; Always advance
                              ;; Otherwise Saturday
                              (if (eq cur-day 1)
                                  2 ;; Can't go back, go to Monday
                                -1))
                            'day))

                ;; One case we need to account for, is if it's Sunday on the last
                ;; day of the month, then we should go back to Friday
                (if (not (eq (decoded-time-month time)
                             (decoded-time-month temp)))
                    (setq time (ecron--time-add time -1 'day)))

                (when (ecron--time-less-p time orig-time)
                  (setq
                   continue t
                   time (ecron--set-field
                         time
                         'month
                         (1+ (mod (decoded-time-month time) 12))))))))))
    (when time
      (if (ecron--time-equal-p time orig-time)
          orig-time
        (ecron--reset-field-maybe
         time
         orig-time
         'hour)))))

(defun ecron-same-or-next (time ecron-vec)
  "Find the next trigger after TIME for a ecron expression.
Or returns TIME if the event should be triggered then.
See `ecron-schedule' for format of ECRON-VEC."
  (cl-loop
   with ecron-vector = (if (stringp ecron-vec)
                          (ecron-parse ecron-vec)
                        ecron-vec)
   with prev-time = time
   do (setq
       prev-time time
       time (cl-loop
             with ntime = prev-time
             for def in ecron--fields-defs
             for expr in ecron-vector
             do (setq ntime (ecron--same-or-next-by-field ntime expr def t))
             while ntime
             finally return ntime))
   until (or (null time) (ecron--time-equal-p prev-time time))
   finally return time))

(defun ecron-parse (expression)
  "Parse a standard ecron string EXPRESSION to Lisp expression.

The parser is a dump one so limited syntax checks are performed.
In case a sub-expression cannot be parse, it is returned as 'ERROR"
  (cl-loop
   for subexpr in (split-string expression " ")
   for col = (cl-loop
              for expr in (split-string subexpr ",")
              collect (pcase expr
                        ((or "*" "?" "L") (intern expr))
                        ("LW" '(W L))
                        ((rx string-start
                             (or
                              (let num (* digit))
                              (let str (= 3 letter)))
                             string-end)
                         (or (and num (string-to-number num))
                             (intern (downcase str))))
                        ((rx string-start
                             (or
                              ""
                              "*"
                              (let num-1 (+ digit))
                              (let str-1 (= 3 letter)))
                             (let sep (any "/" "-" "#"))
                             (or (let num-2 (* digit))
                                 (let str-2 (= 3 letter)))
                             string-end)
                         (list (intern sep)
                               (or (and num-1 (string-to-number num-1))
                                   (and str-1 (intern (downcase str-1))))
                               (or (and num-2 (string-to-number num-2))
                                   (intern (downcase str-2)))))
                        ((rx string-start
                             (or
                              (let num-1 (+ digit))
                              (let str-1 (= 3 letter)))
                             (let post (or "L" "W"))
                             string-end)
                         (list (intern post)
                               (or (and num-1 (string-to-number num-1))
                                   (and str-1 (intern (downcase str-1))))))
                        (_ 'ERROR)))
   collect (if (cdr col) col (car col))))

;;;###autoload
(defun ecron-schedule (ecron-vec function &rest args)
  "Schedule a FUNCTION call with ARGS for a given ecron expression.

ECRON-VEC is a list or vector of up to 7 elements
\(SECONDS MINUTES HOURS DAY-OF-MONTH MONTH WEEKDAY YEAR).

Acceptable values are:
- SECONDS, MINUTES: 0--59
- HOURS: 1--12
- DAY-OF-MONTH: 1--31
- MONTH: 1--12 (or jan--dec)
- WEEKDAY: 0--6 (sun--sat)

Acceptable expressions are:
- A number in range.
- (- min max) to denote an inclusive range of values.
- (/ base period) to denote periodic occurrence starting at base.
- (EXPR EXPR EXPR) to denote multiple acceptable expressions.

Additionally, DAY-OF-MONTH can be
- L: To denote the last day of the month
- (W x): to denote the closest workday to the day x in a month.

WEEKDAY can be:
- L: For Saturday.
- (P weekday): For the last weekday in a month.
- (P weekday nth): For the nth weekday in a month."
  (let ((ecron-vector (if (stringp ecron-vec)
                         (ecron-parse ecron-vec)
                       ecron-vec))
        (timer (timer-create)))
    (timer-set-function timer
                        #'ecron--event-handler
                        (list timer ecron-vector function args))
    (and (ecron--schedule-next timer ecron-vector)
         timer)))

(defun ecron--schedule-next (timer ecron-vec)
  "Set trigger time of TIMER on the next trigger of ECRON-VEC."
  (when-let (next-time (ecron-same-or-next
                        ;; Resolution is at least a second
                        (decode-time (time-add (current-time) 1))
                        ecron-vec))
    (timer-set-time timer (encode-time next-time) nil)
    (timer-activate timer)))

(defun ecron--event-handler (timer ecron-vec function args)
  "Event handler for ecron timers.
Calls FUNCTION with ARGS and schedules the next tigger of TIMER
according to ECRON-VEC."
  (unwind-protect
      (apply function args)
    (ecron--schedule-next timer ecron-vec)))

(provide 'ecron)

;;; ecron.el ends here
