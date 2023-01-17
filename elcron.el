;;; elcron.el --- A utility library for elcron-like parsing and scheduling  -*- lexical-binding:t -*-
;;
;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; URL: https://github.com/haji-ali/elcron.git
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
;; (require 'elcron)
;;
;; (elcron-schedule
;;    "0/5 14,18,20-39,52 * * JAN,MAR,SEP MON-FRI 2002-2050"
;;    'my-function my-args)
;;

;;; Code:
(require 'cl-lib)
(require 'rx)

(defconst elcron--fields-defs
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
  "List of elcron field definitions.
Each element in the list defines a elcron field with the
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

(defvar elcron--log-buffer nil
  "A buffer to log elcron events.
nil means disable logging.")

(defun elcron--parse-value (x field-def)
  "Parse value X according to FIELD-DEF.
Also checks that X is in the correct range."
  (let* ((field-def (elcron--field-def field-def))
         (vals (elcron--field-prop field-def :values))
         (range (elcron--field-prop field-def :range))
         pos)
    (if (and vals
             (setq pos (cl-position x vals)))
        (+ pos (car range))
      (elcron--check-range x range)
      x)))

(defun elcron--check-range (x range)
  "Check that X is a number and falls within RANGE."
  (unless (and (numberp x)
               (>= x (car range))
               (<= x (cdr range)))
    (user-error "Value outside range")))

(defun elcron--field-type-p (field-def field-type)
  "Check if the type of FIELD-DEF is equal to FIELD-TYPE."
  (eq (car field-def) field-type))

(defun elcron--time-add (decoded-time val &optional units)
  "Add VAL seconds to a DECODED-TIME structure.
Similar to `decoded-time-add' except that it updates the weekday
and does not have the warning \"obsolete timestamp with cdr 1\".
Also, if UNITS is non-nil it can be one from
`elcron--fields-defs' to specify a unit different from seconds."
  (when units
    (setq units (elcron--field-def units))
    (setq val (* val (elcron--field-prop units :to-seconds))))
  (decode-time (time-add (encode-time decoded-time) val)))

(defun elcron--time-equal-p (A B)
  "Return non-nil if A and B are equal time values.
Similar to `time-equal-p' except that it works for decoded times."
  (time-equal-p (encode-time A) (encode-time B)))

(defun elcron--time-less-p (A B)
  "Return non-nil if time value A is less than time value B.
Similar to `time-less-p' except that it works for decoded times."
  (time-less-p (encode-time A) (encode-time B)))

(defun elcron--field-def (field)
  "Get FIELD definition.
See `elcron--fields-defs' for a list of definitions. If FIELD is
already a definition simply return it."
  (if (consp field)
      field
    (cons field (alist-get field elcron--fields-defs))))

(defun elcron--too-late-p (time orig-time)
  "Return t if TIME is significantly after than ORIG-TIME.
Returns nil if there is less than 10 years between the arguments."
  (> (- (decoded-time-year time)
        (decoded-time-year orig-time))
     10))

(defun elcron--field-prop (field-def key)
  "Get property corresponding to KEY from FIELD-DEF."
  (plist-get (cdr field-def) key))

(defun elcron--set-field (time field
                             new-value
                             &optional
                             accept-past)
  "Set FIELD in TIME to NEW-VALUE.
This is done by advancing time until the value of FIELD is
correct. If ACCEPT-PAST is non-nil then the result could be in
the past."
  (let* ((field-def (elcron--field-def field))
         (getter (elcron--field-prop field-def :getter))
         (range (elcron--field-prop field-def :range))
         (to-seconds (elcron--field-prop field-def :to-seconds))
         (new-value (elcron--parse-value new-value field-def))
         (orig-time time)
         old-value)
    (cl-loop
     while (and time
                (not (eq
                      (setq old-value (funcall getter time))
                      new-value)))
     never (elcron--too-late-p time orig-time)
     do (if (or accept-past
                (< old-value new-value))
            ;; Simply advance time
            (setq time
                  (elcron--time-add time (* (- new-value old-value) to-seconds)))
          ;; Otherwise, we need to advance to "next-cycle"
          ;; (next minute, next hour ... etc)

          (setq time
                (unless (elcron--field-prop field-def :non-cyclic)
                  (let ((next-cycle (or (elcron--field-prop field-def :next-cycle)
                                        (* (1+ (- (cdr range) (car range))) to-seconds))))
                    (elcron--time-add time
                                    (+
                                     next-cycle
                                     (* to-seconds (- new-value old-value))))))))
     finally return time)))

(defun elcron--reset-field-maybe (time prev-time field
                                     &optional no-recurse)
  "Set FIELD in TIME to it's minimum value if the result is after PREV-TIME.
If the result is less than PREV-TIME, return nil."
  (let ((field-def (elcron--field-def field))
        new-time)
    (when (and (elcron--field-prop field-def :reset) (not no-recurse))
      ;; First try to reset the lesser field
      (setq time (elcron--reset-field-maybe
                  time prev-time
                  (elcron--field-def (elcron--field-prop field-def :reset)))))
    (when time
      (setq new-time (elcron--set-field
                      time
                      field-def
                      ;; set to minimum
                      (car (elcron--field-prop field-def :range))
                      t))
      (if (elcron--time-less-p prev-time new-time)
          new-time))))

(defun elcron--same-or-next-by-field (time expr field &optional reset-lesser)
  "Return the next TIME given an EXPR on FIELD.
See `elcron-same-or-next' for possible expressions.
FIELD can be one of `elcron--fields-defs' or their car's.
if RESET-LESSER is non-nil, all fields which are finer than
field-def are set to their minimum value."
  ;; https://www.netiq.com/documentation/cloud-manager-2-5/ncm-reference/data/bexyssf.html
  (let ((next-time)
        (field-def (elcron--field-def field)))
    (setq next-time
          (let* ((range (elcron--field-prop field-def :range))
                 (vals (elcron--field-prop field-def :values))
                 (n (funcall (elcron--field-prop field-def :getter) time)))
            (pcase expr
              ((pred consp)
               (pcase (car expr)
                 ('-
                  (let ((from (elcron--parse-value (cadr expr) field-def))
                        (to (elcron--parse-value (caddr expr) field-def)))
                    (if (and (>= n from) (<= n to))
                        ;; Time in between from and to
                        time
                      (elcron--set-field time field-def from))))
                 ('/
                  (let ((base (if (eq (cadr expr) '*) ;; If *, return min
                                  (car (elcron--field-prop field-def :range))
                                (elcron--parse-value (cadr expr) field-def)))
                        (period (caddr expr)))
                    (elcron--check-range period range)
                    (elcron--set-field time field-def
                                     (let ((new-value (+ base
                                                         (* (/ (+ (- n base)
                                                                  (1- period))
                                                               period) period))))
                                       (if (> new-value (cdr range))
                                           base
                                         new-value)))))
                 ((or 'L '\#)
                  (unless (elcron--field-type-p field-def 'weekday)
                    (user-error "L/# consp are only accepted for day-of-week"))
                  (let ((weekday (elcron--parse-value (cadr expr)
                                                    'weekday))
                        (nth (caddr expr)))
                    (elcron--set-day-of-month time weekday nth)))
                 ('W
                  (unless (elcron--field-type-p field-def 'day)
                    (user-error "W is only accepted for day-of-month"))
                  (if (eq (cadr expr) 'L)
                      (elcron--set-day-of-month time 'W nil)
                    (let ((day (elcron--parse-value (cadr expr) field-def)))
                      (elcron--set-closest-workday time day))))
                 (_
                  (car (cl-sort (cl-loop for subexp in expr
                                         for res = (elcron--same-or-next-by-field
                                                    time subexp field-def t)
                                         unless (null res)
                                         collect res)
                                #'elcron--time-less-p)))))
              ((guard (or (numberp expr)
                          (memq expr vals)))
               (elcron--set-field time field-def expr))
              ('L
               ;; weekday or day-of-month
               (unless (or (elcron--field-type-p field-def 'day)
                           (elcron--field-type-p field-def 'weekday))
                 (user-error "L is only accepted for day-of-month or weekday"))
               (if (elcron--field-type-p field-def 'weekday)
                   (elcron--set-field time field-def 6)
                 (elcron--set-day-of-month time nil nil)))
              ((or '* '\?) time) ;; I am not sure why ? is needed instead of *
              (_ (user-error "Unknown expression")))))
    (if (or (null next-time)
            (elcron--time-equal-p next-time time)
            (not reset-lesser))
        next-time
      ;; We've advanced the time, try to reset the other fields
      (or (when-let (reset (elcron--field-prop field-def :reset))
            (elcron--reset-field-maybe next-time time
                                     (elcron--field-def reset)))
          next-time))))

(defun elcron--set-day-of-month (time weekday nth)
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
                   (elcron--parse-value weekday 'weekday)))
        cur-month
        cur-nth
        next-month
        (continue t))
    ;; Last day of the current month
    (cl-loop
     while continue
     never (elcron--too-late-p time orig-time)
     do (progn
          (setq continue nil)
          (setq cur-month (decoded-time-month time))
          (setq next-month
                (elcron--reset-field-maybe
                 (elcron--set-field time 'month
                                  (1+ (mod cur-month 12)))
                 time
                 'day))
          (setq time (elcron--time-add next-month -1 'day))
          (when weekday
            (let* ((cur (decoded-time-weekday time))
                   (wanted (or (and (eq weekday 'W)
                                    (or (and (memq cur '(0 6)) 5) cur))
                               weekday)))
              (unless (eq cur wanted)
                ;; Otherwise go backward to last day in days-of-week
                (setq time
                      (elcron--time-add
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
                      (elcron--time-add time (* (- nth cur-nth) 7) 'day))
                (if (and (eq (decoded-time-month time) (decoded-time-month orig-time))
                         (eq (decoded-time-day time) (decoded-time-day orig-time))
                         (eq (decoded-time-year time) (decoded-time-year orig-time)))
                    (setq time orig-time)
                  (when (setq continue (elcron--time-less-p time orig-time))
                    ;; Advance to next month and repeat process
                    (setq continue t
                          time next-month)))))))
     finally return time)))

(defun elcron--set-closest-workday (time day-of-month)
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
       never (elcron--too-late-p time orig-time)
       do (progn
            (setq continue nil
                  time (elcron--set-field time 'day day-of-month))
            (let* ((cur-weekday (decoded-time-weekday time))
                   (cur-day (decoded-time-day time)))
              (when (memq cur-weekday '(0 6))
                (setq temp time
                      time (elcron--time-add
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
                    (setq time (elcron--time-add time -1 'day)))

                (when (elcron--time-less-p time orig-time)
                  (setq
                   continue t
                   time (elcron--set-field
                         time
                         'month
                         (1+ (mod (decoded-time-month time) 12))))))))))
    (when time
      (if (elcron--time-equal-p time orig-time)
          orig-time
        (elcron--reset-field-maybe
         time
         orig-time
         'hour)))))

(defun elcron--normalize (elcron-vec)
  (pcase elcron-vec
    ((pred stringp) (elcron-parse elcron-vec))
    ((pred listp) (elcron--plist-to-vector
                   elcron-vec
                   (cl-loop
                    for def in elcron--fields-defs
                    collect (intern
                             (format ":%S"
                                     (car def))))))
    (_ elcron-vec)))

(defun elcron-same-or-next (time elcron-vec)
  "Find the next trigger after TIME for a elcron expression.
Or returns TIME if the event should be triggered then.
See `elcron-schedule' for format of ELCRON-VEC."
  (cl-loop
   with elcron-vector = (elcron--normalize elcron-vec)
   with prev-time = time
   do (setq
       prev-time time
       time (cl-loop
             with ntime = prev-time
             for def in elcron--fields-defs
             for expr being the elements of elcron-vector
             when expr
             do (setq ntime (elcron--same-or-next-by-field ntime expr def t))
             while ntime
             finally return ntime))
   until (or (null time) (elcron--time-equal-p prev-time time))
   finally return time))

(defun elcron-parse (expression)
  "Parse a standard elcron string EXPRESSION to Lisp expression.

The parser is a dump one so limited syntax checks are performed.
In case a sub-expression cannot be parse, it is returned as 'ERROR"
  (cl-loop
   with expres = (split-string expression " ")
   for subexpr being the elements of expres using (index j)
   with result = (make-vector (length expres) nil)
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
   do (aset result j (if (cdr col) col (car col)))
   finally return result))

;;;###autoload
(defun elcron-schedule (elcron-vec function &rest args)
  "Schedule a FUNCTION call with ARGS for a given elcron expression.

ELCRON-VEC can be a vector of up to 7 elements
\[SECOND MINUTE HOUR DAY MONTH WEEKDAY YEAR]

or a plist-list such as
'(:hour 3 :second 10)

Acceptable values are:
- SECOND, MINUTE: 0--59
- HOUR: 1--12
- DAY: 1--31
- MONTH: 1--12 (or jan--dec)
- WEEKDAY: 0--6 (sun--sat)

Acceptable expressions are:
- A number in the range.
- (- min max) to denote an inclusive range of values.
- (/ base period) to denote periodic occurrence starting at base.
- (EXPR EXPR EXPR) to denote multiple acceptable expressions.

Additionally, DAY-OF-MONTH can be
- L: To denote the last day of the month
- (W x): to denote the closest workday to the day x in a month.

WEEKDAY can be:
- L: For Saturday.
- (# weekday): For the last weekday in a month.
- (# weekday nth): For the nth weekday in a month."
  (let ((elcron-vector (elcron--normalize elcron-vec))
        (timer (timer-create)))
    (timer-set-function timer
                        #'elcron--event-handler
                        (list timer elcron-vector function args))
    (and (elcron--schedule-next timer elcron-vector)
         timer)))

(defun elcron--plist-to-vector (plist keys)
  "Convert PLIST with KEYS to a vector."
  (let ((plist (copy-sequence plist)))
    (prog1 (cl-loop
            with result = (make-vector (length keys) nil)
            for key being the elements of keys using (index j)
            for found = (cl-member key plist)
            when found
            do (progn
                 (aset result j (cadr found))
                 (setf (car found) nil
                       (cadr found) nil)
                 (when (cl-member key plist)
                   (user-error "Duplicate keys in plist")))
            finally return result)
      (when (cl-notevery #'null plist)
        (user-error "Unrecognized keys in plist")))))

(defun elcron--schedule-next (timer elcron-vec)
  "Set trigger time of TIMER on the next trigger of ELCRON-VEC."
  ;; TODO: Is there a risk that it won't work?
  (when (cl-every #'null elcron-vec)
    (with-current-buffer (get-buffer-create elcron--log-buffer)
      (goto-char (point-max))
      (insert "TIMER: %S" timer)
      (insert (with-temp-buffer
                (let ((standard-output (current-buffer)))
                  (backtrace)
                  (buffer-string))))))

  (let ((cur-time (decode-time (time-add (current-time) 1)))
        (elcron-vec (elcron--normalize elcron-vec)))
    (when-let (next-time (elcron-same-or-next
                          ;; Resolution is at least a second
                          cur-time
                          elcron-vec))
      (when elcron--log-buffer
        (with-current-buffer (get-buffer-create elcron--log-buffer)
          (goto-char (point-max))
          (insert (format
                   "%s Setting timer: %S -> %S ==== %S\n" (format-time-string "%d/%m/%Y %T")
                   elcron-vec
                   cur-time
                   next-time))))
      (setq next-time (encode-time next-time))
      (unless (eq (timer--time timer) next-time)
        (cancel-timer timer) ;; In case it was active before
        (timer-set-time timer next-time nil)
        (timer-activate timer)
        t))))

(defun elcron--event-handler (timer elcron-vec function args)
  "Event handler for elcron timers.
Calls FUNCTION with ARGS and schedules the next tigger of TIMER
according to ELCRON-VEC."
  (unwind-protect
      (when elcron--log-buffer
        (with-current-buffer (get-buffer-create elcron--log-buffer)
        (goto-char (point-max))
        (insert (format
                 "%s elcron fired: %S\n" (format-time-string "%d/%m/%Y %T")
                 function))))
      (apply function args)
    (elcron--schedule-next timer elcron-vec)))

(provide 'elcron)

;;; elcron.el ends here
