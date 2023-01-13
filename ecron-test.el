;;; ecron-test.el --- Unit-tests for ecron.el  -*- lexical-binding:t -*-
;;
;; Author: Al Haji-Ali <abdo.haji.ali@gmail.com>
;; URL: https://github.com/haji-ali/ecron.el.git
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
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

(require 'ecron)

(ert "ecron-.*")

(ert-deftest ecron-test-parser ()
  (should (equal
           (ecron-parse "0 0/5 14,18,3-39,52 * JAN,MAR,SEP MON-FRI 2002-2011 LW")
           '(0 (/ 0 5) (14 18 (- 3 39) 52) * (jan mar sep)
               (- mon fri) (- 2002 2011) (W L))))

  (should (equal
           (ecron-parse "L 3L 3W */3 2#2 ? W LW")
           '(L (L 3) (W 3) (/ nil 3) (\# 2 2) \? ERROR (W L)))))

(ert-deftest ecron-test-next ()
  (cl-flet* ((check (x y &optional time)
                    (let* ((time (or time '(9 12 18 11 1 2023 3 nil 0)))
                           (res (ecron-same-or-next time x)))
                      (and (equal (butlast res 3) y)
                           (equal (butlast (ecron-same-or-next res x) 3) y)))))
    (should (check '(0) '(0 13 18 11 1 2023)))
    (should (check '(0 0) '(0 0 19 11 1 2023)))
    (should (check '(3 0 3) '(3 0 3 12 1 2023)))
    (should (check '(* * * * 3 tue) '(0 0 0 7 3 2023)))
    (should (check '(* * * L * *) '(0 0 0 31 1 2023)))
    (should (check '(* * * * * (L fri)) '(0 0 0 27 1 2023)))
    (should (check '(* * * * * (\# fri 1)) '(0 0 0 3 2 2023)))
    (should (check '(* * * * * (\# fri 2)) '(0 0 0 13 1 2023)))
    (should (check '(* * * (W 14) * *) '(0 0 0 13 1 2023)))
    (should (check '(* * * (W L) * *) '(0 0 0 31 1 2023)))
    (should (check '(* * * (W L) * *) '(0 0 0 28 4 2023)
                   '(9 12 18 11 4 2023 3 nil 0)))
    (should (check '(* * * (W 15) * *) '(0 0 0 16 1 2023)))
    (should (check
             "0/5 14,18,20-39,52 * * JAN,MAR,SEP MON-FRI 2002-2050"
             '(0 14 18 11 1 2023)))))
