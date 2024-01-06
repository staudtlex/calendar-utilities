;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Copyright (C) 2023  Alexander Staudt
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;;

(in-package :calendar-utilities)

;;; Helper functions
(defun seq (from-value to-value)
  "Return a sequence of consecutive integers, ranging from FROM-VALUE to TO-VALUE"
  (if (> to-value from-value)
      (let ((length (1+ (- to-value from-value))))
        (loop :for n :below length :collect (+ from-value n)))
      (let ((length (1+ (- from-value to-value))))
        (loop :for n :below length :collect (- from-value n)))
      ))

(defun get-value-from-key (key alist)
  "Return the value from an ALIST associated with a given KEY"
  (cdr (assoc key alist)))

(defun symbol-from-string (string &optional (package *package*)) 
  "Locate a symbol whose name is STRING in a PACKAGE. STRING is always converted to upper-case. If a symbol is found in PACKAGE, the symbol returned, otherwise SYMBOL-FROM-STRING returns NIL. "
  (find-symbol (string-upcase string) package))


;;; Generics
;;; convert date-object to input list for calendrical functions
(defgeneric to-component-list (date)
  (:documentation "Extract the date components of DATE into a list. The order of the date components may differ what is shown when calling (PRINT DATE)."))

;;; convert calendar dates to absolute (fixed) dates
(defgeneric absolute-from-date (date)
  (:documentation "Convert DATE to absolute (fixed) date"))


;;; Classes
;;; General date class (from which all other date classes inherit)
(defclass date () NIL)


;;; General year-month-day class
(defclass ymd (date)
  ((year
    :initarg :year
    :accessor year
    :initform 1)
   (month
    :initarg :month
    :accessor month
    :initform 1)
   (day
    :initarg :day
    :accessor day
    :initform 1)))

(defmethod to-component-list ((date ymd))
  "Extract the components of a YMD DATE into a list. The order of the date components is MDY."
  (list (month date) (day date) (year date)))

(defmethod print-object ((x ymd) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a-~2,'0d-~2,'0d" (year x) (month x) (day x))))


;;; Gregorian date
(defclass gregorian (ymd) ())

(defun make-gregorian (year month day)
  (make-instance 'gregorian :year year :month month :day day))

(defmethod absolute-from-date ((date gregorian))
  (absolute-from-gregorian (to-component-list date)))


;;; ISO Week date
(defclass iso (date)
  ((year
    :initarg :year
    :accessor year
    :initform 1)
   (week
    :initarg :week
    :accessor week
    :initform 1)
   (day
    :initarg :day
    :accessor day
    :initform 1)))

(defun make-iso (year week day)
  (make-instance 'iso :year year :week week :day day))

(defmethod absolute-from-date ((date iso))
  (absolute-from-iso (to-component-list date)))

(defmethod print-object ((x iso) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a-W~2,'0d-~2,'0d" (year x) (week x) (day x))))


;;; Julian date
(defclass julian (ymd) ())

(defun make-julian (year month day)
  (make-instance 'julian :year year :month month :day day))

(defmethod absolute-from-date ((date julian))
  (absolute-from-julian (to-component-list date)))


;;; Islamic date
(defclass islamic (ymd) ())

(defun make-islamic (year month day)
  (make-instance 'islamic :year year :month month :day day))

(defmethod absolute-from-date ((date islamic))
  (absolute-from-islamic (to-component-list date)))


;;; Hebrew date
(defclass hebrew (ymd) ())

(defun make-hebrew (year month day)
  (make-instance 'hebrew :year year :month month :day day))

(defmethod absolute-from-date ((date hebrew))
  (absolute-from-hebrew (to-component-list date)))


;;; Mayan long count
(defclass mayan-long-count (date)
  ((baktun
    :initarg :baktun
    :accessor baktun
    :initform 0)
   (katun
    :initarg :katun
    :accessor katun
    :initform 0)
   (tun
    :initarg :tun
    :accessor tun
    :initform 0)
   (uinal
    :initarg :uinal
    :accessor uinal
    :initform 0)
   (kin
    :initarg :kin
    :accessor kin
    :initform 0)))

(defun make-mayan-long-count (baktun katun tun uinal kin)
  (make-instance 'mayan-long-count
                 :baktun baktun :katun katun :tun tun :uinal uinal :kin kin))

(defmethod to-component-list ((date mayan-long-count))
  (list (baktun date) (katun date) (tun date) (uinal date) (kin date)))

(defmethod absolute-from-date ((date mayan-long-count))
  (absolute-from-mayan-long-count (to-component-list date)))

(defmethod print-object ((x mayan-long-count) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a.~a.~a.~a.~a" (baktun x) (katun x) (tun x) (uinal x) (kin x))))


;;; Mayan haab date
(defclass mayan-haab (date)
  ((month
    :initarg :month
    :accessor month
    :initform 1)
   (day
    :initarg :day
    :accessor day
    :initform 0)))

(defun make-mayan-haab (month day)
  (make-instance 'mayan-haab :month month :day day))

(defmethod to-component-list ((date mayan-haab))
  (list (day date) (month date)))

(defmethod print-object ((x mayan-haab) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a ~a" (month x) (day x))))


;;; Mayan tzolkin date
(defclass mayan-tzolkin (date)
  ((number
    :initarg :number
    :accessor num
    :initform 1)
   (name
    :initarg :name
    :accessor name
    :initform 1)))

(defun make-mayan-tzolkin (number name)
  (make-instance 'mayan-tzolkin :number number :name name))

(defmethod to-component-list ((date mayan-tzolkin))
  (list (num date) (name date)))

(defmethod print-object ((x mayan-tzolkin) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a ~a" (num x) (name x))))


;;; French revolutionary date
(defclass french (ymd) ())

(defun make-french (year month day)
  (make-instance 'french :year year :month month :day day))

(defmethod absolute-from-date ((date french))
  (absolute-from-french (to-component-list date)))


;;; Old Hindu solar
(defclass old-hindu-solar (ymd) ())

(defun make-old-hindu-solar (year month day)
  (make-instance 'old-hindu-solar :year year :month month :day day))

(defmethod absolute-from-date ((date old-hindu-solar))
  (absolute-from-old-hindu-solar (to-component-list date)))


;;; Old Hindu lunar
(defclass old-hindu-lunar (ymd)
  ((year
    :initarg :year
    :accessor year
    :initform 1)
   (month
    :initarg :month
    :accessor month
    :initform 1)
   (leap-month
    :initarg :leap-month
    :accessor leap-month
    :initform nil)
   (day
    :initarg :day
    :accessor day
    :initform 1)))

(defun make-old-hindu-lunar (year month leap-month day)
  (make-instance 'old-hindu-lunar
                 :year year :month month :leap-month leap-month :day day))

(defmethod to-component-list ((date old-hindu-lunar))
  (list (month date) (leap-month date) (day date) (year date)))

(defmethod absolute-from-date ((date old-hindu-lunar))
  (absolute-from-old-hindu-lunar (to-component-list date)))

(defmethod print-object ((x old-hindu-lunar) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a-~2,'0d-~a-~2,'0d" (year x) (month x) (leap-month x) (day x))))


;;; Currently supported calendars
(defparameter *supported-calendars* 
  (list 
   "gregorian" 
   "iso" 
   "julian" 
   "islamic" 
   "hebrew" 
   "mayan-long-count" 
   "mayan-haab" 
   "mayan-tzolkin" 
   "french" 
   "old-hindu-solar" 
   "old-hindu-lunar"))


;;; Create dates from calendrical function outputs (as defined in calendar.lisp)
(defparameter *mdy-calendars* 
  (list 
   "gregorian" 
   "julian" 
   "islamic" 
   "hebrew" 
   "french" 
   "old-hindu-solar"))

(defun date-from-mdy-list (lst &key calendar)
  (let ((calendar-symbol (symbol-from-string calendar :calendar-utilities)))
    (destructuring-bind (m d y) lst
      (make-instance calendar-symbol 
                     :year y 
                     :month m 
                     :day d))))

(defun date-from-iso-list (lst)
  (destructuring-bind (week day year) lst
    (make-instance 'iso
                   :year year 
                   :week week 
                   :day day)))

(defun date-from-mayan-long-count-list (lst)
  (destructuring-bind (baktun katun tun uinal kin) lst
    (make-instance 'mayan-long-count
                   :baktun baktun
                   :katun  katun
                   :tun    tun
                   :uinal  uinal
                   :kin    kin)))

(defun date-from-mayan-haab-list (lst)
  (make-instance 'mayan-haab
                 :month (second lst)
                 :day   (first lst)))

(defun date-from-mayan-tzolkin-list (lst)
  (make-instance 'mayan-tzolkin
                 :number (first lst)
                 :name   (second lst)))

(defun date-from-old-hindu-lunar-list (lst)
  (destructuring-bind (month leap-month day year) lst
    (make-instance 'old-hindu-lunar
                   :year       year
                   :month      month
                   :leap-month leap-month
                   :day        day)))

(defun to-date (lst &key calendar)
  (cond ((member calendar *mdy-calendars* :test #'string-equal)
         (date-from-mdy-list lst :calendar calendar))
        ((string-equal calendar "iso")
         (date-from-iso-list lst))
        ((string-equal calendar "mayan-long-count")
         (date-from-mayan-long-count-list lst))
        ((string-equal calendar "mayan-haab")
         (date-from-mayan-haab-list lst))
        ((string-equal calendar "mayan-tzolkin")
         (date-from-mayan-tzolkin-list lst))
        ((string-equal calendar "old-hindu-lunar")
         (date-from-old-hindu-lunar-list lst))
        (t (make-instance 'date))))

(defun make-date (lst &key calendar)
  "Create an instance of a DATE class from a list LST of date components. The date components in LST must be ordered from largest calendar unit to smallest (e.g. YMD)"
  ;; calendar.lisp orders date components as MDY, LST expects components to be 
  ;; ordered from the largest unit to smallest: reordering components where 
  ;; necessary
  (let ((reordered-components
         (cond ((member calendar *mdy-calendars* :test #'string-equal)
                (destructuring-bind (y m d) lst (list m d y)))
               ((string-equal calendar "mayan-haab")
                (destructuring-bind (month day) lst (list day month)))
               ((string-equal calendar "old-hindu-lunar")
                (destructuring-bind (y m leap-m d) lst (list m leap-m d y)))
               (t lst))))
    (to-date reordered-components :calendar calendar)))

;;; Convert absolute dates to calendar dates
(defun date-from-absolute (rd &key calendar)
  "Convert absolute date RD to date representation given by CALENDAR"
  (to-date
   (cond ((string-equal calendar "gregorian")
          (gregorian-from-absolute rd))
         ((string-equal calendar "iso")
          (iso-from-absolute rd))
         ((string-equal calendar "julian")
          (julian-from-absolute rd))
         ((string-equal calendar "islamic")
          (islamic-from-absolute rd))
         ((string-equal calendar "hebrew")
          (hebrew-from-absolute rd))
         ((string-equal calendar "mayan-long-count")
          (mayan-long-count-from-absolute rd))
         ((string-equal calendar "mayan-haab")
          (mayan-haab-from-absolute rd))
         ((string-equal calendar "mayan-tzolkin")
          (mayan-tzolkin-from-absolute rd))
         ((string-equal calendar "french")
          (french-from-absolute rd))
         ((string-equal calendar "old-hindu-solar")
          (old-hindu-solar-from-absolute rd))
         ((string-equal calendar "old-hindu-lunar")
          (old-hindu-lunar-from-absolute rd))
         (t (make-instance 'date)))
   :calendar calendar))

(defun convert-date (date &key calendar)
  (date-from-absolute (absolute-from-date date) :calendar calendar))


;;; Generate formatted date strings
;;; months
(defparameter gregorian-months
  (reverse (pairlis
            (seq 1 12)
            (list "January" "February" "March"
                  "April" "May" "June"
                  "July" "August" "September"
                  "October" "November" "December"))))

(defparameter islamic-months
  (reverse (pairlis
            (seq 1 12)
            (list "Muharram" "Safar" "Rabi I"
                  "Rabi II" "Jumada I" "Jumada II"
                  "Rajab" "Sha' Ban" "Ramadan"
                  "Shawwal" "Dhu al-Qada" "Dhu al-Hijjah"))))

(defparameter hebrew-months
  (reverse (pairlis
            (seq 1 13)
            (list "Nisan" "Iyyar" "Sivan"
                  "Tammuz" "Av" "Elul"
                  "Tishri" "Heshvan" "Kislev"
                  "Teveth" "Shevat" "Adar" "Adar II"))))

(defparameter haab-months
  (reverse (pairlis
            (seq 1 18)
            (list "Pop" "Uo" "Zip"
                  "Zotz" "Tzec" "Xul"
                  "Yaxkin" "Mol" "Chen"
                  "Yax" "Zac" "Ceh"
                  "Mac" "Kankin" "Muan"
                  "Pax" "Kayab" "Cumku"))))

(defparameter tzolkin-names
  (reverse (pairlis
            (seq 1 20)
            (list "Imix" "Ik" "Akbal" "Kan"
                  "Chiccan" "Cimi" "Manik" "Lamat"
                  "Muluc" "Oc" "Chuen" "Eb"
                  "Ben" "Ix" "Men" "Cib"
                  "Caban" "Etznab" "Cauac" "Ahau"))))

(defparameter french-months
  (reverse (pairlis
            (seq 1 13)
            (list "Vendemiaire" "Brumaire" "Frimaire"
                  "Nivôse" "Pluviôse" "Ventôse"
                  "Germinal" "Floréal" "Prairial"
                  "Messidor" "Thermidor" "Fructidor"
                  "Sansculottides"))))

(defparameter french-months-ascii
  (reverse (pairlis
            (seq 1 13)
            (list "Vendemiaire" "Brumaire" "Frimaire"
                  "Nivose" "Pluviose" "Ventose"
                  "Germinal" "Floreal" "Prairial"
                  "Messidor" "Thermidor" "Fructidor"
                  "Sansculottides"))))

(defparameter old-hindu-solar-months
  (reverse (pairlis
            (seq 1 12)
            (list "Mesha" "Vrshabha" "Mithuna"
                  "Karka" "Simha" "Kanya"
                  "Tula" "Vrischika" "Dhanus"
                  "Makara" "Kumbha" "Mina"))))

(defparameter old-hindu-lunar-months
  (reverse (pairlis
            (seq 1 12)
            (list "Chaitra" "Vaisakha" "Jyaishtha"
                  "Ashadha" "Sravana" "Bhadrapada"
                  "Asvina" "Kartika" "Margasira"
                  "Pausha" "Magha" "Phalguna"))))


;;; Generate output date strings
(defgeneric display-string (date)
  (:documentation "Create string representation from DATE"))

(defparameter *use-ascii-month-names* nil)

(defmethod display-string ((date gregorian))
  (let ((month-name (get-value-from-key (month date) gregorian-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))

(defmethod display-string ((date iso))
  (format nil "~d-W~2,'0d-~2,'0d" (year date) (week date) (day date)))

(defmethod display-string ((date julian))
  (let ((month-name (get-value-from-key (month date) gregorian-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))

(defmethod display-string ((date islamic))
  (let ((month-name (get-value-from-key (month date) islamic-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))

(defmethod display-string ((date hebrew))
  (let ((month-name (get-value-from-key (month date) hebrew-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))

(defmethod display-string ((date mayan-long-count))
  (format nil "~d.~d.~d.~d.~d" (baktun date) (katun date) (tun date) (uinal date) (kin date)))

(defmethod display-string ((date mayan-haab))
  (let ((month-name (get-value-from-key (month date) haab-months)))
    (format nil "~d ~a" (day date) month-name)))

(defmethod display-string ((date mayan-tzolkin))
  (let ((tzolkin-name (get-value-from-key (name date) tzolkin-names)))
    (format nil "~d ~a" (num date) tzolkin-name)))

(defmethod display-string ((date french))
  (let ((month-name (get-value-from-key (month date) 
                                        (if (not *use-ascii-month-names*) 
                                            french-months
                                            french-months-ascii))))
    (format nil "~d ~a ~a" (day date) month-name (year date))))

(defmethod display-string ((date old-hindu-solar))
  (let ((month-name (get-value-from-key (month date) old-hindu-solar-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))

(defmethod display-string ((date old-hindu-lunar))
  (let ((month-name (get-value-from-key (month date) old-hindu-lunar-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))


;;; Create Gregorian date object instance from current date
(defun today ()
  "Return the current date as GREGORIAN-DATE"
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (make-gregorian year month day)))


;;; Extract date components from date objects 
;;; These functions are to be used for presetting date pickers
(defgeneric get-components (date)
  (:documentation "Get components of a date as an array of integers"))

(defmethod get-components ((date ymd))
  (make-array 3 :initial-contents (list (year date) (month date) (day date))
              :element-type '(signed-byte 32)))

(defmethod get-components ((date iso))
  (make-array 3 :initial-contents (list (year date) (week date) (day date))
              :element-type '(signed-byte 32)))

(defmethod get-components ((date mayan-long-count))
  (make-array 5 :initial-contents (list (baktun date) (katun date) (tun date) (uinal date) (kin date))
              :element-type '(signed-byte 32)))

(defmethod get-components ((date mayan-haab))
  (make-array 2 :initial-contents (list (month date) (day date))
              :element-type '(signed-byte 32)))

(defmethod get-components ((date mayan-tzolkin))
  (make-array 2 :initial-contents (list (num date) (name date))
              :element-type '(signed-byte 32)))


;;; Validate dates
;;; These functions are to be used for validation of date picker selections
(defun days-in-gregorian-month (month year)
  (let ((n (list 31 28 31 30 31 30 31 31 30 31 30 31)))
    (if (and (= month 2)
             (= (mod year 4) 0)
             (not (member (mod year 400) (list 100 200 300))))
        29
        (nth (1- month) n))))

(defun days-in-julian-month (month year)
  (let ((n (list 31 28 31 30 31 30 31 31 30 31 30 31)))
    (if (and (= month 2)
             (= (mod year 4) 0))
        29
        (nth (1- month) n))))

(defun days-in-islamic-month (month year)
  (if (or (not (= (mod month 2) 0))
          (and (= month 12)
               (islamic-leap-year year)))
      30
      29))

(defun days-in-hebrew-month (month year)
  (if (or (member month (list 2 4 6 10 13))
          (and (= month 12)
               (not (hebrew-leap-year year)))
          (and (= month 8)
               (not (long-heshvan year)))
          (and (= month 9)
               (short-kislev year)))
      29
      30))

(defun days-in-french-month (month year)
  (cond ((< month 13) 30) 
        ((french-leap-year year) 6)
        (t 5)))


