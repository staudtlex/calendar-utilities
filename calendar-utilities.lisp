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

;;; Generics
;;; convert date-object to input list for calendrical functions
(defgeneric to-list (date)
  (:documentation "Convert date to list"))

;;; convert calendar dates to absolute (fixed) dates
(defgeneric absolute-from-date (date)
  (:documentation "Convert date to absolute (fixed) date"))

;;; Classes
;;; General year-month-day class
(defclass ymd ()
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

(defmethod to-list ((date ymd))
  (list (month date) (day date) (year date)))

(defmethod print-object ((x ymd) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a-~2,'0d-~2,'0d" (year x) (month x) (day x))))


;;; Gregorian date
(defclass gregorian (ymd) ())

(defun make-gregorian (year month day)
  (make-instance 'gregorian :year year :month month :day day))

(defmethod absolute-from-date ((date gregorian))
  (absolute-from-gregorian (to-list date)))


;;; ISO Week date
(defclass iso ()
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
  (absolute-from-iso (to-list date)))

(defmethod print-object ((x iso) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a-W~2,'0d-~2,'0d" (year x) (week x) (day x))))


;;; Julian date
(defclass julian (ymd) ())

(defun make-julian (year month day)
  (make-instance 'julian :year year :month month :day day))

(defmethod absolute-from-date ((date julian))
  (absolute-from-julian (to-list date)))


;;; Islamic date
(defclass islamic (ymd) ())

(defun make-islamic (year month day)
  (make-instance 'islamic :year year :month month :day day))

(defmethod absolute-from-date ((date islamic))
  (absolute-from-islamic (to-list date)))


;;; Hebrew date
(defclass hebrew (ymd) ())

(defun make-hebrew (year month day)
  (make-instance 'hebrew :year year :month month :day day))

(defmethod absolute-from-date ((date hebrew))
  (absolute-from-hebrew (to-list date)))


;;; Mayan long count
(defclass mayan-long-count ()
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

(defmethod to-list ((date mayan-long-count))
  (list (baktun date) (katun date) (tun date) (uinal date) (kin date)))

(defmethod absolute-from-date ((date mayan-long-count))
  (absolute-from-mayan-long-count (to-list date)))

(defmethod print-object ((x mayan-long-count) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a.~a.~a.~a.~a" (baktun x) (katun x) (tun x) (uinal x) (kin x))))


;;; Mayan haab date
(defclass mayan-haab ()
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

(defmethod to-list ((date mayan-haab))
  (list (day date) (month date)))

(defmethod absolute-from-date ((date mayan-haab))
  (absolute-from-mayan-haab (to-list date)))

(defmethod print-object ((x mayan-haab) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a ~a" (month x) (day x))))


;;; Mayan tzolkin date
(defclass mayan-tzolkin ()
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

(defmethod to-list ((date mayan-tzolkin))
  (list (num date) (name date)))

(defmethod absolute-from-date ((date mayan-tzolkin))
  (absolute-from-mayan-tzolkin (to-list date)))

(defmethod print-object ((x mayan-tzolkin) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a ~a" (num x) (name x))))

;;; French revolutionary date
(defclass french (ymd) ())

(defun make-french (year month day)
  (make-instance 'french :year year :month month :day day))

(defmethod absolute-from-date ((date french))
  (absolute-from-french (to-list date)))


;;; Old Hindu solar
(defclass old-hindu-solar (ymd) ())

(defun make-old-hindu-solar (year month day)
  (make-instance 'old-hindu-solar :year year :month month :day day))

(defmethod absolute-from-date ((date old-hindu-solar))
  (absolute-from-old-hindo-solar (to-list date)))


;;; Old Hindu lunar
(defclass old-hindu-lunar ()
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

(defmethod to-list ((date old-hindu-lunar))
  (list (month date) (leap-month date) (day date) (year date)))

(defmethod absolute-from-date ((date old-hindu-lunar))
  (absolute-from-old-hindu-lunar (to-list date)))

(defmethod print-object ((x old-hindu-lunar) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~a-~2,'0d-~a-~2,'0d" (year x) (month x) (leap-month x) (day x))))


;;; Create dates from calendrical function outputs
(defun to-date (lst &key type)
  (cond ((string-equal type "gregorian")
         (make-instance 'gregorian
                        :year  (nth 2 lst)
                        :month (nth 0 lst)
                        :day   (nth 1 lst)))
        ((string-equal type "julian")
         (make-instance 'julian
                        :year  (nth 2 lst)
                        :month (nth 0 lst)
                        :day   (nth 1 lst)))
        ((string-equal type "islamic")
         (make-instance 'islamic
                        :year  (nth 2 lst)
                        :month (nth 0 lst)
                        :day   (nth 1 lst)))
        ((string-equal type "hebrew")
         (make-instance 'hebrew
                        :year  (nth 2 lst)
                        :month (nth 0 lst)
                        :day   (nth 1 lst)))
        ((string-equal type "french")
         (make-instance 'french
                        :year  (nth 2 lst)
                        :month (nth 0 lst)
                        :day   (nth 1 lst)))
        ((string-equal type "old-hindu-solar")
         (make-instance 'old-hindu-solar
                        :year  (nth 2 lst)
                        :month (nth 0 lst)
                        :day   (nth 1 lst)))
        ((string-equal type "iso")
         (make-instance 'iso
                        :year (nth 2 lst)
                        :week (nth 0 lst)
                        :day  (nth 1 lst)))
        ((string-equal type "mayan-long-count")
         (make-instance 'mayan-long-count
                        :baktun (nth 0 lst)
                        :katun  (nth 1 lst)
                        :tun    (nth 2 lst)
                        :uinal  (nth 3 lst)
                        :kin    (nth 4 lst)))
        ((string-equal type "mayan-haab")
         (make-instance 'mayan-haab
                        :month (nth 1 lst)
                        :day   (nth 0 lst)))
        ((string-equal type "mayan-tzolkin")
         (make-instance 'mayan-tzolkin
                        :number (nth 0 lst)
                        :name   (nth 1 lst)))
        ((string-equal type "old-hindu-lunar")
         (make-instance 'old-hindu-lunar
                        :year       (nth 3 lst)
                        :month      (nth 0 lst)
                        :leap-month (nth 1 lst)
                        :day        (nth 2 lst)))
        (t nil)))


;;; Convert absolute dates to calendar dates
(defun date-from-absolute (absolute-date &key calendar)
  "Convert absolute date to given calendar"
  (to-date
   (cond ((string-equal calendar "gregorian")
          (gregorian-from-absolute absolute-date))
         ((string-equal calendar "iso")
          (iso-from-absolute absolute-date))
         ((string-equal calendar "julian")
          (julian-from-absolute absolute-date))
         ((string-equal calendar "islamic")
          (islamic-from-absolute absolute-date))
         ((string-equal calendar "hebrew")
          (hebrew-from-absolute absolute-date))
         ((string-equal calendar "mayan-long-count")
          (mayan-long-count-from-absolute absolute-date))
         ((string-equal calendar "mayan-haab")
          (mayan-haab-from-absolute absolute-date))
         ((string-equal calendar "mayan-tzolkin")
          (mayan-tzolkin-from-absolute absolute-date))
         ((string-equal calendar "french")
          (french-from-absolute absolute-date))
         ((string-equal calendar "old-hindu-solar")
          (old-hindu-solar-from-absolute absolute-date))
         ((string-equal calendar "old-hindu-lunar")
          (old-hindu-lunar-from-absolute absolute-date)))
   :type calendar))

(defun convert-date (date &key calendar)
  (date-from-absolute (absolute-from-date date) :calendar calendar))


;;; Generate formatted date strings
;;; months
(defun seq (from to)
  ;; Generate a sequence of consecutive integers
  (let ((length (1+ (- to from))))
    (loop :for n :below length :collect (+ from n))))

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


(defun get-value-from-key (key alist)
  "Return the value associated with a given key"
  (cdr (assoc key alist)))


;;; Generate output date strings
(defgeneric display-string (date)
  (:documentation "Create string representation from `date'"))

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
  (let ((month-name (get-value-from-key (month date) french-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))

(defmethod display-string ((date old-hindu-solar))
  (let ((month-name (get-value-from-key (month date) old-hindu-solar-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))

(defmethod display-string ((date old-hindu-lunar))
  (let ((month-name (get-value-from-key (month date) old-hindu-lunar-months)))
    (format nil "~d ~a ~a" (day date) month-name (year date))))


;;; Validate dates
(defun today ()
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    (make-gregorian year month day)))

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

(defmethod days-in-islamic-month (month year)
  (if (or (not (= (mod month 2) 0))
          (and (= month 12)
               (islamic-leap-year year)))
      30
      29))

(defmethod days-in-hebrew-month (month year)
  (if (or (member month (list 2 4 6 10 13))
          (and (= month 12)
               (not (hebrew-leap-year year)))
          (and (= month 8)
               (not (long-heshvan year)))
          (and (= month 9)
               (short-kislev year)))
      29
      30))

(defmethod days-in-french-month (month year)
  (if (< month 13)
      30
      (if (french-leap-year year)
          6
          5)))
