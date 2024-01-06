(defpackage :calendar-utilities
  (:nicknames #:calcal)
  (:use #:cl #:asdf)
  (:export #:make-date
           #:make-gregorian
           #:today
           #:display-string
           #:date-from-absolute
           #:absolute-from-date
           #:*supported-calendars*
           #:*use-ascii-month-names*))
