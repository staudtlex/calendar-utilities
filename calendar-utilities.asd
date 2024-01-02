(defpackage :calendar-utilities
  (:nicknames #:cc)
  (:use #:cl #:asdf)
  (:export #:make-date-from-list
           #:today
           #:display-string
           #:date-from-absolute
           #:absolute-from-date))

(defsystem "calendar-utilities"
  :description ""
  :version "0.5"
  :author "Alexander Staudt <staudtlex@live.de>"
  :licence "AGPL-3"
  :components ((:static-file "calendar-utilities.asd")
               (:static-file "LICENSE")
               (:static-file "README.md")
               (:file "calendar")
               (:file "calendar-utilities" :depends-on ("calendar"))))
