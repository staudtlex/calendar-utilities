(defsystem "calendar-utilities"
  :description ""
  :version "0.5"
  :author "Alexander Staudt <staudtlex@live.de>"
  :licence "AGPL-3"
  :components ((:static-file "calendar-utilities.asd")
               (:static-file "LICENSE")
               (:static-file "README.md")
               (:file "package")
               (:file "calendar" :depends-on ("package"))
               (:file "calendar-utilities" :depends-on ("calendar"))))
