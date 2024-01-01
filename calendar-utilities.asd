(defsystem "calendar-utilities"
  :description ""
  :version "0.5"
  :author "Alexander Staudt <staudtlex@live.de>"
  :licence "AGPL-3"
  :components ((:file "calendar")
               (:file "calendar-utilities" :depends-on ("calendar"))))
