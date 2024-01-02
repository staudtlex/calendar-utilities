.PHONY: test

all: calendar.lisp
test: test_with_quicklisp

calendar.lisp: calendar.l modify.patch
	cp calendar.l $@
	patch $@ < modify.patch

test_with_quicklisp: 
	sbcl --eval "(push #p\"../calendar-utilities/\" asdf:*central-registry*)" \
		 --eval "(ql:quickload \"calendar-utilities\")" \
		 --eval "(in-package :calendar-utilities)" \
		 --eval "(print (display-string (date-from-absolute (absolute-from-date (make-date-from-list (list 01 01 2024) :calendar \"gregorian\")) :calendar \"iso\")))" \
		 --eval "(print (display-string (date-from-absolute (absolute-from-date (make-gregorian 2024 01 01)) :calendar \"iso\")))" \
		 --eval "(terpri)" \
		 --eval "(in-package :cl)" \
		 --eval "(print (cc:display-string (cc:date-from-absolute (cc:absolute-from-date (cc:make-date-from-list (list 01 01 2024) :calendar \"gregorian\")) :calendar \"iso\")))" \
		 --eval "(print (cc:display-string (cc:date-from-absolute (cc:absolute-from-date (cc::make-gregorian 2024 01 01)) :calendar \"iso\")))" \
		 --eval "(terpri)" \
		 --quit
