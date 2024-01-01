.PHONY: test

all: calendar.lisp
test: test_with_quicklisp

calendar.lisp: calendar.l
	cp calendar.l $@
	patch $@ < modify.patch

test_with_quicklisp: 
	sbcl --eval "(push #p\"../calendar-utilities/\" asdf:*central-registry*)" \
		 --eval "(ql:quickload \"calendar-utilities\")" \
		 --eval "(print (display-string (date-from-absolute (absolute-from-date (make-gregorian 2024 01 01)) :calendar \"iso\")))" \
		 --eval "(terpri)" \
		 --quit
