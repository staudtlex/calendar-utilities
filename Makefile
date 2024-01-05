.PHONY: test indent

all: calendar.lisp
test: test_with_quicklisp

calendar.lisp: calendar.l modify.patch
	cp calendar.l $@
	patch $@ < modify.patch

indent: calendar-utilities.lisp
	emacs -nw --no-x-resources \
		--eval "(find-file \"calendar-utilities.lisp\")" \
		--eval "(lisp-mode)" \
		--eval "(indent-region (point-min) (point-max) nil)" \
		--eval "(save-buffer)" \
		--kill; \
		printf "\n"

test_with_quicklisp: 
	sbcl --eval "(push #p\"../calendar-utilities/\" asdf:*central-registry*)" \
		 --eval "(ql:quickload \"calendar-utilities\")" \
		 --eval "(in-package :cl)" \
		 --eval "(print (calcal:display-string (calcal:date-from-absolute (calcal:absolute-from-date (calcal:make-date (list 2024 01 01) :calendar \"gregorian\")) :calendar \"iso\")))" \
		 --eval "(print (calcal:display-string (calcal:date-from-absolute (calcal:absolute-from-date (calcal::make-gregorian 2024 01 01)) :calendar \"iso\")))" \
		 --eval "(terpri)" \
		 --quit
