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
		 --eval "(print (let ((x (mapcar (lambda (cal) (calcal:display-string (calcal:date-from-absolute (calcal:absolute-from-date (calcal:today)) :calendar cal))) calcal::*supported-calendars*))) (format nil \"~{~a~%~}\" x)))" \
		 --eval "(terpri)" \
		 --quit
