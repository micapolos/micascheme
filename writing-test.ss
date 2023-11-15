(import
	(micascheme)
	(writing))

(check (equal? (writing-string (empty-writing)) ""))
(check (equal? (writing-string (char-writing #\a)) "a"))
(check (equal? (writing-string (string-writing "foo")) "foo"))

(check (equal? (writing-string (datum-writing 10)) "10"))
(check (equal? (writing-string (datum-writing "foo")) "\"foo\""))
(check (equal? (writing-string (datum-writing '(10 20))) "(10 20)"))

(check (equal? (writing-string (writing-append)) ""))
(check 
	(equal? 
		(writing-string 
			(writing-append 
				(datum-writing 10) 
				(char-writing #\space)
				(datum-writing 20)))
		"10 20"))

(check 
	(equal? 
		(writing-string (writing-indent 2 (string-writing "foo\nbar\n")))
		"foo\n  bar\n  "))
