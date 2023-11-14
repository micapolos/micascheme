(import
	(micascheme)
	(writer))

(define (writer-string $writer $string)
	(list->string 
		(reverse 
			(writer-value 
				(writer-write-string $writer $string)))))

(check
	(equal?
		(writer-string (char-stack-writer) "foo")
		"foo"))

(check
	(equal?
		(writer-string 
			(trim-end-writer (char-stack-writer))
			"foo bar \n  goo \n  \n")
		"foo bar\n  goo\n\n"))

(check
	(equal?
		(writer-string 
			(indent-writer (char-stack-writer) 2)
			"foo\n\nbar\n")
		"foo\n  \n  bar\n  "))
