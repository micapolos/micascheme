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
