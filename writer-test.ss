(import
	(micascheme)
	(writer))

(check
	(equal?
		(writer-string->value (chars-writer) "foo")
		(stack #\f #\o #\o)))