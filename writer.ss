(library (writer)
	(export
		writer writer? writer-value writer-proc
		writer-write-char
		writer-write-string
		writer-string->value
		chars-writer)
	(import (micascheme))

	(data (writer value proc))

	(define (writer-write-char $writer $char)
		(app (writer-proc $writer) $char))

	(define (writer-write-string $writer $string)
		(fold-left writer-write-char $writer (string->list $string)))

	(define (writer-string->value $writer $string)
		(writer-value (writer-write-string $writer $string)))

	(define (chars-writer)
		(chars-push-writer (stack)))

	(define (chars-push-writer $chars)
		(writer $chars
			(lambda ($char)
				(chars-push-writer (push $chars $char)))))
)
