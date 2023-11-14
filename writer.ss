(library (writer)
	(export
		writer writer? writer-value writer-proc
		writer-write-char
		writer-write-chars
		writer-write-string
		writer-string->value
		char-stack-writer
		trim-end-writer)
	(import (micascheme))

	(data (writer value proc))

	(define (writer-write-char $writer $char)
		(app (writer-proc $writer) $char))

	(define (writer-write-chars $writer $chars)
		(fold-left writer-write-char $writer $chars))

	(define (writer-write-string $writer $string)
		(writer-write-chars $writer (string->list $string)))

	(define (writer-string->value $writer $string)
		(writer-value (writer-write-string $writer $string)))

	(define char-stack-writer 
		(case-lambda
			(() 
				(char-stack-writer (stack)))
			(($char-stack)
				(writer $char-stack
					(lambda ($char)
						(char-stack-writer 
							(push $char-stack $char)))))))

	(define trim-end-writer 
		(case-lambda
			(($writer) 
				(trim-end-writer $writer (stack)))
			(($writer $end-whitespaces)
				(writer
					(writer-value $writer)
					(lambda ($char)
						(cond
							((char=? $char #\newline)
								(trim-end-writer 
									(writer-write-char $writer $char)))
							((char-whitespace? $char)
								(trim-end-writer $writer (push $end-whitespaces $char)))
							(else 
								(trim-end-writer
									(writer-write-char
										(writer-write-chars $writer (reverse $end-whitespaces))
										$char)))))))))
)
