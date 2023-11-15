(library (writing)
	(export
		writing writing? writing-writer-write-proc
		writer-write-writing
		empty-writing
		char-writing
		string-writing
		datum-writing
		writing-string
		writing-append
		writing-indent)
	(import
		(micascheme)
		(writer))

	(data (writing writer-write-proc))

	(define (writer-write-writing $writer $writing)
		(app 
			(writing-writer-write-proc $writing) 
			$writer))

	(define (empty-writing)
		(writing identity))

	(define (char-writing $char)
		(writing 
			(lambda ($writer)
				(writer-write-char $writer $char))))

	(define (string-writing $string)
		(writing 
			(lambda ($writer)
				(writer-write-string $writer $string))))

	(define (datum-writing $datum)
		(writing 
			(lambda ($writer)
				(writer-write-string $writer 
					(format "~s" $datum)))))

	(define (writing-string $writing)
		(list->string 
			(reverse 
				(writer-value 
					(writer-write-writing 
						(chars-writer) 
						$writing)))))

	(define (writing-append . $writings)
		(writing
			(lambda ($writer)
				(fold-left
					writer-write-writing
					$writer
					$writings))))

	(define (writing-indent $space-count $writing)
		(writing
			(lambda ($writer)
				(writer-value 
					(writer-write-writing 
						(indented-writer $space-count $writer)
						$writing)))))
)