(library (leo writing-reader)
	(export
    writing-reader
		script-string)
	(import
		(micascheme)
		(writing)
		(leo reader))

  (define (symbol-writing $symbol)
    (string-writing (symbol->string $symbol)))

  (define-reader (writings-reader $end)
    (push-writings-reader (stack) $end))

  (define-reader (push-writings-reader $writings $end)
    (reader
      (lambda ($literal)
        (push-writings-reader
          (push $writings (datum-writing $literal))
          $end))
      (lambda ($symbol)
        (writings-reader
          (lambda ($rhs-writings)
            (push-writings-reader
              (push $writings
                (writing-append
                  (symbol-writing $symbol)
                  (case (length $rhs-writings)
                    ((0) 
                      (empty-writing))
                    ((1) 
                      (writing-append
                        (char-writing #\space)
                        (car $rhs-writings)))
                    (else 
                      (writing-indent 2
                        (apply writing-append
                          (map
                            (partial writing-append (char-writing #\newline))
                            (reverse $rhs-writings))))))))
              $end))))
      (lambda ()
        ($end $writings))))

  (define-reader (writing-reader $end)
    (writings-reader 
      (lambda ($writings)
        ($end
          (apply writing-append
            (map
              (lambda ($writing)
                (writing-append $writing
                  (char-writing #\newline)))
              (reverse $writings)))))))

  (define-syntax-rule (script-string $item ...)
    (writing-string
      (reader-eval (writing-reader) $item ...)))
)