(library (leo writer-reader)
	(export
		writer-reader
		script-string)
	(import
		(micascheme)
		(writer)
		(leo reader)
		(leo writer))

  (data (writers empty? newline-writer space-writer-opt))

  (define (writer-write-symbol $writer $symbol)
    (writer-write-string $writer (symbol->string $symbol)))

  (define (writer-write-literal $writer $literal)
    (writer-write-string $writer (format "~s" $literal)))

  (define-syntax-rule (script-string $item ...)
  	(do-writer-string $writer
  		(reader-eval (writer-reader $writer) $item ...)))

  (define-reader (writers-reader $writers $end)
    (lets
      ($empty? (writers-empty? $writers))
      ($newline-writer (writers-newline-writer $writers))
      ($space-writer-opt (writers-space-writer-opt $writers))
      (reader
        (lambda ($literal)
          (writers-reader 
            (writers
              #f ; empty?
              (writer-write-literal
                (if $empty?
                	$newline-writer
                  (writer-write-char $newline-writer #\newline))
                $literal)
              (and $empty? $space-writer-opt
                (writer-write-literal $space-writer-opt $literal)))
            $end))
        (lambda ($symbol)
          (writers-reader
            (writers
              #t ; $empty?
              (indented-writer 2
                (writer-write-symbol
                  (if $empty? $newline-writer (writer-write-char $newline-writer #\newline))
                  $symbol))
              (and $empty? $space-writer-opt
                (writer-write-symbol
                  (writer-write-char $space-writer-opt #\space)
                  $symbol)))
            (lambda ($symbol-writers)
              (writer-reader
                (writers
                  #f ; $empty?
                  (writer-value (writers-newline-writer $symbol-writers))
                  (writers-space-writer-opt $symbol-writers))
                $end))))
        (lambda ()
          ($end
            (writers
              $empty?
              (if $empty? $newline-writer (writer-write-char $newline-writer #\newline))
              (and $space-writer-opt
                (if $empty? $space-writer-opt (writer-write-char $space-writer-opt #\newline)))))))))

  (define-reader (writer-reader $writer $end)
    (writers-reader
      (writers
        #t ; empty?
        $writer
        $writer)
      (lambda ($writers)
        (or
          (writers-space-writer-opt $writers)
          (writers-newline-writer $writers)))))
)