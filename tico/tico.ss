(library (tico tico)
  (export
    tico-load
    tico-read
    tico-parse)
  (import
    (micascheme)
    (leo parser)
    (leo reader)
    (tico reader)
    (tico typing))

  (define (tico-eval $datum)
    (typing-value
      (car
        (ensure single?
          (reader-end
            (reader-read-list typings-reader $datum))))))

  (define (tico-parse $string)
    (tico-eval (parse-script $string)))

  (define (tico-read $port)
    (tico-eval (read-script $port)))

  (define (tico-load $filename)
    (tico-eval (load-script $filename)))
)
