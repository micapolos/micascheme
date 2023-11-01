(library (tico tico)
  (export
    tico-load
    tico-read
    tico-parse)
  (import
    (micascheme)
    (leo parser)
    (tico reader))

  (define (tico-parse $string)
    (tico-eval (parse-script $string)))

  (define (tico-read $port)
    (tico-eval (read-script $port)))

  (define (tico-load $filename)
    (tico-eval (load-script $filename)))
)
