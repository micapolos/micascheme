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

  (function (tico-eval $datum)
    (typing-value
      (reader-end
        (reader-read-list (typing-reader (stack)) $datum))))

  (function (tico-parse $string)
    (tico-eval (parse-script $string)))

  (function (tico-read $port)
    (tico-eval (read-script $port)))

  (function (tico-load $filename)
    (tico-eval (load-script $filename)))
)
