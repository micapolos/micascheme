(library (leo mica reader quoted)
  (export begin-quoted-annotation)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (mica reader)
    (leo mica reader quotes))

  (define (begin-quoted-annotation $annotation)
    (or
      (optional
        (list-annotation
          (list-with
            (annotation begin-quote)
            (lazy (begin-quoted-annotation $annotation)))))
      $annotation))
)
