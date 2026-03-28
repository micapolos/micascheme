(library (leo mica reader quoted)
  (export
    begin-quoted-annotation
    end-quoted-annotation)
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

  (define (end-quoted-annotation $annotation)
    (or
      (optional
        (list-annotation
          (list-with
            (annotation end-quote)
            (lazy (end-quoted-annotation $annotation)))))
      $annotation))
)
