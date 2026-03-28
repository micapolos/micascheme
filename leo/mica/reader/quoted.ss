(library (leo mica reader quoted)
  (export
    begin-quoted-annotation
    end-quoted-annotation
    end-quoted-annotations)
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

  (define (end-quoted-annotations $annotations)
    (or
      (optional
        (list-with
          (list-annotation
            (cons
              (annotation end-quote)
              (end-quoted-annotations $annotations)))))
      $annotations))
)
