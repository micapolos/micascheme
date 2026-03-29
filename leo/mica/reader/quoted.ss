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

  (define (quoted-annotation $quote $annotation)
    (or
      (optional
        (list-annotation
          (list
            (annotation $quote)
            (lazy (quoted-annotation $quote $annotation)))))
      $annotation))

  (define (begin-quoted-annotation $annotation)
    (quoted-annotation begin-quote $annotation))

  (define (end-quoted-annotation $annotation)
    (quoted-annotation end-quote $annotation))

  (define (end-quoted-annotations $annotations)
    (or
      (optional
        (list
          (list-annotation
            (cons
              (annotation end-quote)
              (end-quoted-annotations $annotations)))))
      $annotations))
)
