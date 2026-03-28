(library (leo mica reader single-line)
  (export
    single-line-annotation)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (mica reader)
    (leo mica reader identifier)
    (leo mica reader literal))

  (define single-line-annotation
    (one-of
      single-line-list-annotation
      single-line-non-list-annotation))

  (define single-line-non-list-annotation
    (one-of
      single-line-sentence-annotation
      single-line-atom-annotation))

  (define single-line-sentence-annotation
    (lets
      ($identifier-annotation (annotation identifier))
      (switch (optional single-line-rhs-annotations)
        ((%false? _)
          (return $identifier-annotation))
        ((else $annotations)
          (list-annotation
            (cons
              (return $identifier-annotation)
              (return $annotations)))))))

  (define single-line-atom-annotation
    (annotation
      (one-of
        number
        string-literal
        special-literal)))

  (define single-line-annotations
    (wrapped
      "("
      (separated ", " single-line-non-list-annotation)
      ")"))

  (define single-line-list-annotation
    (list-annotation single-line-annotations))

  (define single-line-rhs-annotations
    (prefixed " "
      (one-of
        (map single-line-non-list-annotation %list)
        single-line-annotations)))
)
