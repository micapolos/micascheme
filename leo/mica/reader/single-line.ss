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
      (lets
        ($identifier-annotation (annotation identifier))
        (switch (optional single-line-rhs-annotations)
          ((%false? _)
            (return $identifier-annotation))
          ((else $annotations)
            (list-annotation (return (%cons $identifier-annotation $annotations))))))
      (annotation number)
      (annotation string-literal)
      (annotation special-literal)))

  (define single-line-rhs-annotations
    (prefixed " "
      (one-of
        (map single-line-annotation %list)
        single-line-annotations)))

  (define single-line-annotations
    (wrapped "(" (separated ", " single-line-annotation) ")"))
)
