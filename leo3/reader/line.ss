(library (leo3 reader line)
  (export
    atom-annotation
    atom

    inline?-line-annotations
    inline?-non-empty-line-annotations

    inline?-lines
    inline?-non-empty-lines)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define)
    (mica reader)
    (leo3 reader literal)
    (leo3 reader identifier))

  (define (inline?-line-annotations $inline?)
    (map
      (inline?-push-line-annotations $inline? (%stack))
      %reverse))

  (define (inline?-non-empty-line-annotations $inline?)
    (map
      (inline?-push-non-empty-line-annotations $inline? (%stack))
      %reverse))

  (define (inline?-push-line-annotations $inline? $stack)
    (one-of
      (replace eof $stack)
      (inline?-push-non-empty-line-annotations $inline? $stack)))

  (define (inline?-push-non-empty-line-annotations $inline? $stack)
    (one-of
      (prefixed ":"
        (lets
          ($rhs-annotations colon-line-annotations)
          ($sentence-annotation
            (list-annotation (return $rhs-annotations)))
          (inline?-newline-push-line-annotations $inline?
            (%push $stack $sentence-annotation))))
      (lets
        ($atom-annotation atom-annotation)
        (one-of
          (prefixed ":"
            (lets
              ($rhs-annotations colon-line-annotations)
              ($sentence-annotation
                (list-annotation (return (%cons $atom-annotation $rhs-annotations))))
              (inline?-newline-push-line-annotations $inline?
                (%push $stack $sentence-annotation))))
          (inline?-push-next-line-annotations $inline?
            (%push $stack $atom-annotation))))))

  (define (inline?-push-next-line-annotations $inline? $stack)
    (one-of
      (prefixed ", "
        (inline?-push-non-empty-line-annotations #t $stack))
      (lets
        ($newline "\n")
        (inline?-newline-push-line-annotations $inline? $stack))))

  (define (inline?-newline-push-line-annotations $inline? $stack)
    (%if $inline?
      (return $stack)
      (inline?-push-line-annotations #f $stack)))

  (define colon-line-annotations
    (one-of
      (prefixed "\n" (indented (inline?-line-annotations #f)))
      (prefixed " " (inline?-non-empty-line-annotations #t))))

  (define atom-annotation
    (one-of
      (annotation literal)
      (annotation identifier)))

  (define atom
    (apply (%annotation-stripped atom-annotation)))

  (define (inline?-lines $inline?)
    (lets
      ($annotations (inline?-line-annotations $inline?))
      (return (%map %annotation-stripped $annotations))))

  (define (inline?-non-empty-lines $inline?)
    (lets
      ($annotations (inline?-non-empty-line-annotations $inline?))
      (return (%map %annotation-stripped $annotations))))
)
