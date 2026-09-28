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
    (lets
      ($atom-annotation atom-annotation)
      (%switch (%annotation-stripped $atom-annotation)
        ((%symbol? _)
          (one-of
            (prefixed ":"
              (lets
                ($rhs-annotations
                  (one-of
                    (prefixed "\n" (indented (inline?-line-annotations #f)))
                    (prefixed " " (inline?-non-empty-line-annotations #t))))
                ($sentence-annotation
                  (list-annotation (return (%cons $atom-annotation $rhs-annotations))))
                ($line-annotations (return (%push $stack $sentence-annotation)))
                (%if $inline?
                  (return $line-annotations)
                  (inline?-push-line-annotations #f $line-annotations))))
            (inline?-push-next-line-annotations $inline?
              (%push $stack $atom-annotation))))
        ((%else _)
          (inline?-push-next-line-annotations $inline?
            (%push $stack $atom-annotation))))))

  (define (inline?-push-next-line-annotations $inline? $stack)
    (one-of
      (prefixed ", "
        (inline?-push-non-empty-line-annotations #t $stack))
      (lets
        ($newline "\n")
        (%if $inline?
          (return $stack)
          (inline?-push-line-annotations #f $stack)))))

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
