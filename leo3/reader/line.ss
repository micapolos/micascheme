(library (leo3 reader line)
  (export
    line
    lines

    atom
    atom-annotation

    line-annotation
    line-annotations

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
                (return (%push $stack $sentence-annotation))))
            (prefixed ", "
              (inline?-push-non-empty-line-annotations #t
                (%push $stack $atom-annotation)))
            (lets
              ($newline "\n")
              (%if $inline?
                (return (%push $stack $atom-annotation))
                (inline?-push-line-annotations #f (%push $stack $atom-annotation))))))
        ((%else _)
          (one-of
            (prefixed ", "
              (inline?-push-non-empty-line-annotations #t
                (%push $stack $atom-annotation)))
            (lets
              ($newline "\n")
              (%if $inline?
                (return (%push $stack $atom-annotation))
                (inline?-push-line-annotations #f (%push $stack $atom-annotation)))))))))

  (define sentence-annotation
    (lets
      ($identifier-annotation (annotation identifier))
      (one-of
        (replace #\newline $identifier-annotation)
        (lets
          ($rhs-line-annotations
            (prefixed #\:
              (one-of
                (prefixed #\space (list line-annotation))
                (prefixed #\newline (indented line-annotations)))))
          (list-annotation
            (return
              (%cons $identifier-annotation $rhs-line-annotations)))))))

  (define atom-annotation
    (one-of
      (annotation literal)
      (annotation identifier)))

  (define line-annotation
    (one-of
      (suffixed (annotation literal) #\newline)
      sentence-annotation))

  (define line-annotations
    (reject?-list-of %char-newline? line-annotation))

  (define rhs-line-annotations-opt
    (one-of
      (prefixed #\:
        (one-of
          (prefixed #\space (list line-annotation))
          (prefixed #\newline (indented line-annotations))))
      (return %null)))

  (define atom
    (apply (%annotation-stripped atom-annotation)))

  (define line
    (apply (%annotation-stripped line-annotation)))

  (define lines
    (lets
      ($annotations line-annotations)
      (return (%map %annotation-stripped $annotations))))

  (define (inline?-lines $inline?)
    (lets
      ($annotations (inline?-line-annotations $inline?))
      (return (%map %annotation-stripped $annotations))))

  (define (inline?-non-empty-lines $inline?)
    (lets
      ($annotations (inline?-non-empty-line-annotations $inline?))
      (return (%map %annotation-stripped $annotations))))
)
