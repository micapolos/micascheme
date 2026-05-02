(library (leo mica reader quoted)
  (export
    depth->unquoted
    begin-quoted-annotation
    end-quoted-annotations
    end-quoted-annotations?)
  (import
    (prefix (scheme) %)
    (prefix (annotation) %)
    (mica reader)
    (leo mica reader quotes))

  (%define (depth->unquoted $depth $item)
    (%if (%zero? $depth)
      $item
      (suffixed (depth->unquoted (%- $depth 1) $item) #\')))

  (%define (quoted-annotation $quote $annotation)
    (or
      (optional
        (list-annotation
          (list
            (annotation $quote)
            (lazy (quoted-annotation $quote $annotation)))))
      $annotation))

  (%define (begin-quoted-annotation $annotation)
    (quoted-annotation begin-quote $annotation))

  (%define (quoted-annotations $quote $annotations)
    (or
      (optional
        (lets
          ($end-quote-annotation (annotation $quote))
          ($annotations (lazy (quoted-annotations $quote $annotations)))
          (return
            (%map
              (%lambda ($annotation)
                (%list-annotation
                  (%list $end-quote-annotation $annotation)
                  (%annotation-source $end-quote-annotation)))
              $annotations))))
      $annotations))

  (%define (quoted-annotations? $quote $annotations?)
    (or
      (optional
        (lets
          ($end-quote-annotation (annotation $quote))
          ($annotations? (lazy (quoted-annotations? $quote $annotations?)))
          (return
            (%and $annotations?
              (%map
                (%lambda ($annotation)
                  (%list-annotation
                    (%list $end-quote-annotation $annotation)
                    (%annotation-source $end-quote-annotation)))
                $annotations?)))))
      $annotations?))

  (%define (end-quoted-annotations $annotations)
    (quoted-annotations end-quote $annotations))

  (%define (end-quoted-annotations? $annotations?)
    (quoted-annotations? end-quote $annotations?))
)
