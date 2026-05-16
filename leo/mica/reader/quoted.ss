(library (leo mica reader quoted)
  (export
    depth->unquoted
    depth-quoted-annotation
    depth-unquoted-annotations
    depth-unquoted-annotations?
    begin-quoted-annotation
    end-quoted-annotations
    end-quoted-annotations?)
  (import
    (prefix (scheme) %)
    (prefix (annotation) %)
    (prefix (procedure) %)
    (mica reader)
    (leo mica reader quotes))

  (%define (depth->unquoted $depth $item)
    (%if (%zero? $depth)
      $item
      (suffixed (depth->unquoted (%- $depth 1) $item) #\')))

  (%define (depth-quoted-annotation $depth $quote $depth->annotation)
    (or
      (optional
        (list-annotation
          (list
            (annotation $quote)
            (lazy
              (depth-quoted-annotation
                (%+ $depth 1)
                $quote
                $depth->annotation)))))
      ($depth->annotation $depth)))

  (%define (begin-quoted-annotation $annotation)
    (depth-quoted-annotation 0 begin-quote (%always $annotation)))

  (%define (depth-unquoted-annotations $depth $unquote $depth->annotations)
    (or
      (optional
        (lets
          ($end-quote-annotation (annotation $unquote))
          ($annotations
            (lazy
              (depth-unquoted-annotations
                (%- $depth 1)
                $unquote
                $depth->annotations)))
          (return
            (%map
              (%lambda ($annotation)
                (%list-annotation
                  (%list $end-quote-annotation $annotation)
                  (%annotation-source $end-quote-annotation)))
              $annotations))))
      ($depth->annotations $depth)))

  (%define (end-quoted-annotations $annotations)
    (depth-unquoted-annotations 0 end-quote (%always $annotations)))

  (%define (depth-unquoted-annotations? $depth $unquote $depth->annotations?)
    (or
      (optional
        (lets
          ($end-quote-annotation (annotation $unquote))
          ($annotations?
            (lazy
              (depth-unquoted-annotations?
                (%- $depth 1)
                $unquote
                $depth->annotations?)))
          (return
            (%and $annotations?
              (%map
                (%lambda ($annotation)
                  (%list-annotation
                    (%list $end-quote-annotation $annotation)
                    (%annotation-source $end-quote-annotation)))
                $annotations?)))))
      ($depth->annotations? $depth)))

  (%define (end-quoted-annotations? $annotations?)
    (depth-unquoted-annotations? 0 end-quote (%always $annotations?)))
)
