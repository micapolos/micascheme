(library (source-object)
  (export
    source-object=?
    source-object-append
    source-object->syntax)
  (import
    (chezscheme)
    (source-file-descriptor)
    (syntax))

  (define (source-object=? $a $b)
    (and
      (source-file-descriptor=?
        (source-object-sfd $a)
        (source-object-sfd $b))
      (=
        (source-object-bfp $a)
        (source-object-bfp $b))
      (=
        (source-object-efp $a)
        (source-object-efp $b))
      (eq?
        (source-object-line $a)
        (source-object-line $b))
      (eq?
        (source-object-column $a)
        (source-object-column $b))))


  (define (source-object-append $source-object . $source-objects)
    (make-source-object
      (source-object-sfd $source-object)
      (apply min (map source-object-bfp (cons $source-object $source-objects)))
      (apply max (map source-object-efp (cons $source-object $source-objects)))))

  (define (source-object->syntax $source-object)
    #`(make-source-object
      #,(source-file-descriptor->syntax (source-object-sfd $source-object))
      #,(literal->syntax (source-object-bfp $source-object))
      #,(literal->syntax (source-object-efp $source-object))
      #,@(let
        (($line (source-object-line $source-object))
         ($column (source-object-column $source-object)))
        (if (and $line $column)
          #`(list
            #,(literal->syntax $line)
            #,(literal->syntax $column))
          #'()))))
)
