(library (source-object)
  (export
    source-object=?
    append-source-object)
  (import
    (chezscheme)
    (source-file-descriptor))

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


  (define (append-source-object $source-object . $source-objects)
    (make-source-object
      (source-object-sfd $source-object)
      (apply min (map source-object-bfp (cons $source-object $source-objects)))
      (apply max (map source-object-efp (cons $source-object $source-objects)))))
)
