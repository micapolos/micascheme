(library (zexy res)
  (export)
  (import
    (micascheme)
    (zexy math))

  (data (res stack org labels))

  (define (res-push $res $syntax)
    (res
      (push (res-stack $res) $syntax)
      (res-org $res)
      (res-labels $res)))

  (define (res... $res . $syntaxes)
    (fold-left res-push $res $syntaxes))

  (define (res-db-n $res $n)
    (res-push $res #`(db #'#,$n)))

  (define (res-dw-nm $res $nm)
    (res-push $res #`(dw #'#,$nm)))

  (define (res-org-nm $res $nm)
    (res
      (res-stack $res)
      $nm
      (res-labels $res)))

  (define (res-label $res $label)
    (res
      (res-stack $res)
      (res-org $res)
      (push (res-labels $res) (cons $label (res-org $res)))))

  (define (res-op $res $syntax)
    (or
      (syntax-case $syntax ()
        (($op $arg ...)
          (identifier? #'$op)
          (case (datum $op)
            ((db) #f)
            (else #f))))
      (res-push $res $syntax)))
)
