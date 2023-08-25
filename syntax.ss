(library (syntax)
  (export 
    tuple-syntax 
    value-tuple
    tuple-ref-syntax

    index-syntax
    value-index
    index-switch-syntax

    indexed-syntax
    value-indexed
    indexed-switch-syntax)
  (import (micascheme))

  (define (tuple-syntax $syntaxes)
    (case (length $syntaxes)
      ((0) #`#f)
      ((1) (car $syntaxes))
      ((2) #`(cons #,(car $syntaxes) #,(cadr $syntaxes)))
      (else #`(vector #,@$syntaxes))))

  (define (value-tuple $size $value)
    (case $size
      ((0) (list))
      ((1) (list $value))
      ((2) (list (car $value) (cdr $value)))
      (else (vector->list $value))))

  (define (tuple-ref-syntax $syntax $index $size)
    (case $size
      ((1) $syntax)
      ((2) #`(#,(if (= $index 0) #`car #`cdr) #,$syntax))
      (else #`(vector-ref #,$syntax #,$index))))

  (define (index-syntax $index $size)
    (case $size
      ((1) #`#f)
      ((2) (= $index 0))
      (else $index)))

  (define (value-index $size $value)
    (case $size
      ((1) 0)
      ((2) (if $value 0 1))
      (else $value)))

  (define (index-switch-syntax $selector $cases)
    (lets 
      ($size (length $cases))
      ($last-index (- $size 1))
      (case $size
        ((1) (car $cases))
        ((2) #`(if #,$selector #,@$cases))
        (else #`(case #,$selector
          #,@(map-indexed
            (lambda ($index $case)
              #`(
                #,(if (= $index $last-index) #`else #`(#,$index))
                #,$case))
            $cases))))))

  (define (indexed-syntax $index $size $value-syntax)
    (case $size
      ((1) $value-syntax)
      (else #`(cons #,(index-syntax $index $size) #,$value-syntax))))

  (define (value-indexed $size $value)
    (case $size
      ((1) (cons 0 $value))
      (else (cons (value-index $size (car $value)) (cdr $value)))))

  (define (indexed-switch-syntax $indexed-syntax $value-identifier $case-syntaxes)
    (case (length $case-syntaxes)
      ((1)
        #`(lets
          (#,$value-identifier #,$indexed-syntax)
          #,(car $case-syntaxes)))
      (else
        (lets
          ($indexed-identifier (generate-temporary #`indexed))
          #`(lets
            (#,$indexed-identifier #,$indexed-syntax)
            (#,$value-identifier (cdr #,$indexed-identifier))
            #,(index-switch-syntax #`(car #,$indexed-identifier) $case-syntaxes))))))
)
