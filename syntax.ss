(library (syntax)
  (export 
    tuple-syntax 
    tuple-ref-syntax
    index-syntax
    index-switch-syntax
    indexed-syntax
    indexed-switch-syntax)
  (import (micascheme))

  (define (tuple-syntax $syntaxes)
    (case (length $syntaxes)
      ((0) #`#f)
      ((1) (car $syntaxes))
      ((2) #`(cons #,(car $syntaxes) #,(cadr $syntaxes)))
      (else #`(vector #,@$syntaxes))))

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
