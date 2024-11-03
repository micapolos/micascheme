(library (micac unit-syntax)
  (export
    bit
    transform-uint-type)
  (import
    (micascheme)
    (micac syntax))

  (define-aux-keywords bit struct)

  (define (size->number $size)
    (lets
      ($datum (syntax->datum $size))
      (or
        (and (number? $datum) (positive? $datum) $datum)
        (syntax-error $size "invalid size"))))

  (define (scope-ref $scope $id)
    (or
      (assid $scope $id)
      (syntax-error $id "unbound")))

  (define (transform-uint-type $type)
    (syntax-case $type (bit *)
      ((* bit n)
        (size->uint-type #'n))))

  (define (size->uint-type $size)
    (lets
      ($size (size->number $size))
      (cond
        ((<= $size 8) #'uint8_t)
        ((<= $size 16) #'uint16_t)
        ((<= $size 32) #'uint32_t)
        ((<= $size 64) #'uint64_t)
        (else (syntax-error $size "bit size greater than 64")))))
)
