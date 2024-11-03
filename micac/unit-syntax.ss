(library (micac unit-syntax)
  (export
    bit
    transform-type)
  (import
    (micascheme)
    (micac syntax))

  (define-aux-keywords bit)

  (define (size->number $size)
    (lets
      ($datum (syntax->datum $size))
      (or
        (and (number? $datum) (positive? $datum) $datum)
        (syntax-error $size "invalid size"))))

  (define (transform-type $type)
    (syntax-case $type (bit *)
      (bit
        #'uint8_t)
      ((* bit n)
        (size->uint-type #'n))
      ((* type n)
        #`(*
          #,(transform-type #'type)
          #,(size->number #'n)))))

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
