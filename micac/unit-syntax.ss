(library (micac unit-syntax)
  (export
    bit
    transform-type)
  (import
    (micascheme)
    (micac syntax))

  (define-aux-keywords bit)

  (define (transform-type $type)
    (syntax-case $type (bit *)
      (bit
        #'uint8_t)
      ((* bit n)
        (size->uint-type #'n))
      ((* type n)
        #`(* #,(transform-type #'type) n))))

  (define (size->uint-type $size)
    (lets
      ($datum (syntax->datum $size))
      (or
        (and
          (number? $datum)
          (positive? $datum)
          (cond
            ((<= $datum 8) #'uint8_t)
            ((<= $datum 16) #'uint16_t)
            ((<= $datum 32) #'uint32_t)
            ((<= $datum 64) #'uint64_t)
            (else #f)))
        (syntax-error $size "invalid bit size"))))
)
