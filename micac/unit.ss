(library (micac unit)
  (export
    bit
    size->value
    size->uint-type
    transform-decl)
  (import
    (micascheme)
    (micac syntax))

  (define-aux-keywords bit)

  (define (size->value $size)
    (switch (syntax->datum $size)
      ((integer? $integer)
        (if (positive? $integer)
          $integer
          (syntax-error $size "not positive")))
      ((else _)
        (syntax-error $size "not integer"))))

  (define (size->uint-type $size)
    (lets
      ($bits (size->value $size))
      (cond
        ((<= $bits 8) #`uint8_t)
        ((<= $bits 16) #`uint16_t)
        ((<= $bits 32) #`uint32_t)
        ((<= $bits 64) #`uint64_t)
        (else (syntax-error $size "64 <")))))

  (define (transform-decl $decl)
    (syntax-case $decl (bit)
      ((id bit)
        #`(var uint8_t id))
      ((id (* bit n))
        #`(var #,(size->uint-type #'n) id))
      ((id (* type n))
        (syntax-case (transform-decl #'(id type)) (var)
          ((var type id)
            #`(var type (* id #,(size->value #'n))))))))
)
