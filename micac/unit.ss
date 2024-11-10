(library (micac unit)
  (export
    size->value
    size->uint-type
    init update define-unit)
  (import
    (micascheme)
    (syntax)
    (micac syntax)
    (micac c))

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

  (define (size->pot-value $size)
    (datum->syntax #'+ (bitwise-arithmetic-shift-left 1 (size->value $size))))

  (define (size->mask $size)
    (datum->syntax #'+ (- (size->pot-value $size) 1)))

  (define-micac init
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ id)
            (app
              (or
                ($lookup #'id #'init)
                (syntax-error $syntax "not unit"))
              $syntax))))))

  (define-micac update
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ id)
            (app
              (or
                ($lookup #'id #'update)
                (syntax-error $syntax "not unit"))
              $syntax))))))

  (define-syntax define-unit
    (lambda ($syntax)
      (lambda ($lookup)
        (syntax-case $syntax (init update)
          ((_ (id args ...)
            (init init-body ...)
            (update update-body ...))
            #`(begin
              (define-aux-keyword id)
              (define-property id init
                (lambda ($syntax)
                  #`(begin init-body ...)))
              (define-property id update
                (lambda ($syntax)
                  #`(begin init-body ...)))))))))
)
