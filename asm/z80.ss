(library (asm z80)
  (export
    org db dw dz ds align r r?
    a b c d e h l hl ix iy ixh ixl iyh iyl
    r r?)
  (import
    (micascheme)
    (labs syntax)
    (asm base))

  (define (emit-u16 $emit-u8 $u16)
    (run
      ($emit-u8 (fxand $u16 #xff))
      ($emit-u8 (fxsrl $u16 8))))

  (define-asm-core-syntax org
    (lambda ($syntax $emit-u8 $org)
      (syntax-case $syntax ()
        ((_ $value)
          (and (integer? (datum $value)) (nonnegative? (datum $value)))
          (run
            ($org (datum $value))
            #`(begin))))))

  (define-asm-core-syntax db
    (lambda ($syntax $emit-u8 $org)
      (syntax-case $syntax ()
        ((_ $expr ...)
          #`(begin
            #,@(flatten
              (map syntax-flatten
                (map
                  (lambda ($expr)
                    (syntax-case $expr ()
                      ($string (string? (datum $string))
                        (lets
                          ($bytevector (string->utf8 (datum $string)))
                          ($size (bytevector-length $bytevector))
                          (run
                            ($org (+ ($org) $size))
                            #`(begin
                              #,@(map
                                (lambda ($u8) #`(#,$emit-u8 #,$u8))
                                (bytevector->u8-list $bytevector))))))
                      ($expr
                        (run
                          ($org (add1 ($org)))
                          #`(switch $expr
                            ((char? $char) (#,$emit-u8 (char->integer $char)))
                            ((else $other) (#,$emit-u8 $other)))))))
                  (syntax->list #'($expr ...))))))))))

  (define-asm-core-syntax dw
    (lambda ($syntax $emit-u8 $org)
      (syntax-case $syntax ()
        ((_ $expr ...)
          (run
            ($org (+ ($org) (* 2 (length (syntax->list #'($expr ...))))))
            #`(begin (emit-u16 #,$emit-u8 $expr) ...))))))

  (define-asm-core-syntax ds
    (lambda ($syntax $emit-u8 $org)
      (syntax-case $syntax ()
        ((_ $size)
          (and (integer? (datum $size)) (nonnegative? (datum $size)))
          #`(begin
            #,@(map
              (lambda ($index)
                ($org (add1 ($org)))
                #`(#,$emit-u8 0))
              (indices (datum $size))))))))

  (define-asm-syntax-rule (dz $expr ...)
    (db $expr ... 0))

  (define-asm-core-syntax align
    (lambda ($syntax $emit-u8 $org)
      (syntax-case $syntax ()
        ((_ $expr)
          (and (integer? (datum $expr)) (nonnegative? (datum $expr)))
          (lets
            ($pc ($org))
            ($new-pc (bitwise-align $pc (datum $expr)))
            ($slack (- $new-pc $pc))
            (run
              ($org $new-pc)
              #`(repeat #,$slack (#,$emit-u8 0))))))))

  (define-syntax-rule (define-registers $reg ...)
    (begin (define-aux-keyword $reg) ...))

  (define-registers a b c d e h l hl ix iy ixh ixl iyh iyl)

  (define-syntax-rule (db-233 $a $b $c)
    (db (fxior (fxsll $a 6) (fxsll $b 3) $c)))

  (define-syntax r?
    (syntax-rules (+ a b c d e h l hl ix iy ixh ixl iyh iyl)
      ((_ b) #t)
      ((_ c) #t)
      ((_ d) #t)
      ((_ e) #t)
      ((_ h) #t)
      ((_ ixh) #t)
      ((_ iyh) #t)
      ((_ l) #t)
      ((_ ixl) #t)
      ((_ iyl) #t)
      ((_ (hl)) #t)
      ((_ (+ ix $d)) #t)
      ((_ (+ iy $d)) #t)
      ((_ a) #t)
      ((_ $) #f)))

  (define-syntax r
    (syntax-rules (+ a b c d e h l hl ix iy ixh ixl iyh iyl)
      ((_ b) (values #f #b000 #f))
      ((_ c) (values #f #b001 #f))
      ((_ d) (values #f #b010 #f))
      ((_ e) (values #f #b011 #f))
      ((_ h) (values #f #b100 #f))
      ((_ ixh) (values #xdd #b100 #f))
      ((_ iyh) (values #xfd #b100 #f))
      ((_ l) (values #f #b101 #f))
      ((_ ixl) (values #xdd #b101 #f))
      ((_ iyl) (values #xfd #b101 #f))
      ((_ (hl)) (values #f #b110 #f))
      ((_ (+ ix $d)) (values #xdd #b110 $d))
      ((_ (+ iy $d)) (values #xfd #b110 $d))
      ((_ a) (values #f #b111 #f))))
)
