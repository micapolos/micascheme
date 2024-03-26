(library (asm z80)
  (export
    org db dw dz ds align
    a b c d e h l hl ix iy ixh ixl iyh iyl
    r? r-prefix r-code r-offset

    ld)
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

  (meta define (r? $syntax)
    (syntax-case-opt $syntax (+ - a b c d e h l hl ix iy ixh ixl iyh iyl)
      (b #t)
      (c #t)
      (d #t)
      (e #t)
      (h #t)
      (ixh #t)
      (iyh #t)
      (l #t)
      (ixl #t)
      (iyl #t)
      ((hl) #t)
      ((+ ix $d) #t)
      ((- ix $d) #t)
      ((+ iy $d) #t)
      ((- iy $d) #t)
      (a #t)))

  (define-syntax r-prefix
    (syntax-rules (+ - a b c d e h l hl ix iy ixh ixl iyh iyl)
      ((_ b) #f)
      ((_ c) #f)
      ((_ d) #f)
      ((_ e) #f)
      ((_ h) #f)
      ((_ ixh) #xdd)
      ((_ iyh) #xfd)
      ((_ l) #f)
      ((_ ixl) #xdd)
      ((_ iyl) #xfd)
      ((_ (hl)) #f)
      ((_ (+ ix $d)) #xdd)
      ((_ (- ix $d)) #xdd)
      ((_ (+ iy $d)) #xfd)
      ((_ (- iy $d)) #xfd)
      ((_ a) #f)))

  (define-syntax r-code
    (syntax-rules (+ - a b c d e h l hl ix iy ixh ixl iyh iyl)
      ((_ b) #b000)
      ((_ c) #b001)
      ((_ d) #b010)
      ((_ e) #b011)
      ((_ h) #b100)
      ((_ ixh) #b100)
      ((_ iyh) #b100)
      ((_ l) #b101)
      ((_ ixl) #b101)
      ((_ iyl) #b101)
      ((_ (hl)) #b110)
      ((_ (+ ix $d)) #b110)
      ((_ (- ix $d)) #b110)
      ((_ (+ iy $d)) #b110)
      ((_ (- iy $d)) #b110)
      ((_ a) #b111)))

  (define-syntax r-offset
    (syntax-rules (+ a b c d e h l hl ix iy ixh ixl iyh iyl)
      ((_ b) #f)
      ((_ c) #f)
      ((_ d) #f)
      ((_ e) #f)
      ((_ h) #f)
      ((_ ixh) #f)
      ((_ iyh) #f)
      ((_ l) #f)
      ((_ ixl) #f)
      ((_ iyl) #f)
      ((_ (hl)) #f)
      ((_ (+ ix $d)) $d)
      ((_ (- ix $d)) (- $d))
      ((_ (+ iy $d)) $d)
      ((_ (- iy $d)) (- $d))
      ((_ a) #f)))

  (define-asm-syntax db-233
    (syntax-rules ()
      ((_ $a $b $c)
        (db (fxior (fxsll $a 6) (fxsll $b 3) $c)))))

  (define-asm-syntax ld
    (syntax-rules ()
      ((_ $r1 $r2) (and (r? #'$r1) (r? #'$r2))
        (db-233 #b01 (r-code $r1) (r-code $r2)))
      ((_ $r $n) (r? #'$r)
        (begin
          (db-233 #b00 (r-code $r) #b110)
          (db $n)))))
)
