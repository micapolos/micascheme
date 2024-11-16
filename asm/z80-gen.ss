(library (asm z80-gen)
  (export z80-gen a b c d e f h l hl ld nop)
  (import (micascheme))

  (define-aux-keywords a f b c d e h l)
  (define-aux-keywords af bc de hl sp)

  (define-aux-keywords ld nop)

  (define (db $u8)
    (displayln `(db ,$u8)))

  (define regs-8 #`(b c d e h l (hl) a))
  (define regs-16-af #`(bc de hl af))
  (define regs-16-sp #`(bc de hl sp))

  (define reg-8-vector (list->vector (syntax->list regs-8)))

  (define (u233 $a $b $c)
    (fxior (fxsll $a 6) (fxsll $b 3) $c))

  (define (z80-gen)
    #`(syntax-rules (b c d e h l hl a ld nop)
      #,@(map-product
        (lambda ($rrr $ppp)
          #`(
            #,(op-01-rrr-ppp $rrr $ppp)
            (begin (db #,(u233 #b01 $rrr $ppp)))))
        (indices 8)
        (indices 8))))

  (define (op-01-rrr-ppp $rrr $ppp)
    (if (and (= $rrr #b110) (= $ppp #b110))
      #`(nop)
      #`(ld
        #,(vector-ref reg-8-vector $rrr)
        #,(vector-ref reg-8-vector $ppp))))
)
