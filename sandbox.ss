(import (micascheme))

(define (db $u8)
  (displayln `(db ,$u8)))

(define (db-233 $a $b $c)
  (displayln `(db-233 ,$a ,$b ,$c)))

(define-aux-keywords ld b c d e h l hl a)

(define-syntax-rule (matcher-lets ($matcher-expr $param ...) $body)
  (opt-lets
    ($matcher $matcher-expr)
    ($matcher
      (lambda ($param ...)
        (syntax-case #`(#,$param ...) ()
          (($param ...)
            $body))))))

(displayln
  (syntax->datum
    (matcher-lets ((lambda ($fn) ($fn #'#b110 #'#b101)) $a $b)
      #`(db-233 #b01 $a $b))))

(define-syntax (asm $syntax)
  (define (r $syntax)
    (syntax-case $syntax (b c d e h l a)
      (b (lambda ($fn) ($fn #'#b000)))
      (c (lambda ($fn) ($fn #'#b001)))
      (d (lambda ($fn) ($fn #'#b010)))
      (e (lambda ($fn) ($fn #'#b011)))
      (h (lambda ($fn) ($fn #'#b100)))
      (l (lambda ($fn) ($fn #'#b101)))
      (a (lambda ($fn) ($fn #'#b111)))
      (_ #f))

    (syntax-case $syntax (ld)
      ((_ (ld a b))
        (matcher-lets ((r #'a) $code-a)
          (matcher-lets ((r #'b) $code-b)
            #`(db-233 #b01 $code-a $code-b)))))))

(asm (ld b c))
