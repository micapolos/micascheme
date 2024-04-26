(library (masm runtime)
  (export
    push push-16
    drop drop-16
    dup dup-16
    out out-16
    inc dec add sub and or xor not)
  (import
    (except (micascheme) top and or xor push pop not))

  (define size #x10000)
  (define mem (make-bytevector size))
  (define sp-box (box (bytevector-length mem)))

  (define-rules-syntax ()
    ((sp) (unbox sp-box))
    ((sp $sp) (set-box! sp-box $sp)))

  (define-rule-syntax (offset $offset)
    (sp (fx+/wraparound (sp) $offset)))

  (define-rules-syntaxes ()
    ((top) (bytevector-u8-ref mem (sp)))
    ((top $u8) (bytevector-u8-set! mem (sp) $u8)))

  (define-rules-syntaxes ()
    ((top-16)
      (lets
        ($sp (sp))
        (fxior
          (bytevector-u8-ref mem $sp)
          (fxsll (bytevector-u8-ref mem (fx+/wraparound $sp 1)) 8))))
    ((top-16 $expr)
      (lets
        ($sp (sp))
        ($u16 $expr)
        (run
          (bytevector-u8-set! mem $sp (fxand #xff $u16))
          (bytevector-u8-set! mem (fx+/wraparound $sp 1) (fxsrl $u16 8))))))

  (define-rule-syntax (push $u8)
    (begin
      (offset -1)
      (top $u8)))

  (define-rule-syntax (push-16 $expr)
    (begin
      (offset -2)
      (top-16 $expr)))

  (define-rule-syntax (pop)
    (lets
      ($u8 (top))
      (run (offset 1))
      $u8))

  (define-rule-syntax (pop-16)
    (lets
      ($u16 (top-16))
      (run (offset 2))
      $u16))

  (define-rule-syntax (drop)
    (offset -1))

  (define-rule-syntax (drop-16)
    (offset -2))

  (define-rules-syntax ()
    ((out)
      (displayln (pop)))
    ((out $label)
      (begin
        (display (symbol->string (quote $label)))
        (display ": ")
        (out))))

  (define-rules-syntax ()
    ((out-16)
      (displayln (pop-16)))
    ((out-16 $label)
      (begin
        (display (symbol->string (quote $label)))
        (display ": ")
        (out-16))))

  (define-rule-syntax (dup)
    (lets
      ($u8 (top))
      (push $u8)))

  (define-rule-syntax (dup-16)
    (lets
      ($u16 (top-16))
      (push-16 $u16)))

  (define-rule-syntax (fx->u8 $x)
    (fxand $x #xff))

  (define-rule-syntax (u8+1 $x)
    (fx->u8 (fx+/wraparound $x 1)))

  (define-rule-syntax (u8-1 $x)
    (fx->u8 (fx-/wraparound $x 1)))

  (define-rule-syntax (u8-not $x)
    (fx->u8 (fxnot $x)))

  (define-rule-syntax (u8+ $x $y)
    (fx->u8 (fx+/wraparound $x $y)))

  (define-rule-syntax (u8- $x $y)
    (fx->u8 (fx-/wraparound $x $y)))

  (define-rule-syntax (op1 $id)
    (lets
      ($sp (sp))
      (bytevector-u8-set! mem $sp
        ($id (bytevector-u8-ref mem $sp)))))

  (define-rule-syntax (inc) (op1 u8+1))
  (define-rule-syntax (dec) (op1 u8-1))
  (define-rule-syntax (not) (op1 u8-not))

  (define-rule-syntax (op2 $id)
    (lets
      ($rhs (pop))
      (run (top ($id (top) $rhs)))))

  (define-rule-syntax (add) (op2 fx+/wraparound))
  (define-rule-syntax (sub) (op2 fx-/wraparound))
  (define-rule-syntax (and) (op2 fxand))
  (define-rule-syntax (or) (op2 fxior))
  (define-rule-syntax (xor) (op2 fxxor))
)
