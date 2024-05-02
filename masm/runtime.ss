(library (masm runtime)
  (export
    reset
    grow grow-16
    drop drop-16
    push push-16
    dup dup-16
    out out-16
    inc dec inc-16 dec-16
    add sub add-16 sub-16
    and or xor not
    proc
    do switch)
  (import
    (except (micascheme) top and or xor push pop not reset load switch do))

  (define size #x10000)
  (define mem (make-bytevector size))
  (define sp-box (box size))

  (define-rules-syntax
    ((sp) (unbox sp-box))
    ((sp $sp) (set-box! sp-box $sp)))

  (define-rule-syntax (reset)
    (sp size))

  (define-rule-syntax (offset $offset)
    (sp (fx+/wraparound (sp) $offset)))

  (define-rules-syntaxes
    ((grow) (grow 1))
    ((grow-16) (grow 2))
    ((grow $offset) (offset (fx-/wraparound 0 $offset))))

  (define-rules-syntaxes
    ((top) (bytevector-u8-ref mem (sp)))
    ((top $u8) (bytevector-u8-set! mem (sp) $u8)))

  (define-rules-syntaxes
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

  (define-rules-syntaxes
    ((drop) (drop 1))
    ((drop-16) (drop 2))
    ((drop $offset) (offset $offset)))

  (define-rules-syntax
    ((out)
      (displayln (pop)))
    ((out $label)
      (begin
        (display (symbol->string (quote $label)))
        (display ": ")
        (out))))

  (define-rules-syntax
    ((out-16)
      (displayln (pop-16)))
    ((out-16 $label)
      (begin
        (display (symbol->string (quote $label)))
        (display ": ")
        (out-16))))

  (define-rules-syntax
    ((dup) (dup 0))
    ((dup $offset)
      (lets
        ($u8 (bytevector-u8-ref mem (fx+/wraparound (sp) $offset)))
        (push $u8))))

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

  (define-rule-syntax (fx->u16 $x)
    (fxand $x #xffff))

  (define-rule-syntax (u16+1 $x)
    (fx->u16 (fx+/wraparound $x 1)))

  (define-rule-syntax (u16-1 $x)
    (fx->u16 (fx-/wraparound $x 1)))

  (define-rule-syntax (u16+ $x $y)
    (fx->u16 (fx+/wraparound $x $y)))

  (define-rule-syntax (u16- $x $y)
    (fx->u16 (fx-/wraparound $x $y)))

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
      (top ($id (top) $rhs))))

  (define-rule-syntax (add) (op2 u8+))
  (define-rule-syntax (sub) (op2 u8-))
  (define-rule-syntax (and) (op2 fxand))
  (define-rule-syntax (or) (op2 fxior))
  (define-rule-syntax (xor) (op2 fxxor))

  (define-rule-syntax (op1-16 $id)
    (lets
      ($u16 (top-16))
      (top-16 ($id $u16))))

  (define-rule-syntax (inc-16) (op1-16 u16+1))
  (define-rule-syntax (dec-16) (op1-16 u16-1))

  (define-rule-syntax (op2-16 $id)
    (lets
      ($rhs (pop-16))
      (top-16 ($id (top-16) $rhs))))

  (define-rule-syntax (add-16) (op2-16 u16+))
  (define-rule-syntax (sub-16) (op2-16 u16-))

  (define-rule-syntax (proc $id $instr ...)
    (define ($id) (run $instr ... (void))))

  (define-rule-syntax (do $instr ...)
    (begin $instr ...))

  (define-syntax (switch $syntax)
    (syntax-case $syntax ()
      ((_ $case ... $default)
        #`(case (pop)
          #,@(map-indexed
            (lambda ($index $case)
              #`((#,$index) #,$case))
            (syntax->list #'($case ...)))
          (else $default)))))
)
