(library (micac c-code)
  (export
    c-body c-body? c-body-code c-body-size
    c-body-append

    type->c-code

    const->c-code
    variable->c-code
    value->c-code

    instr->c-code)
  (import (micascheme) (micac ast) (code))

  (data (c-body code size))

  (define (empty-c-body)
    (c-body empty-code 0))

  (define (type->c-code $type)
    (type-switch $type
      ((u8? _) (code "uint8_t"))
      ((u16? _) (code "uint16_t"))
      ((u32? _) (code "uint32_t"))))

  (define (c-body-append . $c-bodies)
    (c-body
      (apply code-append (map c-body-code $c-bodies))
      (apply + (map c-body-size $c-bodies))))

  (define (type->size $type)
    (type-switch $type
      ((u8? _) 1)
      ((u16? _) 2)
      ((u32? _) 4)))

  (define (value->c-code $value)
    (value-switch $value
      ((const? $const) (const->c-code $const))
      ((variable? $variable) (variable->c-code $variable))))

  (define (const->c-code $const)
    (string-code
      (number->string
        (const-value $const))))

  (define (variable->c-code $variable)
    (string-code
      (string-append "_"
        (number->string
          (variable-index $variable)))))

  (define (c-body+instr $c-body $instr)
    (instr-switch $instr
      ((alloc? $alloc)
        (c-body
          (code
            (c-body-code $c-body)
            (space-separated-code
              (type->c-code (alloc-type $alloc))
              (variable->c-code (variable (c-body-size $c-body))))
            ";\n")
          (+ (c-body-size $c-body) 1)))
      ((ld? $ld)
        (c-body+op2 $c-body (ld-variable $ld) "=" (ld-value $ld)))
      ((add? $add)
        (c-body+op2 $c-body (add-variable $add) "+=" (add-value $add)))
      ((block? $block)
        (fold-left c-body+instr $c-body (block-instrs $block)))))

  (define (c-body+op2 $c-body $variable $op $value)
    (c-body-append $c-body
      (c-body
        (code
          (space-separated-code
            (variable->c-code $variable)
            $op
            (value->c-code $value))
          ";\n")
        0)))

  (define (instr->c-code $instr)
    (c-body-code (c-body+instr (empty-c-body) $instr)))
)
