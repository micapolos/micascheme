(library (micac c-code)
  (export
    c-block c-block? c-block-code c-block-size
    c-block-append

    type->c-code

    const->c-code
    variable->c-code
    value->c-code

    instr->c-code)
  (import (micascheme) (micac ast) (code))

  (data (c-block code size))

  (define (empty-c-block)
    (c-block empty-code 0))

  (define (type->c-code $type)
    (type-switch $type
      ((u8? _) (code "uint8_t"))
      ((u16? _) (code "uint16_t"))
      ((u32? _) (code "uint32_t"))))

  (define (c-block-append . $c-bodies)
    (c-block
      (apply code-append (map c-block-code $c-bodies))
      (apply + (map c-block-size $c-bodies))))

  (define (type->size $type)
    (type-switch $type
      ((u8? _) 1)
      ((u16? _) 2)
      ((u32? _) 4)))

  (define (value->c-code $value $size)
    (value-switch $value
      ((const? $const) (const->c-code $const))
      ((variable? $variable) (variable->c-code $variable $size))))

  (define (const->c-code $const)
    (string-code
      (number->string
        (const-value $const))))

  (define (index->c-code $index)
    (string-code
      (string-append "v"
        (number->string $index))))

  (define (variable->c-code $variable $size)
    (index->c-code (- $size (variable-index $variable) 1)))

  (define (c-block+type $c-block $type)
    (c-block
      (code-append
        (c-block-code $c-block)
        (code
          (space-separated-code
            (type->c-code $type)
            (index->c-code (c-block-size $c-block)))
          ";\n"))
      (+ (c-block-size $c-block) 1)))

  (define (c-block+instr $c-block $instr)
    (instr-switch $instr
      ((alloc? $alloc)
        (lets
          ($c-block (fold-left c-block+type $c-block (alloc-types $alloc)))
          (fold-left c-block+instr $c-block (alloc-instrs $alloc))))
      ((ld? $ld)
        (c-block+op2 $c-block (ld-variable $ld) "=" (ld-value $ld)))
      ((add? $add)
        (c-block+op2 $c-block (add-variable $add) "+=" (add-value $add)))))

  (define (c-block+op2 $c-block $variable $op $value)
    (c-block-append $c-block
      (c-block
        (code
          (space-separated-code
            (variable->c-code $variable (c-block-size $c-block))
            (string-code $op)
            (value->c-code $value (c-block-size $c-block)))
          ";\n")
        0)))

  (define (instr->c-code $instr)
    (c-block-code (c-block+instr (empty-c-block) $instr)))
)
