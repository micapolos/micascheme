(library (micac syntax-c)
  (export
    syntax-c)
  (import
    (micascheme)
    (code)
    (micac syntax))

  (data (block code identifiers))

  (define empty-block
    (block empty-code (stack)))

  (define (type->code $type)
    (syntax-case $type (u8 u16 u32)
      (u8 (code "uint8_t"))
      (u16 (code "uint16_t"))
      (u32 (code "uint32_t"))
      (_ (syntax-error $type "unknown type"))))

  (define (block-append . $blocks)
    (block
      (apply code-append (map block-code $blocks))
      (apply append (map block-identifiers $blocks))))

  (define (identifier->code $identifier)
    (string-code (symbol->string (syntax->datum $identifier))))

  (define (bound-identifier $identifiers $identifier)
    (or
      (find (partial free-identifier=? $identifier) $identifiers)
      (syntax-error $identifier "unbound variable")))

  (define (literal->code $literal)
    (switch (syntax->datum $literal)
      ((fixnum? $fixnum) (string-code (number->string $fixnum)))
      ((else $other) (syntax-error $literal "not literal"))))

  (define (variable->code $identifiers $variable)
    (or
      (and
        (find (partial free-identifier=? $variable) $identifiers)
        (identifier->code $variable))
      (syntax-error $variable "unbound variable")))

  (define (value->code $identifiers $value)
    (syntax-case $value ()
      (id
        (identifier? #'id)
        (variable->code $identifiers #'id))
      (const
        (literal->code #'const))))

  (define (block+instr $block $syntax)
    (syntax-case $syntax (begin var set add sub and or xor)
      ((begin instr ...)
        (fold-left block+instr $block (syntax->list #'(instr ...))))
      ((var type id)
        (block
          (code-append
            (block-code $block)
            (code
              (space-separated-code
                (type->code #'type)
                (identifier->code #'id))
              ";\n"))
          (push (block-identifiers $block) #'id)))
      ((set variable value)
        (block+op2 $block #'variable "=" #'value))
      ((add variable value)
        (block+op2 $block #'variable "+=" #'value))
      ((sub variable value)
        (block+op2 $block #'variable "-=" #'value))
      ((and variable value)
        (block+op2 $block #'variable "&=" #'value))
      ((or variable value)
        (block+op2 $block #'variable "|=" #'value))
      ((xor variable value)
        (block+op2 $block #'variable "^=" #'value))))

  (define (block+op2 $block $variable $op $value)
    (block-append $block
      (block
        (code
          (space-separated-code
            (variable->code (block-identifiers $block) $variable)
            (string-code $op)
            (value->code (block-identifiers $block) $value))
          ";\n")
        (block-identifiers $block))))

  (define (syntax-c $syntax)
    (code-string (block-code (block+instr empty-block $syntax))))
)
