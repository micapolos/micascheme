(library (micac syntax-c)
  (export
    syntax-c)
  (import
    (micascheme)
    (code)
    (micac syntax))

  (data (block code identifiers))

  (data (expr priority code))

  (define empty-block
    (block empty-code (stack)))

  (define (size->code $size)
    (lets
      ($datum (syntax->datum $size))
      (or
        (and
          (integer? $datum)
          (nonnegative? $datum)
          (number-code $datum))
        (syntax-error $size "invalid size"))))

  (define (type->code $type)
    (syntax-case $type (u8 u16 u32 *)
      (u8 (code "uint8_t"))
      (u16 (code "uint16_t"))
      (u32 (code "uint32_t"))
      ((* type) (code (type->code #'type) "*"))
      ((* type size)
        (code
          (type->code #'type)
          (code-in-square-brackets (size->code #'size))))
      (_ (syntax-error $type "unknown type"))))

  (define (block-append . $blocks)
    (block
      (apply code-append (map block-code $blocks))
      (apply append (map block-identifiers $blocks))))

  (define (block-begin $block)
    (block empty-code (block-identifiers $block)))

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

  (define (block+instrs $block $syntaxes)
    (fold-left block+instr $block $syntaxes))

  (define (instr->begin $instr)
    (syntax-case $instr (begin)
      ((begin body ...) $instr)
      (other #'(begin other))))

  (define (op->string $op)
    (syntax-case $op (set add sub and or xor)
      (set "=")
      (add "+=")
      (sub "-=")
      (and "&=")
      (or "|=")
      (xor "^=")
      (_ (syntax-error $op "invalid op"))))

  (define (block+instr $block $syntax)
    (syntax-case $syntax (begin var if switch while print)
      ((begin instr ...)
        (lets
          ($begin-block
            (block+instrs
              (block-begin $block)
              (syntax->list #'(instr ...))))
          (block
            (code
              (block-code $block)
              (code-in-curly-brackets
                (code-indent
                  (code "\n"
                    (block-code $begin-block))))
              "\n")
            (block-identifiers $block))))
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
      ((if variable then-instr)
        (block
          (code-append
            (block-code $block)
            (code
              (space-separated-code
                "if"
                (code-in-round-brackets
                  (variable->code
                    (block-identifiers $block)
                    #'variable))
                (block-code (block+instr (block-begin $block) (instr->begin #'then-instr))))))
          (block-identifiers $block)))
      ((if variable then-instr else-instr)
        (block
          (code-append
            (block-code $block)
            (code
              (space-separated-code
                "if"
                (code-in-round-brackets
                  (variable->code
                    (block-identifiers $block)
                    #'variable))
                (block-code (block+instr (block-begin $block) (instr->begin #'then-instr))))
              (space-separated-code
                "else"
                (block-code (block+instr (block-begin $block) (instr->begin #'else-instr))))))
          (block-identifiers $block)))
      ((while variable instr ...)
        (block
          (code-append
            (block-code $block)
            (code
              (space-separated-code
                "while"
                (code-in-round-brackets
                  (variable->code
                    (block-identifiers $block)
                    #'variable))
                (block-code
                  (block+instr
                    (block-begin $block)
                    #'(begin instr ...))))))
          (block-identifiers $block)))
      ((print label expr)
        (block
          (code-append
            (block-code $block)
            (code
              "printf"
              (code-in-round-brackets
                (code
                  "\""
                  (string-code (symbol->string (datum label)))
                  ": %i\\n"
                  "\""
                  ", "
                  (syntax->expr-code (block-identifiers $block) #'expr)))
              ";\n"))
          (block-identifiers $block)))
      ((op variable expr)
        (block+op2 $block #'variable (op->string #'op) #'expr))))

  (define (block+op2 $block $variable $op $expr)
    (block-append $block
      (block
        (code
          (space-separated-code
            (variable->code (block-identifiers $block) $variable)
            (string-code $op)
            (syntax->expr-code (block-identifiers $block) $expr))
          ";\n")
        (block-identifiers $block))))

  (define (syntax->expr-code $identifiers $syntax)
    (syntax-case $syntax (u8 u16 u32 + - and or xor)
      ((u8 number)
        (number-code (datum number)))
      ((u16 number)
        (number-code (datum number)))
      ((u32 number)
        (number-code (datum number)))
      ((+ a b)
        (op2->expr-code $identifiers #'a "+" #'b))
      ((- a b)
        (op2->expr-code $identifiers #'a "-" #'b))
      ((and a b)
        (op2->expr-code $identifiers #'a "&" #'b))
      ((or a b)
        (op2->expr-code $identifiers #'a "|" #'b))
      ((xor a b)
        (op2->expr-code $identifiers #'a "^" #'b))
      (other
        (value->code $identifiers #'other))))

  (define (op2->expr-code $identifiers $lhs $op $rhs)
    (code-in-round-brackets
      (space-separated-code
        (syntax->expr-code $identifiers $lhs)
        (string-code $op)
        (syntax->expr-code $identifiers $rhs))))

  (define (syntax-c . $syntaxes)
    (code-string
      (block-code
        (block+instrs empty-block $syntaxes))))
)
