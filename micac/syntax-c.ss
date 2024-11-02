(library (micac syntax-c)
  (export
    syntax-c)
  (import
    (micascheme)
    (code)
    (micac syntax))

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

  (define (identifier->code $identifier)
    (string-code (symbol->string (syntax->datum $identifier))))

  (define (literal->code $literal)
    (switch (syntax->datum $literal)
      ((number? $fixnum) (string-code (number->string $fixnum)))
      ((string? $string) (code "\"" (string-code $string) "\""))
      ((else $other) (syntax-error $literal "not literal"))))

  (define (variable->code $variable)
    (identifier->code $variable))

  (define (value->code $value)
    (syntax-case $value ()
      (id
        (identifier? #'id)
        (variable->code #'id))
      (const
        (literal->code #'const))))

  (define (code+instrs $lookup $code $syntaxes)
    (fold-left (partial code+instr $lookup) $code $syntaxes))

  (define (instr->begin $instr)
    (syntax-case $instr (begin)
      ((begin body ...) $instr)
      (other #'(begin other))))

  (define (op2->string-opt $op)
    (syntax-case $op (set add sub and or xor shl shr)
      (set "=")
      (add "+=")
      (sub "-=")
      (and "&=")
      (or "|=")
      (xor "^=")
      (shl "<<=")
      (shr ">>=")
      (_ #f)))

  (define (code+instr $lookup $code $syntax)
    (syntax-case $syntax (begin var if switch while)
      ((begin instr ...)
        (code $code
          (code-in-curly-brackets
            (code-indent
              (code "\n"
                (code+instrs $lookup empty-code
                  (syntax->list #'(instr ...))))))
          "\n"))
      ((var type id)
        (code $code
          (space-separated-code
            (type->code #'type)
            (identifier->code #'id))
          ";\n"))
      ((if variable then-instr)
        (code $code
          (space-separated-code
            "if"
            (code-in-round-brackets
              (variable->code #'variable))
            (code+instr $lookup empty-code
              (instr->begin #'then-instr)))))
      ((if variable then-instr else-instr)
        (code $code
          (space-separated-code
            "if"
            (code-in-round-brackets
              (variable->code #'variable))
            (code+instr $lookup empty-code
              (instr->begin #'then-instr)))
          (space-separated-code
            "else"
            (code+instr $lookup empty-code
              (instr->begin #'else-instr)))))
      ((while variable instr ...)
        (code $code
          (space-separated-code
            "while"
            (code-in-round-brackets
              (variable->code #'variable))
            (code+instr $lookup empty-code
              #'(begin instr ...)))))
      ((op2 variable expr)
        (op2->string-opt #'op2)
        (code+op2 $lookup $code
          #'variable (op2->string-opt #'op2) #'expr))
      ((id arg ...)
        (and (identifier? #'id) ($lookup #'id #'micac))
        (code+instrs $lookup $code
          (begin-syntaxes
            (($lookup #'id #'micac) $syntax))))
      ((id arg ...)
        (identifier? #'id)
        (code $code
          (identifier->code #'id)
          (code-in-round-brackets
            (apply code-append
              (intercalate
                (map
                  (partial syntax->expr-code $lookup)
                  (syntax->list #`(arg ...)))
                (code ", "))))
          ";\n"))))

  (define (code+op2 $lookup $code $variable $op $expr)
    (code $code
      (space-separated-code
        (variable->code $variable)
        (string-code $op)
        (syntax->expr-code $lookup $expr))
      ";\n"))

  (define (syntax->expr-code $lookup $syntax)
    (syntax-case $syntax (= != > >= < <= + - and or xor shl shr neg not inv ref)
      ((= a b)
        (op2->expr-code $lookup #'a "==" #'b))
      ((!= a b)
        (op2->expr-code $lookup #'a "!=" #'b))
      ((> a b)
        (op2->expr-code $lookup #'a ">" #'b))
      ((>= a b)
        (op2->expr-code $lookup #'a ">=" #'b))
      ((< a b)
        (op2->expr-code $lookup #'a "<" #'b))
      ((<= a b)
        (op2->expr-code $lookup #'a "<=" #'b))
      ((+ a b)
        (op2->expr-code $lookup #'a "+" #'b))
      ((- a)
        (op1->expr-code $lookup "-" #'a))
      ((- a b)
        (op2->expr-code $lookup #'a "-" #'b))
      ((and a b)
        (op2->expr-code $lookup #'a "&" #'b))
      ((or a b)
        (op2->expr-code $lookup #'a "|" #'b))
      ((xor a b)
        (op2->expr-code $lookup #'a "^" #'b))
      ((shl a b)
        (op2->expr-code $lookup #'a "<<" #'b))
      ((shr a b)
        (op2->expr-code $lookup #'a ">>" #'b))
      ((inv a)
        (op1->expr-code $lookup "~" #'a))
      ((not a)
        (op1->expr-code $lookup "!" #'a))
      ((ref var x ...)
        (fold-left
          (lambda ($code $x)
            (syntax-case $x (*)
              (*
                (code-in-round-brackets (code "*" $code)))
              (id (identifier? #'id)
                (code $code "." (identifier->code #'id)))
              (expr
                (code $code
                  (code-in-square-brackets
                    (syntax->expr-code $lookup #'expr))))))
          (variable->code #'var)
          (syntax->list #'(x ...))))
      ((id arg ...)
        (and (identifier? #'id) ($lookup #'id #'micac))
        (syntax->expr-code $lookup
          (($lookup #'id #'micac) $syntax)))
      ((id arg ...)
        (identifier? #'id)
        (code
          (identifier->code #'id)
          (code-in-round-brackets
            (apply code-append
              (intercalate
                (map
                  (partial syntax->expr-code $lookup)
                  (syntax->list #`(arg ...)))
                (code ", "))))))
      (other
        (value->code #'other))))

  (define (op1->expr-code $lookup $op $rhs)
    (code-in-round-brackets
      (code
        (string-code $op)
        (syntax->expr-code $lookup $rhs))))

  (define (op2->expr-code $lookup $lhs $op $rhs)
    (code-in-round-brackets
      (space-separated-code
        (syntax->expr-code $lookup $lhs)
        (string-code $op)
        (syntax->expr-code $lookup $rhs))))

  (define (syntax-c $lookup . $syntaxes)
    (code-string (code+instrs $lookup empty-code $syntaxes)))
)
