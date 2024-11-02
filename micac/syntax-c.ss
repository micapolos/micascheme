(library (micac syntax-c)
  (export
    syntax-c)
  (import
    (micascheme)
    (code)
    (micac syntax))

  (data (expr priority left-to-right? code))

  (define (expr-operand-code $expr $priority $right?)
    (if
      (or
        (< (expr-priority $expr) $priority)
        (and
          (= (expr-priority $expr) $priority)
          (boolean=? (expr-left-to-right? $expr) (not $right?))))
      (expr-code $expr)
      (code-in-round-brackets (expr-code $expr))))

  (define (binary-expr $priority $left-to-right? $lhs $op $rhs)
    (expr $priority $left-to-right?
      (code
        (expr-operand-code $lhs $priority #f)
        (string-code $op)
        (expr-operand-code $rhs $priority #t))))

  (define (prefix-expr $priority $left-to-right? $op $expr)
    (expr $priority $left-to-right?
      (code
        (string-code $op)
        (expr-operand-code $expr $priority #t))))

  (define (paren-expr $priority $left-to-right? $lhs $lparen $inner $rparen)
    (expr $priority $left-to-right?
      (code
        (expr-operand-code $lhs $priority #f)
        (string-code $lparen)
        (expr-code $inner)
        (string-code $rparen))))

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
    (syntax-case $type ()
      (id (identifier? #'id)
        (identifier->code #'id))
      (_
        (syntax-error $type "unknown type"))))

  (define (declarator->code $syntax)
    (syntax-case $syntax (*)
      (id (identifier? #'id)
        (type->code #'id))
      ((* decl)
        (code "*" (declarator->code #'decl)))
      ((* decl expr)
        (code
          (declarator->code #'decl)
          (code-in-square-brackets
            (number-code (datum expr)))))))

  (define (identifier->code $identifier)
    (string-code (symbol->string (syntax->datum $identifier))))

  (define (literal->expr $literal)
    (switch (syntax->datum $literal)
      ((number? $fixnum)
        (expr 0 #t (string-code (number->string $fixnum))))
      ((string? $string)
        (expr 0 #t (code "\"" (string-code $string) "\"")))
      ((else $other)
        (syntax-error $literal "not literal"))))

  (define (variable->expr $variable)
    (expr 0 #t (identifier->code $variable)))

  (define (lhs->expr $lookup $syntax)
    (syntax-case $syntax ()
      (id (identifier? #'id)
        (variable->expr #'id))
      (other
        (ref->expr $lookup #'other))))

  (define (value->expr $value)
    (syntax-case $value ()
      (id
        (identifier? #'id)
        (variable->expr #'id))
      (const
        (literal->expr #'const))))

  (define (code+instrs $lookup $code $syntax)
    (syntax-case $syntax (defer break-if)
      (() $code)
      (((defer deferred ...) body ...)
        (code+instrs $lookup
          (code+instrs $lookup $code #'(body ...))
          #'(deferred ...)))
      (((break-if expr break-body ...) body ...)
        (code+instr $lookup $code
          #`(if expr
            (begin break-body ...)
            (begin body ...))))
      (((id arg ...) body ...)
        (and (identifier? #'id) ($lookup #'id #'micac))
        (code+instrs $lookup $code
          #`(
            #,@(begin-syntaxes
              (($lookup #'id #'micac) #`(id arg ...)))
            body ...)))
      ((other body ...)
        (code+instrs $lookup
          (code+instr $lookup $code #'other)
          #'(body ...)))))

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
                (code+instrs $lookup empty-code #'(instr ...)))))
          "\n"))
      ((var type id)
        (code $code
          (space-separated-code
            (type->code #'type)
            (declarator->code #'id))
          ";\n"))
      ((var type id expr)
        (code $code
          (space-separated-code
            (type->code #'type)
            (declarator->code #'id)
            "="
            (expr-code (syntax->expr $lookup #'expr)))
          ";\n"))
      ((if expr then-instr)
        (code $code
          (space-separated-code
            "if"
            (code-in-round-brackets
              (expr-code (syntax->expr $lookup #'expr)))
            (code+instr $lookup empty-code
              (instr->begin #'then-instr)))))
      ((if expr then-instr else-instr)
        (code $code
          (space-separated-code
            "if"
            (code-in-round-brackets
              (expr-code (syntax->expr $lookup #'expr)))
            (code+instr $lookup empty-code
              (instr->begin #'then-instr)))
          (space-separated-code
            "else"
            (code+instr $lookup empty-code
              (instr->begin #'else-instr)))))
      ((while expr instr ...)
        (code $code
          (space-separated-code
            "while"
            (code-in-round-brackets
              (expr-code (syntax->expr $lookup #'expr)))
            (code+instr $lookup empty-code
              #'(begin instr ...)))))
      ((op2 lhs expr)
        (op2->string-opt #'op2)
        (code+op2 $lookup $code
          #'lhs (op2->string-opt #'op2) #'expr))
      ((id arg ...)
        (and (identifier? #'id) ($lookup #'id #'micac))
        (code+instrs $lookup $code
          #`(
            #,@(begin-syntaxes
              (($lookup #'id #'micac) $syntax)))))
      ((id arg ...)
        (identifier? #'id)
        (code $code
          (expr-code
            (paren-expr 1 #t
              (variable->expr #'id)
              "("
              (expr 0 #t
                (apply code-append
                  (intercalate
                    (map expr-code
                      (map
                        (partial syntax->expr $lookup)
                        (syntax->list #`(arg ...))))
                    (code ", "))))
              ")"))
          ";\n"))))

  (define (code+op2 $lookup $code $lhs $op $expr)
    (code $code
      (space-separated-code
        (expr-code (lhs->expr $lookup $lhs))
        (string-code $op)
        (expr-code (syntax->expr $lookup $expr)))
      ";\n"))

  (define (syntax->expr $lookup $syntax)
    (syntax-case $syntax (= != > >= < <= + - * and or bitwise-and bitwise-ior bitwise-xor shl shr neg not inv ref &ref)
      ((= a b)
        (op2->expr $lookup 7 #t #'a " == " #'b))
      ((!= a b)
        (op2->expr $lookup 7 #t #'a " != " #'b))
      ((> a b)
        (op2->expr $lookup 6 #t #'a " > " #'b))
      ((>= a b)
        (op2->expr $lookup 6 #t #'a " >= " #'b))
      ((< a b)
        (op2->expr $lookup 6 #t #'a " < " #'b))
      ((<= a b)
        (op2->expr $lookup 6 #t #'a " <= " #'b))
      ((+)
        (syntax->expr $lookup #'0))
      ((+ a)
        (syntax->expr $lookup #'a))
      ((+ a b)
        (op2->expr $lookup 4 #t #'a " + " #'b))
      ((+ a b cs ...)
        (syntax->expr $lookup #`(+ (+ a b) cs ...)))
      ((- a)
        (op1->expr $lookup 2 #f "-" #'a))
      ((- a b)
        (op2->expr $lookup 4 #t #'a " - " #'b))
      ((- a b cs ...)
        (syntax->expr $lookup #`(- (- a b) cs ...)))
      ((*)
        (syntax->expr $lookup #'1))
      ((* a)
        (syntax->expr $lookup #'a))
      ((* a b)
        (op2->expr $lookup 3 #t #'a " * " #'b))
      ((* a b cs ...)
        (syntax->expr $lookup #`(* (* a b) cs ...)))
      ((and)
        (syntax->expr $lookup #'true))
      ((and a)
        (syntax->expr $lookup #'a))
      ((and a b)
        (op2->expr $lookup 11 #t #'a " && " #'b))
      ((and a b cs ...)
        (syntax->expr $lookup #`(and (and a b) cs ...)))
      ((or)
        (syntax->expr $lookup #'false))
      ((or a)
        (syntax->expr $lookup #'a))
      ((or a b)
        (op2->expr $lookup 12 #t #'a " || " #'b))
      ((or a b cs ...)
        (syntax->expr $lookup #`(or (or a b) cs ...)))
      ((bitwise-and)
        (syntax->expr $lookup #'-1))
      ((bitwise-and a)
        (syntax->expr $lookup #'a))
      ((bitwise-and a b)
        (op2->expr $lookup 8 #t #'a " & " #'b))
      ((bitwise-and a b cs ...)
        (syntax->expr $lookup #`(bitwise-and (bitwise-and a b) cs ...)))
      ((bitwise-ior)
        (syntax->expr $lookup #'0))
      ((bitwise-ior a)
        (syntax->expr $lookup #'a))
      ((bitwise-ior a b)
        (op2->expr $lookup 10 #t #'a " | " #'b))
      ((bitwise-ior a b cs ...)
        (syntax->expr $lookup #`(bitwise-ior (bitwise-ior a b) cs ...)))
      ((bitwise-xor)
        (syntax->expr $lookup #'0))
      ((bitwise-xor a)
        (syntax->expr $lookup #'a))
      ((bitwise-xor a b)
        (op2->expr $lookup 9 #t #'a " ^ " #'b))
      ((bitwise-xor a b cs ...)
        (syntax->expr $lookup #`(bitwise-xor (bitwise-xor a b) cs ...)))
      ((shl a b)
        (op2->expr $lookup 5 #t #'a " << " #'b))
      ((shr a b)
        (op2->expr $lookup 5 #t #'a " >> " #'b))
      ((inv a)
        (op1->expr $lookup 2 #f "~" #'a))
      ((not a)
        (op1->expr $lookup 2 #f "!" #'a))
      ((ref var x ...)
        (ref->expr $lookup #'(var x ...)))
      ((&ref var x ...)
        (expr 2 #f (code "&" (expr-code (ref->expr $lookup #'(var x ...))))))
      ((id arg ...)
        (and (identifier? #'id) ($lookup #'id #'micac))
        (syntax->expr $lookup
          (($lookup #'id #'micac) $syntax)))
      ((id arg ...)
        (identifier? #'id)
        (paren-expr 1 #t
          (variable->expr #'id)
          "("
          (expr 0 #t
            (apply code-append
              (intercalate
                (map expr-code
                  (map
                    (partial syntax->expr $lookup)
                    (syntax->list #`(arg ...))))
                (code ", "))))
          ")"))
      (other
        (value->expr #'other))))

  (define (ref->expr $lookup $syntax)
    (syntax-case $syntax (*)
      ((var x ...)
        (fold-left
          (lambda ($expr $x)
            (syntax-case $x (*)
              (*
                (prefix-expr 2 #f "*" $expr))
              (id (identifier? #'id)
                (binary-expr 1 #t $expr "." (variable->expr #'id)))
              (expr
                (paren-expr 1 #t $expr "[" (syntax->expr $lookup #'expr) "]"))))
          (variable->expr #'var)
          (syntax->list #'(x ...))))))

  (define (op1->expr $lookup $priority $left-to-right? $op $rhs)
    (prefix-expr $priority $left-to-right? $op
      (syntax->expr $lookup $rhs)))

  (define (op2->expr $lookup $priority $left-to-right? $lhs $op $rhs)
    (binary-expr $priority $left-to-right?
      (syntax->expr $lookup $lhs)
      $op
      (syntax->expr $lookup $rhs)))

  (define (paren->expr $lookup $priority $left-to-right? $lparen $expr $rparen)
    (paren-expr $priority $left-to-right?
      $lparen
      (syntax->expr $lookup $expr)
      $rparen))

  (define (syntax-c $lookup . $syntaxes)
    (code-string
      (code+instrs $lookup empty-code
        #`(#,@$syntaxes))))
)
