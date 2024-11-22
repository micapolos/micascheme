(library (micac syntax-c)
  (export syntax-c)
  (import
    (micascheme)
    (code)
    (micac expr)
    (micac code)
    (micac syntax)
    (micac expand))

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
    (syntax-case $type (*)
      (id (identifier? #'id)
        (identifier->code #'id))
      ((* type)
        (code (type->code #'type) "*"))
      (_
        (syntax-error $type "unknown type"))))

  (define (declarator->code $syntax)
    (syntax-case $syntax (*)
      (id (identifier? #'id)
        (identifier->code #'id))
      ((* decl)
        (code "*" (declarator->code #'decl)))
      ((* decl expr)
        (code
          (declarator->code #'decl)
          (code-in-square-brackets
            (expr-code (syntax->expr #'expr)))))))

  (define (literal->expr $literal)
    (switch (syntax->datum $literal)
      ((number? $fixnum)
        (expr 0 #t (string-code (number->string $fixnum))))
      ((string? $string)
        (expr 0 #t (code "\"" (string-code $string) "\"")))
      ((boolean? $boolean)
        (expr 0 #t (string-code (if $boolean "true" "false"))))
      ((else $other)
        (syntax-error $literal "not literal"))))

  (define (variable->expr $variable)
    (expr 0 #t (identifier->code $variable)))

  (define (lhs->expr $syntax)
    (syntax-case $syntax ()
      (id (identifier? #'id)
        (identifier->expr #'id))
      (other
        (ref->expr #'other))))

  (define (value->expr $value)
    (syntax-case $value ()
      (id (identifier? #'id)
        (identifier->expr #'id))
      (other
        (literal->expr #'other))))

  (define (instrs-code $top-level? $instrs)
    (syntax-case $instrs ()
      (() (code ""))
      ((instr ... last)
        (code
          (apply code-append
            (map (partial instr-code #f)
              (syntax->list #'(instr ...))))
          (instr-code #t #'last)))))

  (define (instr->begin $instr)
    (syntax-case $instr (begin)
      ((begin body ...) $instr)
      (other #'(begin other))))

  (define (block-code $instrs)
    (code-in-curly-brackets
      (code-indent
        (code "\n"
          (instrs-code #f $instrs)))))

  (define (instr-code $allow-return? $instr)
    (syntax-case $instr
      (
        begin var const if when cond while then else set
        + - * div and or bitwise-and bitwise-ior bitwise-xor
        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        return)
      ((begin instr ...)
        (newline-ended-code (block-code (syntaxes instr ...))))
      ((var type decl)
        (newline-ended-code
          (colon-ended-code
            (space-separated-code
              (type->code #'type)
              (declarator->code #'decl)))))
      ((var type decl expr)
        (newline-ended-code
          (colon-ended-code
            (space-separated-code
              (type->code #'type)
              (declarator->code #'decl)
              "="
              (expr-code (syntax->expr #'expr))))))
      ((const type decl expr)
        (newline-ended-code
          (colon-ended-code
            (space-separated-code
              "const"
              (type->code #'type)
              (declarator->code #'decl)
              "="
              (expr-code (syntax->expr #'expr))))))
      ((if expr (then then-body ...) (else else-body ...))
        (newline-ended-code
          (space-separated-code
            "if"
            (code-in-round-brackets
              (expr-code (syntax->expr #'expr)))
            (block-code #'(then-body ...))
          (space-separated-code
            "else"
            (block-code #'(else-body ...))))))
      ((when expr body ...)
        (newline-ended-code
          (space-separated-code
            "if"
            (code-in-round-brackets
              (expr-code (syntax->expr #'expr)))
            (block-code #'(body ...)))))
      ; TODO: Move "cond" to expanded, and expand to "if"
      ((cond clause clause* ... (else else-instr ...))
        (newline-ended-code
          (list->code
            (intercalate
              (append
                (map clause-code (syntaxes clause clause* ...))
                (list (block-code (syntaxes else-instr ...))))
              (code " else ")))))
      ; TODO: Move "cond" to expanded, and expand to "when"
      ((cond clause clause* ...)
        (newline-ended-code
          (list->code
            (intercalate
              (map clause-code (syntaxes clause clause* ...))
              (code " else ")))))
      ((while expr body ...)
        (newline-ended-code
          (space-separated-code
            "while"
            (code-in-round-brackets
              (expr-code (syntax->expr #'expr)))
            (block-code #'(body ...)))))
      ((set lhs expr)
        (op2-code #'lhs "=" #'expr))
      ((set lhs + expr)
        (op2-code #'lhs "+=" #'expr))
      ((set lhs - expr)
        (op2-code #'lhs "-=" #'expr))
      ((set lhs * expr)
        (op2-code #'lhs "*=" #'expr))
      ((set lhs div expr)
        (op2-code #'lhs "/=" #'expr))
      ((set lhs and expr)
        (op2-code #'lhs "&&=" #'expr))
      ((set lhs or expr)
        (op2-code #'lhs "||=" #'expr))
      ((set lhs bitwise-and expr)
        (op2-code #'lhs "&=" #'expr))
      ((set lhs bitwise-ior expr)
        (op2-code #'lhs "|=" #'expr))
      ((set lhs bitwise-xor expr)
        (op2-code #'lhs "^=" #'expr))
      ((set lhs bitwise-arithmetic-shift-left expr)
        (op2-code #'lhs "<<=" #'expr))
      ((set lhs bitwise-arithmetic-shift-right expr)
        (op2-code #'lhs ">>=" #'expr))
      ((return expr)
        (if $allow-return?
          (newline-ended-code
            (colon-ended-code
              (space-separated-code
                "return"
                (expr-code (syntax->expr #'expr)))))
          (syntax-error #'return "not allowed")))
      ((id arg ...)
        (newline-ended-code
          (colon-ended-code
            (expr-code
              (parenthesized-expr 1 #t
                (identifier->expr #'id)
                "("
                (expr 0 #t
                  (apply code-append
                    (intercalate
                      (map expr-code
                        (map syntax->expr (syntaxes arg ...)))
                      (code ", "))))
                ")")))))))

  (define (clause-code $clause)
    (syntax-case $clause ()
      ((expr instr ...)
        (space-separated-code
          "if"
          (code-in-round-brackets
            (expr-code (syntax->expr #'expr)))
          (block-code #'(instr ...))))))

  (define (op2-code $lhs $op $expr)
    (newline-ended-code
      (colon-ended-code
        (space-separated-code
          (expr-code (lhs->expr $lhs))
          (string-code $op)
          (expr-code (syntax->expr $expr))))))

  (define (syntax->expr $syntax)
    (syntax-case $syntax
      (cast = > >= < <= + - * div
        not and or
        bitwise-and bitwise-ior bitwise-xor bitwise-not
        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        if
        ref &ref ?)
      ((cast type rhs)
        (expr 2 #f
          (code
            (code-in-round-brackets (type->code #'type))
            (expr-operand-code (syntax->expr #'rhs) 2 #t))))
      ((not (= a b))
        (op2->expr 7 #t #'a " != " #'b))
      ((= a b)
        (op2->expr 7 #t #'a " == " #'b))
      ((> a b)
        (op2->expr 6 #t #'a " > " #'b))
      ((>= a b)
        (op2->expr 6 #t #'a " >= " #'b))
      ((< a b)
        (op2->expr 6 #t #'a " < " #'b))
      ((<= a b)
        (op2->expr 6 #t #'a " <= " #'b))
      ((+ a b)
        (op2->expr 4 #t #'a " + " #'b))
      ((- a)
        (op1->expr 2 #f "-" #'a))
      ((- a b)
        (op2->expr 4 #t #'a " - " #'b))
      ((* a b)
        (op2->expr 3 #t #'a " * " #'b))
      ((div a b)
        (op2->expr 3 #t #'a " / " #'b))
      ((not a)
        (op1->expr 2 #f "!" #'a))
      ((and a b)
        (op2->expr 11 #t #'a " && " #'b))
      ((or a b)
        (op2->expr 12 #t #'a " || " #'b))
      ((bitwise-not a)
        (op1->expr 2 #f "~" #'a))
      ((bitwise-and a b)
        (op2->expr 8 #t #'a " & " #'b))
      ((bitwise-ior a b)
        (op2->expr 10 #t #'a " | " #'b))
      ((bitwise-xor a b)
        (op2->expr 9 #t #'a " ^ " #'b))
      ((bitwise-arithmetic-shift-left a b)
        (op2->expr 5 #t #'a " << " #'b))
      ((bitwise-arithmetic-shift-right a b)
        (op2->expr 5 #t #'a " >> " #'b))
      ((ref var x ...)
        (ref->expr #'(var x ...)))
      ((&ref var x ...)
        (expr 2 #f (code "&" (expr-code (ref->expr #'(var x ...))))))
      ((if cond true false)
        (expr 13 #f
          (code
            (expr-operand-code (syntax->expr #'cond) 13 #f)
            " ? "
            (expr-code (syntax->expr #'true))
            " : "
            (expr-operand-code (syntax->expr #'false) 13 #t))))
      ((id arg ...)
        (parenthesized-expr 1 #t
          (identifier->expr #'id)
          "("
          (expr 0 #t
            (apply code-append
              (intercalate
                (map expr-code
                  (map syntax->expr
                    (syntaxes arg ...)))
                (code ", "))))
          ")"))
      (other
        (value->expr #'other))))

  (define (ref->expr $syntax)
    (syntax-case $syntax (*)
      ((var x ...)
        (fold-left
          (lambda ($expr $x)
            (syntax-case $x (*)
              (*
                (prefix-expr 2 #f "*" $expr))
              (id (identifier? #'id)
                (binary-expr 1 #t $expr "." (variable->expr #'id)))
              ((expr)
                (parenthesized-expr 1 #t $expr "[" (syntax->expr #'expr) "]"))))
          (identifier->expr #'var)
          (syntaxes x ...)))))

  (define (op1->expr $priority $left-to-right? $op $rhs)
    (prefix-expr $priority $left-to-right? $op
      (syntax->expr $rhs)))

  (define (op2->expr $priority $left-to-right? $lhs $op $rhs)
    (binary-expr $priority $left-to-right?
      (syntax->expr $lhs)
      $op
      (syntax->expr $rhs)))

  (define (syntax-c $syntax)
    (syntax-case $syntax ()
      ((body ...)
        (code-string
          (instrs-code #t #'(body ...))))))
)
