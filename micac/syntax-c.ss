(library (micac syntax-c)
  (export syntax-c)
  (import
    (micascheme)
    (code)
    (micac expr)
    (micac syntax)
    (micac scope)
    (micac compiled)
    (micac env))

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

  (define (declarator->code $env $syntax)
    (syntax-case $syntax (*)
      (id (identifier? #'id)
        (type->code #'id))
      ((* decl)
        (code "*" (declarator->code $env #'decl)))
      ((* decl expr)
        (code
          (declarator->code $env #'decl)
          (code-in-square-brackets
            (expr-code (syntax->expr $env #'expr)))))))

  (define (identifier->code $identifier)
    (string-code
      (list->string
        (map-with
          ($char
            (string->list
              (symbol->string
                (syntax->datum $identifier))))
          (case $char
            ((#\- #\?) #\_)
            (else $char))))))

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

  (define (lhs->expr $env $syntax)
    (syntax-case $syntax ()
      (id (identifier? #'id)
        (variable->expr #'id))
      (other
        (ref->expr $env #'other))))

  (define (value->expr $value)
    (syntax-case $value ()
      (id
        (identifier? #'id)
        (variable->expr #'id))
      (const
        (literal->expr #'const))))

  (define (compiled-code+instrs $compiled $syntax)
    (syntax-case $syntax (defer break-if)
      (() $compiled)
      (((defer deferred ...) body ...)
        (compiled-code+instrs
          (compiled-code+instrs $compiled #'(body ...))
          #'(deferred ...)))
      (((break-if expr break-body ...) body ...)
        (compiled-code+instr $compiled
          #`(if expr
            (then break-body ...)
            (else body ...))))
      (((id arg ...) body ...)
        (and (identifier? #'id) (compiled-ref $compiled #'id))
        (compiled-code+instrs $compiled
          #`(
            #,@(begin-syntaxes
              (compiled-transform $compiled #'id #'(id arg ...)))
            body ...)))
      ((other body ...)
        (compiled-code+instrs
          (compiled-code+instr $compiled #'other)
          #'(body ...)))))

  (define (instr->begin $instr)
    (syntax-case $instr (begin)
      ((begin body ...) $instr)
      (other #'(begin other))))

  (define (op2->string-opt $op)
    (syntax-case $op
      (set
        set+ set- set* set/
        set-and set-or
        set-bitwise-and set-bitwise-ior set-bitwise-xor
        set-bitwise-arithmetic-shift-left set-bitwise-arithmetic-shift-right)
      (set "=")
      (set+ "+=")
      (set- "-=")
      (set* "*=")
      (set/ "/=")
      (set-and "&&=")
      (set-or "||=")
      (set-bitwise-and "&=")
      (set-bitwise-ior "|=")
      (set-bitwise-xor "^=")
      (set-bitwise-arithmetic-shift-left "<<=")
      (set-bitwise-arithmetic-shift-right ">>=")
      (_ #f)))

  (define (compiled-code+instr $compiled $syntax)
    (syntax-case $syntax (macro begin var const if when while then else)
      ((macro (id param ...) body ...)
        (compiled
          (env+
            (compiled-env $compiled)
            (identifier id)
            (lambda ($syntax)
              (lambda ($lookup)
                (syntax-case $syntax ()
                  ((_ arg ...)
                    (syntax-subst
                      #'(param ...)
                      #'(arg ...)
                      #'(begin body ...)))))))
          (compiled-value $compiled)))
      ((begin instr ...)
        (compiled-map
          ($code $compiled)
          (code $code
            (code-in-curly-brackets
              (code-indent
                (code "\n"
                  (compiled-value
                    (compiled-code+instrs
                      (compiled-with $compiled empty-code)
                      #'(instr ...))))))
            "\n")))
      ((var type id)
        (compiled-map
          ($code $compiled)
          (code $code
            (space-separated-code
              (type->code #'type)
              (declarator->code (compiled-env $compiled) #'id))
            ";\n")))
      ((var type id expr)
        (compiled-map
          ($code $compiled)
          (code $code
            (space-separated-code
              (type->code #'type)
              (declarator->code (compiled-env $compiled) #'id)
              "="
              (expr-code (syntax->expr (compiled-env $compiled) #'expr)))
            ";\n")))
      ((const type id expr)
        (compiled-map
          ($code $compiled)
          (code $code
            (space-separated-code
              "const"
              (type->code #'type)
              (declarator->code (compiled-env $compiled) #'id)
              "="
              (expr-code (syntax->expr (compiled-env $compiled) #'expr)))
            ";\n")))
      ((if expr (then then-body ...) (else else-body ...))
        (compiled-map
          ($code $compiled)
          ($then-code
            (compiled-code+instr (compiled-with $compiled empty-code)
              (instr->begin #'(begin then-body ...))))
          ($else-code
            (compiled-code+instr (compiled-with $compiled empty-code)
              (instr->begin #'(begin else-body ...))))
          (code $code
            (space-separated-code
              "if"
              (code-in-round-brackets
                (expr-code (syntax->expr (compiled-env $compiled) #'expr)))
              $then-code)
            (space-separated-code
              "else"
              $else-code))))
      ((when expr body ...)
        (compiled-map
          ($code $compiled)
          ($when-code
            (compiled-code+instr
              (compiled-with $compiled empty-code)
              (instr->begin #'(begin body ...))))
          (code $code
            (space-separated-code
              "if"
              (code-in-round-brackets
                (expr-code (syntax->expr (compiled-env $compiled) #'expr)))
              $when-code))))
      ((while expr instr ...)
        (compiled-map
          ($code $compiled)
          ($while-code
            (compiled-code+instr (compiled-with $compiled empty-code)
              #'(begin instr ...)))
          (code $code
            (space-separated-code
              "while"
              (code-in-round-brackets
                (expr-code (syntax->expr (compiled-env $compiled) #'expr)))
              $while-code))))
      ((op2 lhs expr)
        (op2->string-opt #'op2)
        (compiled-map
          ($code $compiled)
          (code+op2 (compiled-env $compiled) $code
            #'lhs (op2->string-opt #'op2) #'expr)))
      ((id arg ...)
        (lets
          ($transformer (compiled-ref $compiled (identifier id)))
          (if $transformer
            (compiled-code+instrs $compiled
              #`(
                #,@(begin-syntaxes
                  (compiled-transform $compiled #'id $syntax))))
            (compiled-map
              ($code $compiled)
              (code $code
                (expr-code
                  (parenthesized-expr 1 #t
                    (variable->expr #'id)
                    "("
                    (expr 0 #t
                      (apply code-append
                        (intercalate
                          (map expr-code
                            (map
                              (partial syntax->expr (compiled-env $compiled))
                              (syntaxes arg ...)))
                          (code ", "))))
                    ")"))
                ";\n")))))))

  (define (code+op2 $env $code $lhs $op $expr)
    (code $code
      (space-separated-code
        (expr-code (lhs->expr $env $lhs))
        (string-code $op)
        (expr-code (syntax->expr $env $expr)))
      ";\n"))

  (define (syntax->expr $env $syntax)
    (syntax-case $syntax
      (cast = > >= < <= + - * /
        and or bitwise-and bitwise-ior bitwise-xor
        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        neg not inv ref &ref ?)
      ((cast type rhs)
        (expr 2 #f
          (code
            (code-in-round-brackets (type->code #'type))
            (expr-operand-code (syntax->expr $env #'rhs) 2 #t))))
      ((= a b)
        (op2->expr $env 7 #t #'a " == " #'b))
      ((not (= a b))
        (op2->expr $env 7 #t #'a " != " #'b))
      ((> a b)
        (op2->expr $env 6 #t #'a " > " #'b))
      ((>= a b)
        (op2->expr $env 6 #t #'a " >= " #'b))
      ((< a b)
        (op2->expr $env 6 #t #'a " < " #'b))
      ((<= a b)
        (op2->expr $env 6 #t #'a " <= " #'b))
      ((+)
        (syntax->expr $env #'0))
      ((+ a)
        (syntax->expr $env #'a))
      ((+ a b)
        (op2->expr $env 4 #t #'a " + " #'b))
      ((+ a b cs ...)
        (syntax->expr $env #`(+ (+ a b) cs ...)))
      ((- a)
        (op1->expr $env 2 #f "-" #'a))
      ((- a b)
        (op2->expr $env 4 #t #'a " - " #'b))
      ((- a b cs ...)
        (syntax->expr $env #`(- (- a b) cs ...)))
      ((*)
        (syntax->expr $env #'1))
      ((* a)
        (syntax->expr $env #'a))
      ((* a b)
        (op2->expr $env 3 #t #'a " * " #'b))
      ((* a b cs ...)
        (syntax->expr $env #`(* (* a b) cs ...)))
      ((/ a)
        (syntax->expr $env #`(/ 1 a)))
      ((/ a b)
        (op2->expr $env 3 #t #'a " / " #'b))
      ((/ a b cs ...)
        (syntax->expr $env #`(/ (/ a b) cs ...)))
      ((and)
        (syntax->expr $env #'true))
      ((and a)
        (syntax->expr $env #'a))
      ((and a b)
        (op2->expr $env 11 #t #'a " && " #'b))
      ((and a b cs ...)
        (syntax->expr $env #`(and (and a b) cs ...)))
      ((or)
        (syntax->expr $env #'false))
      ((or a)
        (syntax->expr $env #'a))
      ((or a b)
        (op2->expr $env 12 #t #'a " || " #'b))
      ((or a b cs ...)
        (syntax->expr $env #`(or (or a b) cs ...)))
      ((bitwise-and)
        (syntax->expr $env #'-1))
      ((bitwise-and a)
        (syntax->expr $env #'a))
      ((bitwise-and a b)
        (op2->expr $env 8 #t #'a " & " #'b))
      ((bitwise-and a b cs ...)
        (syntax->expr $env #`(bitwise-and (bitwise-and a b) cs ...)))
      ((bitwise-ior)
        (syntax->expr $env #'0))
      ((bitwise-ior a)
        (syntax->expr $env #'a))
      ((bitwise-ior a b)
        (op2->expr $env 10 #t #'a " | " #'b))
      ((bitwise-ior a b cs ...)
        (syntax->expr $env #`(bitwise-ior (bitwise-ior a b) cs ...)))
      ((bitwise-xor)
        (syntax->expr $env #'0))
      ((bitwise-xor a)
        (syntax->expr $env #'a))
      ((bitwise-xor a b)
        (op2->expr $env 9 #t #'a " ^ " #'b))
      ((bitwise-xor a b cs ...)
        (syntax->expr $env #`(bitwise-xor (bitwise-xor a b) cs ...)))
      ((bitwise-arithmetic-shift-left a b)
        (op2->expr $env 5 #t #'a " << " #'b))
      ((bitwise-arithmetic-shift-right a b)
        (op2->expr $env 5 #t #'a " >> " #'b))
      ((inv a)
        (op1->expr $env 2 #f "~" #'a))
      ((not a)
        (op1->expr $env 2 #f "!" #'a))
      ((ref var x ...)
        (ref->expr $env #'(var x ...)))
      ((&ref var x ...)
        (expr 2 #f (code "&" (expr-code (ref->expr $env #'(var x ...))))))
      ((? cond true false)
        (expr 13 #f
          (code
            (expr-operand-code (syntax->expr $env #'cond) 13 #f)
            " ? "
            (expr-code (syntax->expr $env #'true))
            " : "
            (expr-operand-code (syntax->expr $env #'false) 13 #t))))
      ((id arg ...)
        (lets
          ($transformer (env-ref $env (identifier id)))
          (if $transformer
            (syntax->expr $env
              (env-transform $env (identifier id) $syntax))
            (parenthesized-expr 1 #t
              (variable->expr #'id)
              "("
              (expr 0 #t
                (apply code-append
                  (intercalate
                    (map expr-code
                      (map
                        (partial syntax->expr $env)
                        (syntaxes arg ...)))
                    (code ", "))))
              ")"))))
      (other
        (value->expr #'other))))

  (define (ref->expr $env $syntax)
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
                (parenthesized-expr 1 #t $expr "[" (syntax->expr $env #'expr) "]"))))
          (variable->expr #'var)
          (syntaxes x ...)))))

  (define (op1->expr $env $priority $left-to-right? $op $rhs)
    (prefix-expr $priority $left-to-right? $op
      (syntax->expr $env $rhs)))

  (define (op2->expr $env $priority $left-to-right? $lhs $op $rhs)
    (binary-expr $priority $left-to-right?
      (syntax->expr $env $lhs)
      $op
      (syntax->expr $env $rhs)))

  (define (syntax-c $lookup . $syntaxes)
    (code-string
      (compiled-value
        (compiled-code+instrs
          (compiled (env $lookup (scope)) empty-code)
          #`(#,@$syntaxes)))))
)
