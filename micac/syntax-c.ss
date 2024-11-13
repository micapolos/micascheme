(library (micac syntax-c)
  (export syntax-c)
  (import
    (micascheme)
    (code)
    (micac expr)
    (micac syntax)
    (micac scope)
    (micac compiled)
    (micac env)
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

  (define (declarator->compiled-code $env $syntax)
    (syntax-case $syntax (*)
      (id (identifier? #'id)
        (compiled-map
          ($expr (compiled-alloc $env #'id))
          (expr-code $expr)))
      ((* decl)
        (compiled-map
          ($code (declarator->compiled-code $env #'decl))
          (code "*" $code)))
      ((* decl expr)
        (compiled-map
          ($code (declarator->compiled-code $env #'decl))
          (code $code
            (code-in-square-brackets
              (expr-code (syntax->expand-expr $env #'expr))))))))

  (define (identifier->code $identifier)
    (string-code
      (list->string
        (map-with
          ($char (fluent $identifier (syntax->datum) (symbol->string) (string->list)))
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
        (variable-identifier->expr $env #'id))
      (other
        (ref->expr $env #'other))))

  (define (variable-identifier->expr $env $identifier)
    (switch (env-ref $env $identifier)
      ((expr? $expr)
        $expr)
      ((else _)
        (syntax-error $identifier "unboud identifier"))))

  (define (env-identifier->expr $env $identifier)
    (switch (env-ref $env $identifier)
      ((expr? $expr)
        $expr)
      ((else $transformer)
        (syntax->expand-expr $env
          (env-transform $env $transformer $identifier)))))

  (define (value->expr $env $value)
    (syntax-case $value ()
      (id (identifier? #'id)
        (env-identifier->expr $env #'id))
      (other
        (literal->expr #'other))))

  ; TODO: Separate "expand" and "compile"
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
      (((id arg ...) body ...) (identifier? #'id)
        (switch (compiled-transformer $compiled #'id)
          ((false? _)
            (compiled-code+instrs
              (compiled-code+instr $compiled #'(id arg ...))
              #'(body ...)))
          ((else $transformer)
            (compiled-code+instrs $compiled
              #`(
                #,@(begin-syntaxes
                  (compiled-transform $compiled $transformer #'(id arg ...)))
                body ...)))))
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

  (define (block-code $env $syntaxes)
    (code-in-curly-brackets
      (code-indent
        (code "\n"
          (compiled-value
            (compiled-code+instrs
              (compiled $env empty-code)
              $syntaxes))))))

  ; TODO: Separate "expand" and "compile"
  (define (compiled-code+instr $compiled $syntax)
    (lets
      ((compiled $env $code) $compiled)
      (syntax-case $syntax (extern macro begin var const if when while then else)
        ((extern id)
          (compiled+ $compiled
            (identifier id)
            (identifier->expr (identifier id))))
        ((macro (id param ...) body ...)
          (compiled+ $compiled
            (identifier id)
            (lambda ($syntax)
              (lambda ($lookup)
                (syntax-case $syntax ()
                  ((_ arg ...)
                    (syntax-subst
                      #'(param ...)
                      #'(arg ...)
                      #'(begin body ...))))))))
        ((macro id expr)
          (compiled+ $compiled
            (identifier id)
            (lambda ($syntax)
              (lambda ($lookup)
                #'expr))))
        ((begin instr ...)
          (compiled-with $compiled
            (code $code
              (block-code $env (syntaxes instr ...))
              "\n")))
        ((var type id)
          (compiled-map
            ($declarator-code (declarator->compiled-code $env #'id))
              (code $code
                (space-separated-code
                  (type->code #'type)
                  $declarator-code)
                ";\n")))
        ((var type id expr)
          (compiled-map
            ($declarator-code (declarator->compiled-code $env #'id))
            (code $code
              (space-separated-code
                (type->code #'type)
                $declarator-code
                "="
                (expr-code (syntax->expand-expr $env #'expr)))
              ";\n")))
        ((const type id expr)
          (compiled-map
            ($declarator-code (declarator->compiled-code $env #'id))
            (code $code
              (space-separated-code
                "const"
                (type->code #'type)
                $declarator-code
                "="
                (expr-code (syntax->expand-expr $env #'expr)))
              ";\n")))
        ((if expr (then then-body ...) (else else-body ...))
          (compiled-with $compiled
            (code $code
              (space-separated-code
                "if"
                (code-in-round-brackets
                  (expr-code (syntax->expand-expr $env #'expr)))
                (block-code $env (syntaxes then-body ...))
              (space-separated-code
                "else"
                (block-code $env (syntaxes else-body ...))))
              "\n")))
        ((when expr body ...)
          (compiled-with $compiled
            (code $code
              (space-separated-code
                "if"
                (code-in-round-brackets
                  (expr-code (syntax->expand-expr $env #'expr)))
                (block-code $env (syntaxes body ...)))
              "\n")))
        ((while expr body ...)
          (compiled-with $compiled
            (code $code
              (space-separated-code
                "while"
                (code-in-round-brackets
                  (expr-code (syntax->expand-expr $env #'expr)))
                (block-code $env (syntaxes body ...)))
              "\n")))
        ((op2 lhs expr)
          (op2->string-opt #'op2)
          (compiled-with $compiled
            (code+op2 $env $code
              #'lhs (op2->string-opt #'op2) #'expr)))
        ((id arg ...)
          (switch (compiled-ref $compiled (identifier id))
            ((expr? $expr)
              (compiled-with $compiled
                (code $code
                  (expr-code
                    (parenthesized-expr 1 #t
                      $expr
                      "("
                      (expr 0 #t
                        (apply code-append
                          (intercalate
                            (map expr-code
                              (map
                                (partial syntax->expand-expr $env)
                                (syntaxes arg ...)))
                            (code ", "))))
                      ")"))
                  ";\n")))
            ((else $transformer)
              (compiled-code+instrs $compiled
                #`(
                  #,@(begin-syntaxes
                    (compiled-transform $compiled $transformer $syntax))))))))))

  (define (code+op2 $env $code $lhs $op $expr)
    (code $code
      (space-separated-code
        (expr-code (lhs->expr $env $lhs))
        (string-code $op)
        (expr-code (syntax->expand-expr $env $expr)))
      ";\n"))

  (define (syntax->expand-expr $env $syntax)
    (syntax->expr $env (expand-expr (env->lookup $env) $syntax)))

  (define (syntax->expr $env $syntax)
    (syntax-case $syntax
      (cast = > >= < <= + - * div
        not and or
        bitwise-and bitwise-ior bitwise-xor bitwise-not
        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        ref &ref ?)
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
      ((+ a b)
        (op2->expr $env 4 #t #'a " + " #'b))
      ((- a)
        (op1->expr $env 2 #f "-" #'a))
      ((- a b)
        (op2->expr $env 4 #t #'a " - " #'b))
      ((* a b)
        (op2->expr $env 3 #t #'a " * " #'b))
      ((div a b)
        (op2->expr $env 3 #t #'a " / " #'b))
      ((not a)
        (op1->expr $env 2 #f "!" #'a))
      ((and a b)
        (op2->expr $env 11 #t #'a " && " #'b))
      ((or a b)
        (op2->expr $env 12 #t #'a " || " #'b))
      ((bitwise-not a)
        (op1->expr $env 2 #f "~" #'a))
      ((bitwise-and a b)
        (op2->expr $env 8 #t #'a " & " #'b))
      ((bitwise-ior a b)
        (op2->expr $env 10 #t #'a " | " #'b))
      ((bitwise-xor a b)
        (op2->expr $env 9 #t #'a " ^ " #'b))
      ((bitwise-arithmetic-shift-left a b)
        (op2->expr $env 5 #t #'a " << " #'b))
      ((bitwise-arithmetic-shift-right a b)
        (op2->expr $env 5 #t #'a " >> " #'b))
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
        (switch (env-ref $env (identifier id))
          ((expr? $expr)
            (parenthesized-expr 1 #t
              $expr
              "("
              (expr 0 #t
                (apply code-append
                  (intercalate
                    (map expr-code
                      (map
                        (partial syntax->expr $env)
                        (syntaxes arg ...)))
                    (code ", "))))
              ")"))
          ((else $transformer)
            (syntax-error #'id "macro identifier"))))
      (other
        (value->expr $env #'other))))

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
          (variable-identifier->expr $env #'var)
          (syntaxes x ...)))))

  (define (op1->expr $env $priority $left-to-right? $op $rhs)
    (prefix-expr $priority $left-to-right? $op
      (syntax->expr $env $rhs)))

  (define (op2->expr $env $priority $left-to-right? $lhs $op $rhs)
    (binary-expr $priority $left-to-right?
      (syntax->expr $env $lhs)
      $op
      (syntax->expr $env $rhs)))

  (define (syntax-c $env . $syntaxes)
    (code-string
      (compiled-value
        (compiled-code+instrs
          (compiled $env empty-code)
          #`(#,@$syntaxes)))))
)
