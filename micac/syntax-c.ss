(library (micac syntax-c)
  (export syntax-c)
  (import
    (micascheme)
    (code)
    (micac expr)
    (micac syntax))

  (define scope list)

  (define (scope+ $scope $id $transformer)
    (push $scope (cons $id $transformer)))

  (define (scope-ref $scope $id)
    (lets
      ($ass (assid $id $scope))
      (and $ass (cdr $ass))))

  (data (compiled scope value))

  (define (lookup-scope-ref $lookup $scope $id)
    (or
      (scope-ref $scope $id)
      ($lookup $id)))

  (define (compiled-ref $lookup $compiled $id)
    (lookup-scope-ref $lookup (compiled-scope $compiled) $id))

  (define (lookup-scope-transform $lookup $scope $id $syntax)
    (lets
      ($transformer (lookup-scope-ref $lookup $scope $id))
      (if $transformer
        (transform $transformer $syntax $lookup)
        (syntax-error $id "no macro"))))

  (define (compiled-transform $lookup $compiled $id $syntax)
    (lets
      ($transformer (compiled-ref $lookup $compiled $id))
      (if $transformer
        (transform $transformer $syntax $lookup)
        (syntax-error $id "no macro"))))

  (define-rule-syntax (pure-compiled value)
    (compiled (scope) value))

  (define-rule-syntax (compiled-with compiled-expr value)
    (compiled (compiled-scope compiled-expr) value))

  (define-rules-syntax
    ((compiled-map body) (pure-compiled body))
    ((compiled-map (value compiled-expr) decl ... body)
      (compiled-map decl ...
        (compiled-value
          (lets
            ($compiled compiled-expr)
            (value (compiled-value $compiled))
            ($body-value body)
            (compiled (compiled-scope $compiled) $body-value))))))

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

  (define (declarator->code $lookup $syntax)
    (syntax-case $syntax (*)
      (id (identifier? #'id)
        (type->code #'id))
      ((* decl)
        (code "*" (declarator->code $lookup #'decl)))
      ((* decl expr)
        (code
          (declarator->code $lookup #'decl)
          (code-in-square-brackets
            (expr-code (syntax->expr $lookup #'expr)))))))

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

  (define (compiled-code+instrs $lookup $compiled $syntax)
    (syntax-case $syntax (defer break-if)
      (() $compiled)
      (((defer deferred ...) body ...)
        (compiled-code+instrs $lookup
          (compiled-code+instrs $lookup $compiled #'(body ...))
          #'(deferred ...)))
      (((break-if expr break-body ...) body ...)
        (compiled-code+instr $lookup $compiled
          #`(if expr
            (then break-body ...)
            (else body ...))))
      (((id arg ...) body ...)
        (and (identifier? #'id) (compiled-ref $lookup $compiled #'id))
        (compiled-code+instrs $lookup $compiled
          #`(
            #,@(begin-syntaxes
              (compiled-transform $lookup $compiled #'id #'(id arg ...)))
            body ...)))
      ((other body ...)
        (compiled-code+instrs $lookup
          (compiled-code+instr $lookup $compiled #'other)
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

  (define (compiled-code+instr $lookup $compiled $syntax)
    (syntax-case $syntax (macro begin var const if when while then else)
      ((macro (id param ...) body ...)
        (compiled
          (scope+
            (compiled-scope $compiled)
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
          ($begin-code (compiled-code+instrs $lookup (compiled-with $compiled empty-code) #'(instr ...)))
          (code $code
            (code-in-curly-brackets (code-indent (code "\n" $begin-code)))
            "\n")))
      ((var type id)
        (compiled-map
          ($code $compiled)
          (code $code
            (space-separated-code
              (type->code #'type)
              (declarator->code $lookup #'id))
            ";\n")))
      ((var type id expr)
        (compiled-map
          ($code $compiled)
          (code $code
            (space-separated-code
              (type->code #'type)
              (declarator->code $lookup #'id)
              "="
              (expr-code (syntax->expr $lookup #'expr)))
            ";\n")))
      ((const type id expr)
        (compiled-map
          ($code $compiled)
          (code $code
            (space-separated-code
              "const"
              (type->code #'type)
              (declarator->code $lookup #'id)
              "="
              (expr-code (syntax->expr $lookup #'expr)))
            ";\n")))
      ((if expr (then then-body ...) (else else-body ...))
        (compiled-map
          ($code $compiled)
          ($then-code
            (compiled-code+instr $lookup (compiled-with $compiled empty-code)
              (instr->begin #'(begin then-body ...))))
          ($else-code
            (compiled-code+instr $lookup (compiled-with $compiled empty-code)
              (instr->begin #'(begin else-body ...))))
          (code $code
            (space-separated-code
              "if"
              (code-in-round-brackets
                (expr-code (syntax->expr $lookup #'expr)))
              $then-code)
            (space-separated-code
              "else"
              $else-code))))
      ((when expr body ...)
        (compiled-map
          ($code $compiled)
          ($when-code
            (compiled-code+instr $lookup
              (compiled-with $compiled empty-code)
              (instr->begin #'(begin body ...))))
          (code $code
            (space-separated-code
              "if"
              (code-in-round-brackets
                (expr-code (syntax->expr $lookup #'expr)))
              $when-code))))
      ((while expr instr ...)
        (compiled-map
          ($code $compiled)
          ($while-code
            (compiled-code+instr $lookup (compiled-with $compiled empty-code)
              #'(begin instr ...)))
          (code $code
            (space-separated-code
              "while"
              (code-in-round-brackets
                (expr-code (syntax->expr $lookup #'expr)))
              $while-code))))
      ((op2 lhs expr)
        (op2->string-opt #'op2)
        (compiled-map
          ($code $compiled)
          (code+op2 $lookup $code
            #'lhs (op2->string-opt #'op2) #'expr)))
      ((id arg ...)
        (and (identifier? #'id) (compiled-ref $lookup $compiled #'id))
        (compiled-code+instrs $lookup $compiled
          #`(
            #,@(begin-syntaxes
              (compiled-transform $lookup $compiled #'id $syntax)))))
      ((id arg ...)
        (identifier? #'id)
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
                          (partial syntax->expr $lookup)
                          (syntaxes arg ...)))
                      (code ", "))))
                ")"))
            ";\n")))))

  (define (code+op2 $lookup $code $lhs $op $expr)
    (code $code
      (space-separated-code
        (expr-code (lhs->expr $lookup $lhs))
        (string-code $op)
        (expr-code (syntax->expr $lookup $expr)))
      ";\n"))

  (define (syntax->expr $lookup $syntax)
    (syntax-case $syntax
      (cast = > >= < <= + - * /
        and or bitwise-and bitwise-ior bitwise-xor
        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        neg not inv ref &ref ?)
      ((cast type rhs)
        (expr 2 #f
          (code
            (code-in-round-brackets (type->code #'type))
            (expr-operand-code (syntax->expr $lookup #'rhs) 2 #t))))
      ((= a b)
        (op2->expr $lookup 7 #t #'a " == " #'b))
      ((not (= a b))
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
      ((/ a)
        (syntax->expr $lookup #`(/ 1 a)))
      ((/ a b)
        (op2->expr $lookup 3 #t #'a " / " #'b))
      ((/ a b cs ...)
        (syntax->expr $lookup #`(/ (/ a b) cs ...)))
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
      ((bitwise-arithmetic-shift-left a b)
        (op2->expr $lookup 5 #t #'a " << " #'b))
      ((bitwise-arithmetic-shift-right a b)
        (op2->expr $lookup 5 #t #'a " >> " #'b))
      ((inv a)
        (op1->expr $lookup 2 #f "~" #'a))
      ((not a)
        (op1->expr $lookup 2 #f "!" #'a))
      ((ref var x ...)
        (ref->expr $lookup #'(var x ...)))
      ((&ref var x ...)
        (expr 2 #f (code "&" (expr-code (ref->expr $lookup #'(var x ...))))))
      ((? cond true false)
        (expr 13 #f
          (code
            (expr-operand-code (syntax->expr $lookup #'cond) 13 #f)
            " ? "
            (expr-code (syntax->expr $lookup #'true))
            " : "
            (expr-operand-code (syntax->expr $lookup #'false) 13 #t))))
      ((id arg ...)
        (and (identifier? #'id) ($lookup #'id))
        (syntax->expr $lookup
          (transform
            ($lookup #'id)
            $syntax
            $lookup)))
      ((id arg ...)
        (identifier? #'id)
        (parenthesized-expr 1 #t
          (variable->expr #'id)
          "("
          (expr 0 #t
            (apply code-append
              (intercalate
                (map expr-code
                  (map
                    (partial syntax->expr $lookup)
                    (syntaxes arg ...)))
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
              ((expr)
                (parenthesized-expr 1 #t $expr "[" (syntax->expr $lookup #'expr) "]"))))
          (variable->expr #'var)
          (syntaxes x ...)))))

  (define (op1->expr $lookup $priority $left-to-right? $op $rhs)
    (prefix-expr $priority $left-to-right? $op
      (syntax->expr $lookup $rhs)))

  (define (op2->expr $lookup $priority $left-to-right? $lhs $op $rhs)
    (binary-expr $priority $left-to-right?
      (syntax->expr $lookup $lhs)
      $op
      (syntax->expr $lookup $rhs)))

  (define (syntax-c $lookup . $syntaxes)
    (code-string
      (compiled-value
        (compiled-code+instrs $lookup (pure-compiled empty-code)
          #`(#,@$syntaxes)))))
)
