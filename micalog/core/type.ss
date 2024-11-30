; TODO:
; - check that register is assigned within single domain
(library (micalog core type)
  (export
    literal->typed
    int->typed
    expr->typed
    type->syntax
    lookup-expr->typed
    gen?-lookup-instr->typed-syntax
    gen?-lookup-instrs->typed-syntax
    lookup-instr->typed-syntax
    lookup-instrs->typed-syntax
    lookup-module->typed-syntax
    module->typed-syntax
    lookup+core)
  (import
    (micascheme)
    (syntax lookup)
    (syntax scoped)
    (only (micalog core utils) opposite-edges?)
    (prefix (micalog keywords) %))

  (data (literal-typer fn))
  (data (expr-typer fn))
  (data (instr-typer fn))

  (define (lookup+core $lookup)
    (fluent $lookup
      (lookup+literal int)
      (lookup+expr append)
      (lookup+expr take)
      (lookup+expr drop)
      (lookup+expr = relational-op2)
      (lookup+expr != relational-op2)
      (lookup+expr < relational-op2)
      (lookup+expr <= relational-op2)
      (lookup+expr > relational-op2)
      (lookup+expr >= relational-op2)
      (lookup+expr not op1)
      (lookup+expr and fold-op2)
      (lookup+expr or fold-op2)
      (lookup+expr xor fold-op2)
      (lookup+expr nand fold-op2)
      (lookup+expr nor fold-op2)
      (lookup+expr xnor fold-op2)
      (lookup+expr wrap+ fold-op2)
      (lookup+expr wrap- fold-op1/2)
      (lookup+expr wrap* fold-op2)
      (lookup+expr + additive2)
      (lookup+expr - additive1/2)
      (lookup+expr * multiplicative2)
      (lookup+expr if)
      (lookup+instr input)
      (lookup+instr output)
      (lookup+instr wire)
      (lookup+instr register)
      (lookup+instr set)
      (lookup+instr cond)
      (lookup+instr on)
      (lookup+instr repeat)
      (lookup+instr log)
      (lookup+instr macro)))

  (define (lookup+id-item $lookup $id $item)
    (if (radix->typed? $id)
      (syntax-error $id "can not redefine literal")
      (lookup+undefined $lookup $id $item)))

  (define-syntax (lookup+literal $syntax)
    (syntax-case $syntax ()
      ((_ lookup id)
        #`(lookup+id-item lookup
          #'#,(identifier-append #'id #'% #'id)
          (literal-typer #,(identifier-append #'id #'id #'->typed))))))

  (define-syntax (lookup+expr $syntax)
    (syntax-case $syntax ()
      ((_ lookup id fn)
        #`(lookup+id-item lookup
          #'#,(identifier-append #'id #'% #'id)
          (expr-typer #,(identifier-append #'id #'lookup- #'fn #'->typed))))
      ((_ lookup id)
        #`(lookup+expr lookup id id))))

  (define-case-syntax (lookup+instr lookup id)
    #`(lookup+id-item lookup
      #'#,(identifier-append #'id #'% #'id)
      (instr-typer #,(identifier-append #'id #'gen?-scoped-syntaxes+ #'id))))

  (define (binding-kind $binding)
    (syntax-case (cdr $binding) ()
      ((kind type) #'kind)))

  (define (binding-type $binding)
    (syntax-case (cdr $binding) ()
      ((kind type) #'type)))

  (define (binding-name $binding)
    (car $binding))

  (define (binding $kind $type $name)
    (pair $name #`(#,$kind #,$type)))

  (define (type->syntax $type)
    (literal->syntax (type-size $type)))

  (define (edge->syntax $edge)
    (syntax-case $edge (%posedge %negedge)
      (%posedge #'%posedge)
      (%negedge #'%negedge)))

  (define (literal->typed? $literal)
    (syntax-case $literal ()
      (integer (nonnegative-integer? (datum integer))
        #`(
          #,(literal->syntax (string-length (number->string (datum integer) 2)))
          integer))
      (radix
        (radix->typed? #'radix))))

  (define (radix->typed? $radix)
    (syntax-case $radix ()
      (id (identifier? #'id)
        (or
          (radix-size-id->typed? "bin" 1 #'id)
          (radix-size-id->typed? "oct" 3 #'id)
          (radix-size-id->typed? "hex" 4 #'id)))
      (else #f)))

  (define (int->typed $int)
    (syntax-case $int ()
      ((%int type integer)
        (or
          (type-literal->typed?
            (type->syntax #'type)
            #'integer)
          (syntax-error #'integer "invalid int")))))

  (define (type-literal->typed? $type $literal)
    (syntax-case $literal ()
      (integer (integer? (datum integer))
        #`(#,$type integer))
      (_
        #f)))

  (define (radix-size-id->typed? $radix-string $size $id)
    (lets
      ($prefix (string-append $radix-string "-"))
      ($string (symbol->string (syntax->datum $id)))
      ($length (string-length $string))
      ($prefix-length (string-length $prefix))
      ($radix (bitwise-arithmetic-shift-left 1 $size))
      (and (> $length $prefix-length)
        (lets
          ($string-prefix (substring $string 0 $prefix-length))
          ($data (substring $string $prefix-length $length))
          ($data-length (string-length $data))
          (and (string=? $string-prefix $prefix)
            (lets
              ($number (string->number $data $radix))
              (if (and $number (integer? $number))
                #`(
                  #,(literal->syntax (* $data-length $size))
                  #,(literal->syntax $number))
                (syntax-error $id
                  (format "illegal ~a literal" $radix-string)))))))))

  (define (literal->typed $literal)
    (or
      (literal->typed? $literal)
      (syntax-error $literal "invalid literal")))

  (define (lookup-id->binding $lookup $id)
    (switch (lookup-ref $lookup $id)
      ((procedure? _)
        (syntax-error $id "macro"))
      ((literal-typer? _)
        (syntax-error $id "literal"))
      ((expr-typer? _)
        (syntax-error $id "expression"))
      ((instr-typer? _)
        (syntax-error $id "instruction"))
      ((else $binding)
        $binding)))

  (define (kinds-contains? $kinds $kind)
    (find (partial free-identifier=? $kind) $kinds))

  (define-rule-syntax (kinds kind ...)
    (list #'kind ...))

  (define (lookup-id-kinds->binding $lookup $id $kinds)
    (switch (lookup-ref $lookup $id)
      ((procedure? _)
        (syntax-error $id "macro"))
      ((literal-typer? _)
        (syntax-error $id "literal"))
      ((expr-typer? _)
        (syntax-error $id "expression"))
      ((instr-typer? _)
        (syntax-error $id "statement"))
      ((else $binding)
        (lets
          ($kind (binding-kind $binding))
          (if (kinds-contains? $kinds $kind)
            $binding
            (syntax-error $id
              (format "illegal kind ~a, expected ~a in"
                (syntax->datum $kind)
                (if (single? $kinds)
                  (syntax->datum (car $kinds))
                  `(one-of ,@(map syntax->datum $kinds))))))))))

  (define (lookup-id->typed $lookup $id)
    (lets
      ($binding (lookup-id->binding $lookup $id))
      #`(
        #,(binding-type $binding)
        #,(binding-name $binding))))

  (define (lookup-id->transformer? $lookup $id)
    (switch ($lookup $id)
      ((procedure? $transformer)
        $transformer)
      ((else _)
        #f)))

  (define (lookup-type-expr->typed $lookup $expected-type $expr)
    (if (integer? (syntax->datum $expr))
      (typed $expected-type (syntax->datum $expr))
      (lets
        ($typed (lookup-expr->typed $lookup $expr))
        ($type (typed-type $typed))
        (if (type=? $type $expected-type)
          $typed
          (syntax-error $expr
            (format "type mismatch ~a, expected ~a in"
              (syntax->datum $type)
              (syntax->datum $expected-type)))))))

  (define (lookup-expr->typed $lookup $expr)
    (or
      (literal->typed? $expr)
      (lookup-default-expr->typed $lookup $expr)))

  (define (lookup-default-expr->typed $lookup $expr)
    (syntax-case $expr ()
      ((id arg ...)
        (switch (lookup-ref $lookup (identifier id))
          ((literal-typer? $literal-typer)
            (app (literal-typer-fn $literal-typer) $expr))
          ((expr-typer? $expr-typer)
            (app (expr-typer-fn $expr-typer) $lookup $expr))
          ((procedure? $procedure)
            (fluent $procedure
              (transform $expr $lookup)
              (unbegin-syntax)
              (let $expr (lookup-expr->typed $lookup $expr))))
          ((else $other)
            (syntax-error #'id "not expression"))))
      (id
        (lookup-id->typed $lookup (identifier id)))))

  (define (expr->typed $expr)
    (lookup-expr->typed (lookup+core (empty-lookup)) $expr))

  (define (lookup-op1->typed $lookup $expr)
    (syntax-case $expr ()
      ((op a)
        (lets
          ($typed-a (lookup-expr->typed $lookup #'a))
          ($type-a (typed-type $typed-a))
          (typed $type-a
            #`(op #,$type-a
              #,(typed-value $typed-a)))))))

  (define (lookup-additive1->typed $lookup $expr)
    (syntax-case $expr ()
      ((op a)
        (lets
          ($typed-a (lookup-expr->typed $lookup #'a))
          ($type-a (typed-type $typed-a))
          ($size-a (type-size $type-a))
          ($size (+ $size-a 1))
          ($type (size->type $size))
          (typed $type
            #`(op #,$type
              #,(typed-value $typed-a)))))))

  (define (lookup-relational-op2->typed $lookup $expr)
    (type-lookup-op2->typed #'1 $lookup $expr))

  (define (type-lookup-op2->typed $type $lookup $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (lookup-expr->typed $lookup #'a))
          ($typed-b (lookup-expr->typed $lookup #'b))
          ($size-a (type-size (typed-type $typed-a)))
          ($size-b (type-size (typed-type $typed-b)))
          ($size (max $size-a $size-b))
          (typed $type
            #`(op
              #,(size->type $size)
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)))))))

  (define (lookup-op2->typed $lookup $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (lookup-expr->typed $lookup #'a))
          ($type (typed-type $typed-a))
          ($typed-b (lookup-type-expr->typed $lookup $type #'b))
          (typed $type
            #`(op #,$type
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)))))))

  (define (lookup-op1/2->typed $lookup $expr)
    (syntax-case $expr ()
      ((_ _) (lookup-op1->typed $lookup $expr))
      ((_ _ _) (lookup-op2->typed $lookup $expr))))

  (define (lookup-fold-op2->typed $lookup $op)
    (syntax-case $op ()
      ((op a)
        (lookup-expr->typed $lookup #'a))
      ((op a b)
        (lookup-op2->typed $lookup $op))
      ((op a b c ...)
        (lookup-expr->typed $lookup #'(op (op a b) c ...)))))

  (define (lookup-fold-op1/2->typed $lookup $op)
    (syntax-case $op ()
      ((op a)
        (lookup-op1->typed $lookup $op))
      ((op a b ...)
        (lookup-fold-op2->typed $lookup $op))))

  (define (lookup-additive2->typed $lookup $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (lookup-expr->typed $lookup #'a))
          ($typed-b (lookup-expr->typed $lookup #'b))
          ($size-a (type-size (typed-type $typed-a)))
          ($size-b (type-size (typed-type $typed-b)))
          ($size (+ (max $size-a $size-b) 1))
          ($type (size->type $size))
          (typed $type
            #`(op #,$type
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)))))))

  (define (lookup-multiplicative2->typed $lookup $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (lookup-expr->typed $lookup #'a))
          ($typed-b (lookup-expr->typed $lookup #'b))
          ($size-a (type-size (typed-type $typed-a)))
          ($size-b (type-size (typed-type $typed-b)))
          ($size (+ $size-a $size-b))
          ($type (size->type $size))
          (typed $type
            #`(op #,$type
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)))))))

  (define (lookup-additive1/2->typed $lookup $expr)
    (syntax-case $expr ()
      ((_ _) (lookup-additive1->typed $lookup $expr))
      ((_ _ _) (lookup-additive2->typed $lookup $expr))))

  (define (lookup-if->typed $lookup $if)
    (syntax-case $if (%if)
      ((%if a b c)
        (lets
          ($typed-a (lookup-type-expr->typed $lookup #'1 #'a))
          ($typed-b (lookup-expr->typed $lookup #'b))
          ($type-b (typed-type $typed-b))
          ($typed-c (lookup-type-expr->typed $lookup $type-b #'c))
          (typed $type-b
            #`(%if
              #,$type-b
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)
              #,(typed-value $typed-c)))))))

  (define (lookup-append->typed $lookup $append)
    (syntax-case $append (%append)
      ((%append expr ...)
        (lets
          ($typeds (map (partial lookup-expr->typed $lookup) (syntaxes expr ...)))
          ($types (map typed-type $typeds))
          ($values (map typed-value $typeds))
          ($sizes (map type-size $types))
          ($type (size->type (fold-left + 0 $sizes)))
          (typed $type
            #`(%append
              #,@(map typed $types $values)))))))

  (define (lookup-take->typed $lookup $take)
    (syntax-case $take (%take)
      ((%take a size)
        (lets
          ($typed-a (lookup-expr->typed $lookup #'a))
          ($type-a (typed-type $typed-a))
          ($a-size (type-size $type-a))
          ($size (type-size #'size))
          (if
            (>= $a-size $size)
            (typed #'size
              #`(%take
                size
                #,(typed-value $typed-a)
                size))
            (syntax-error #'a
              (format "type mismatch ~a, expected ~a in"
                (syntax->datum $type-a)
                `(>= ,$size))))))))

  (define (lookup-drop->typed $lookup $drop)
    (syntax-case $drop (%drop)
      ((%drop a drop)
        (lets
          ($typed-a (lookup-expr->typed $lookup #'a))
          ($type-a (typed-type $typed-a))
          ($a-size (type-size $type-a))
          ($drop (nonnegative-number #'drop))
          ($size (- $a-size $drop))
          ($type (size->type $size))
          (if (> $size 0)
            (typed $type
              #`(%drop
                #,$type
                #,(typed-value $typed-a)
                drop))
            (syntax-error #'a "invalid drop")))))) ; TODO: better message

  (define (gen-scoped-binding-name $gen? $lookup $kind $type $name)
    (lets
      ($gen-name (if $gen? (generate-identifier $name) $name))
      ($lookup (lookup+id-item $lookup $name (binding $kind $type $gen-name)))
      (scoped $lookup $gen-name)))

  (define (gen-scoped-name $gen? $lookup $name $item)
    (lets
      ($gen-name (if $gen? (generate-identifier $name) $name))
      ($lookup (lookup+id-item $lookup $name $item))
      (scoped $lookup $gen-name)))

  (define (gen?-scoped-syntaxes+input $gen? (scoped $lookup $syntaxes) $input)
    (syntax-case $input (%input)
      ((%input id)
        (gen?-scoped-syntaxes+instr $gen? (scoped $lookup $syntaxes) #`(%input 1 id)))
      ((%input type id)
        (lets
          ($type (type->syntax #'type))
          (scoped-map
            ($name (gen-scoped-binding-name $gen? $lookup #'%wire $type (identifier id)))
            (push $syntaxes
              #`(%input #,$type #,$name)))))))

  (define (gen?-scoped-syntaxes+output $gen? (scoped $lookup $syntaxes) $output)
    (syntax-case $output ()
      ((%output id expr)
        (lets
          ($typed (lookup-expr->typed $lookup #'expr))
          ($type (typed-type $typed))
          (scoped-map
            ($name (gen-scoped-binding-name $gen? $lookup #'%wire $type (identifier id)))
            (push $syntaxes #`(%output #,$type id #,(typed-value $typed))))))))

  (define (gen?-scoped-syntaxes+wire $gen? (scoped $lookup $syntaxes) $wire)
    (syntax-case $wire ()
      ((%wire id expr)
        (lets
          ($typed (lookup-expr->typed $lookup #'expr))
          ($type (typed-type $typed))
          (scoped-map
            ($name (gen-scoped-binding-name $gen? $lookup #'%wire $type (identifier id)))
            (push $syntaxes #`(%wire #,$type id #,(typed-value $typed))))))))

  (define (gen?-scoped-syntaxes+register $gen? (scoped $lookup $syntaxes) $register)
    (syntax-case $register ()
      ((%register id)
        (gen?-scoped-syntaxes+instr $gen? (scoped $lookup $syntaxes) #`(%register 1 id)))
      ((%register type id)
        (lets
          ($type (type->syntax #'type))
          (scoped-map
            ($name (gen-scoped-binding-name $gen? $lookup #'%register $type (identifier id)))
            (push $syntaxes #`(%register #,$type #,$name)))))))

  (define (gen?-scoped-syntaxes+set $gen? (scoped $lookup $syntaxes) $set)
    (syntax-case $set ()
      ((%set id expr)
        (lets
          ($id-binding (lookup-id-kinds->binding $lookup (identifier id) (kinds %register)))
          ($id-kind (binding-kind $id-binding))
          ($id-type (binding-type $id-binding))
          ($id-size (type-size $id-type))
          ($id-name (binding-name $id-binding))
          ($typed (lookup-expr->typed $lookup #'expr))
          ($type (typed-type $typed))
          ($size (type-size $type))
          (if (<= $size $id-size)
            (scoped $lookup
              (push $syntaxes #`(%set #,$id-type #,$id-name #,(typed-value $typed))))
            (syntax-error #'expr
              (format "invalid type ~a, expected <= ~a in" $size $id-size)))))))

  (define (gen?-scoped-syntaxes+cond $gen? (scoped $lookup $syntaxes) $cond)
    (syntax-case $cond (%else)
      ((%cond clause ... (%else els ...))
        (scoped $lookup
          (push $syntaxes
            #`(%cond
              #,@(map
                (partial gen?-lookup-clause->typed-syntax $gen? $lookup)
                (syntaxes clause ...))
              (%else
                #,@(syntax->list
                  (gen?-lookup-instrs->typed-syntax $gen? $lookup #'(els ...))))))))
      ((%cond clause clause* ...)
        (scoped $lookup
          (push $syntaxes
            #`(%cond
              #,@(map
                (partial gen?-lookup-clause->typed-syntax $gen? $lookup)
                (syntaxes clause clause* ...))))))))

  (define (gen?-scoped-syntaxes+on $gen? (scoped $lookup $syntaxes) $on)
    (syntax-case $on ()
      ((%on (edge name) body ...)
        (scoped $lookup
          (push $syntaxes
            #`(%on
              (
                #,(edge->syntax #'edge)
                #,(typed-value (lookup-type-expr->typed $lookup #'1 #'name)))
              #,@(syntax->list (gen?-lookup-instrs->typed-syntax $gen? $lookup #'(body ...)))))))))

  (define (gen?-scoped-syntaxes+repeat $gen? $scoped $repeat)
    (syntax-case $repeat ()
      ((%repeat (index count) body ...)
        (fold-left
          (lambda ($scoped $index)
            (gen?-scoped-syntaxes+instrs #t $scoped
              (syntax-subst
                #'index
                (literal->syntax $index)
                (syntaxes body ...))))
          $scoped
          (indices (count-number #'count))))))

  (define (gen?-scoped-syntaxes+log $gen? (scoped $lookup $syntaxes) $log)
    (syntax-case $log ()
      ((%log label expr)
        (lets
          ($typed (lookup-expr->typed $lookup #'expr))
          (scoped $lookup
            (push $syntaxes
              #`(%log label
                #,(typed-type $typed)
                #,(typed-value $typed))))))))

  (define (gen?-scoped-syntaxes+macro $gen? (scoped $lookup $syntaxes) $macro)
    (syntax-case $macro ()
      ((%macro (name param ...) body ...)
        (scoped-map
          ($name (gen-scoped-name $gen? $lookup (identifier name)
            (lambda ($syntax)
              (syntax-case $syntax ()
                ((_ arg ...)
                  (syntax-subst
                    #'(param ...)
                    #'(arg ...)
                    #'(begin body ...)))))))
          $syntaxes))))

  (define (gen?-scoped-syntaxes+instr $gen? $scoped $instr)
    (lets
      ((scoped $lookup $syntaxes) $scoped)
      (syntax-case $instr ()
        ((id arg ...)
          (switch (lookup-ref $lookup (identifier id))
            ((instr-typer? $instr-typer)
              (fluent $instr-typer
                (instr-typer-fn)
                (app $gen? $scoped $instr)))
            ((procedure? $transformer)
              (gen?-scoped-syntaxes+instrs #t $scoped
                (list->syntax
                  (unbegin-syntaxes
                    (transform $transformer $instr $lookup)))))
            ((else $other)
              (syntax-error #'id "not statement")))))))

  (define (gen?-lookup-clause->typed-syntax $gen? $lookup $clause)
    (syntax-case $clause ()
      ((cond body ...)
        #`(
          #,(typed-value (lookup-type-expr->typed $lookup #'1 #'cond))
          #,@(syntax->list (gen?-lookup-instrs->typed-syntax $gen? $lookup #'(body ...)))))))

  (define (gen?-scoped-syntaxes+instrs $gen? $scoped $instrs)
    (fold-left (partial gen?-scoped-syntaxes+instr $gen?) $scoped (syntax->list $instrs)))

  (define (gen?-lookup-instrs->typed-syntax $gen? $lookup $instrs)
    (fluent $gen?
      (gen?-scoped-syntaxes+instrs (scoped $lookup (stack)) (syntax->list $instrs))
      (scoped-value)
      (reverse)
      (list->syntax)))

  (define (lookup-instrs->typed-syntax $lookup $instrs)
    (gen?-lookup-instrs->typed-syntax #f $lookup $instrs))

  (define (module->typed-syntax $module)
    (lookup-module->typed-syntax (lookup+core (empty-lookup)) $module))

  (define (lookup-module->typed-syntax $lookup $module)
    (syntax-case $module ()
      ((%module name body ...)
        #`(%module #,(identifier name)
          #,@(syntax->list
            (gen?-lookup-instrs->typed-syntax
              #f
              $lookup
              #'(body ...)))))))

  (define (gen?-lookup-instr->typed-syntax $gen? $lookup $instr)
    (syntax-single (gen?-lookup-instrs->typed-syntax $gen? $lookup #`(#,$instr))))

  (define (lookup-instr->typed-syntax $lookup $instr)
    (gen?-lookup-instr->typed-syntax #f $lookup $instr))

  (define (typed $type $value)
    #`(#,$type #,$value))

  (define (typed-type $typed)
    (syntax-case $typed ()
      ((type value) #'type)))

  (define (typed-value $typed)
    (syntax-case $typed ()
      ((type value) #'value)))

  (define (type-size $type)
    (syntax-case $type ()
      (size (positive-integer? (datum size)) (datum size))
      (_ (syntax-error $type "illegal type"))))

  (define (nonnegative-number $syntax)
    (syntax-case $syntax ()
      (size (nonnegative-integer? (datum size)) (datum size))
      (_ (syntax-error $syntax "illegal nonnegative number"))))

  (define (shift-number $shift)
    (syntax-case $shift ()
      (size (nonnegative-integer? (datum size)) (datum size))
      (_ (syntax-error $shift "illegal shift"))))

  (define (count-number $count)
    (syntax-case $count ()
      (count (nonnegative-integer? (datum count)) (datum count))
      (_ (syntax-error $count "illegal count"))))

  (define (size->type $size)
    (literal->syntax $size))

  (define type=? syntax=?)
)
