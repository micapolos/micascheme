; TODO:
; - check that register is assigned within single domain
(library (micalog core type)
  (export
    literal->typed
    int->typed
    expr->typed
    type->syntax
    scope-expr->typed
    gen?-scope-instr->typed-syntax
    gen?-scope-instrs->typed-syntax
    scope-instr->typed-syntax
    scope-instrs->typed-syntax
    scope-module->typed-syntax
    module->typed-syntax
    scope+core)
  (import
    (micascheme)
    (syntax scope)
    (syntax scoped)
    (only (micalog core utils) opposite-edges?)
    (prefix (micalog keywords) %))

  (data (literal-typer fn))
  (data (expr-typer fn))
  (data (instr-typer fn))

  (define (scope+core $scope)
    (fluent $scope
      (scope+literal int)
      (scope+expr append)
      (scope+expr take)
      (scope+expr drop)
      (scope+expr = relational-op2)
      (scope+expr != relational-op2)
      (scope+expr < relational-op2)
      (scope+expr <= relational-op2)
      (scope+expr > relational-op2)
      (scope+expr >= relational-op2)
      (scope+expr not op1)
      (scope+expr and fold-op2)
      (scope+expr or fold-op2)
      (scope+expr xor fold-op2)
      (scope+expr nand fold-op2)
      (scope+expr nor fold-op2)
      (scope+expr xnor fold-op2)
      (scope+expr wrap+ fold-op2)
      (scope+expr wrap- fold-op1/2)
      (scope+expr wrap* fold-op2)
      (scope+expr + additive2)
      (scope+expr - additive1/2)
      (scope+expr * multiplicative2)
      (scope+expr if)
      (scope+instr input)
      (scope+instr output)
      (scope+instr wire)
      (scope+instr register)
      (scope+instr set)
      (scope+instr cond)
      (scope+instr on)
      (scope+instr repeat)
      (scope+instr log)
      (scope+instr macro)))

  (define (scope+id-item $scope $id $item)
    (if (radix->typed? $id)
      (syntax-error $id "can not redefine literal")
      (scope+undefined $scope $id $item)))

  (define-syntax (scope+literal $syntax)
    (syntax-case $syntax ()
      ((_ scope id)
        #`(scope+id-item scope
          #'#,(identifier-append #'id #'% #'id)
          (literal-typer #,(identifier-append #'id #'id #'->typed))))))

  (define-syntax (scope+expr $syntax)
    (syntax-case $syntax ()
      ((_ scope id fn)
        #`(scope+id-item scope
          #'#,(identifier-append #'id #'% #'id)
          (expr-typer #,(identifier-append #'id #'scope- #'fn #'->typed))))
      ((_ scope id)
        #`(scope+expr scope id id))))

  (define-case-syntax (scope+instr scope id)
    #`(scope+id-item scope
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

  (define (scope-id->binding $scope $id)
    (switch (scope-item $scope $id)
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

  (define (scope-id-kinds->binding $scope $id $kinds)
    (switch (scope-item $scope $id)
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

  (define (scope-id->typed $scope $id)
    (lets
      ($binding (scope-id->binding $scope $id))
      #`(
        #,(binding-type $binding)
        #,(binding-name $binding))))

  (define (scope-id->transformer? $scope $id)
    (switch (scope-ref $scope $id)
      ((procedure? $transformer)
        $transformer)
      ((else _)
        #f)))

  (define (scope-type-expr->typed $scope $expected-type $expr)
    (if (integer? (syntax->datum $expr))
      (typed $expected-type (syntax->datum $expr))
      (lets
        ($typed (scope-expr->typed $scope $expr))
        ($type (typed-type $typed))
        (if (type=? $type $expected-type)
          $typed
          (syntax-error $expr
            (format "type mismatch ~a, expected ~a in"
              (syntax->datum $type)
              (syntax->datum $expected-type)))))))

  (define (scope-expr->typed $scope $expr)
    (or
      (literal->typed? $expr)
      (scope-default-expr->typed $scope $expr)))

  (define (scope-default-expr->typed $scope $expr)
    (syntax-case $expr ()
      ((id arg ...)
        (switch (scope-item $scope (identifier id))
          ((literal-typer? $literal-typer)
            (app (literal-typer-fn $literal-typer) $expr))
          ((expr-typer? $expr-typer)
            (app (expr-typer-fn $expr-typer) $scope $expr))
          ((procedure? $procedure)
            (fluent $procedure
              (transform $expr $scope)
              (unbegin-syntax)
              (let $expr (scope-expr->typed $scope $expr))))
          ((else $other)
            (syntax-error #'id "not expression"))))
      (id
        (scope-id->typed $scope (identifier id)))))

  (define (expr->typed $expr)
    (scope-expr->typed (scope+core (empty-scope)) $expr))

  (define (scope-op1->typed $scope $expr)
    (syntax-case $expr ()
      ((op a)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($type-a (typed-type $typed-a))
          (typed $type-a
            #`(op #,$type-a
              #,(typed-value $typed-a)))))))

  (define (scope-additive1->typed $scope $expr)
    (syntax-case $expr ()
      ((op a)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($type-a (typed-type $typed-a))
          ($size-a (type-size $type-a))
          ($size (+ $size-a 1))
          ($type (size->type $size))
          (typed $type
            #`(op #,$type
              #,(typed-value $typed-a)))))))

  (define (scope-relational-op2->typed $scope $expr)
    (type-scope-op2->typed #'1 $scope $expr))

  (define (type-scope-op2->typed $type $scope $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($typed-b (scope-expr->typed $scope #'b))
          ($size-a (type-size (typed-type $typed-a)))
          ($size-b (type-size (typed-type $typed-b)))
          ($size (max $size-a $size-b))
          (typed $type
            #`(op
              #,(size->type $size)
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)))))))

  (define (scope-op2->typed $scope $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($type (typed-type $typed-a))
          ($typed-b (scope-type-expr->typed $scope $type #'b))
          (typed $type
            #`(op #,$type
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)))))))

  (define (scope-op1/2->typed $scope $expr)
    (syntax-case $expr ()
      ((_ _) (scope-op1->typed $scope $expr))
      ((_ _ _) (scope-op2->typed $scope $expr))))

  (define (scope-fold-op2->typed $scope $op)
    (syntax-case $op ()
      ((op a)
        (scope-expr->typed $scope #'a))
      ((op a b)
        (scope-op2->typed $scope $op))
      ((op a b c ...)
        (scope-expr->typed $scope #'(op (op a b) c ...)))))

  (define (scope-fold-op1/2->typed $scope $op)
    (syntax-case $op ()
      ((op a)
        (scope-op1->typed $scope $op))
      ((op a b ...)
        (scope-fold-op2->typed $scope $op))))

  (define (scope-additive2->typed $scope $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($typed-b (scope-expr->typed $scope #'b))
          ($size-a (type-size (typed-type $typed-a)))
          ($size-b (type-size (typed-type $typed-b)))
          ($size (+ (max $size-a $size-b) 1))
          ($type (size->type $size))
          (typed $type
            #`(op #,$type
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)))))))

  (define (scope-multiplicative2->typed $scope $expr)
    (syntax-case $expr ()
      ((op a b)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
          ($typed-b (scope-expr->typed $scope #'b))
          ($size-a (type-size (typed-type $typed-a)))
          ($size-b (type-size (typed-type $typed-b)))
          ($size (+ $size-a $size-b))
          ($type (size->type $size))
          (typed $type
            #`(op #,$type
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)))))))

  (define (scope-additive1/2->typed $scope $expr)
    (syntax-case $expr ()
      ((_ _) (scope-additive1->typed $scope $expr))
      ((_ _ _) (scope-additive2->typed $scope $expr))))

  (define (scope-if->typed $scope $if)
    (syntax-case $if (%if)
      ((%if a b c)
        (lets
          ($typed-a (scope-type-expr->typed $scope #'1 #'a))
          ($typed-b (scope-expr->typed $scope #'b))
          ($type-b (typed-type $typed-b))
          ($typed-c (scope-type-expr->typed $scope $type-b #'c))
          (typed $type-b
            #`(%if
              #,$type-b
              #,(typed-value $typed-a)
              #,(typed-value $typed-b)
              #,(typed-value $typed-c)))))))

  (define (scope-append->typed $scope $append)
    (syntax-case $append (%append)
      ((%append expr ...)
        (lets
          ($typeds (map (partial scope-expr->typed $scope) (syntaxes expr ...)))
          ($types (map typed-type $typeds))
          ($values (map typed-value $typeds))
          ($sizes (map type-size $types))
          ($type (size->type (fold-left + 0 $sizes)))
          (typed $type
            #`(%append
              #,@(map typed $types $values)))))))

  (define (scope-take->typed $scope $take)
    (syntax-case $take (%take)
      ((%take a size)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
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

  (define (scope-drop->typed $scope $drop)
    (syntax-case $drop (%drop)
      ((%drop a drop)
        (lets
          ($typed-a (scope-expr->typed $scope #'a))
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

  (define (gen-scoped-binding-name $gen? $scope $kind $type $name)
    (lets
      ($gen-name (if $gen? (generate-identifier $name) $name))
      ($scope (scope+id-item $scope $name (binding $kind $type $gen-name)))
      (scoped $scope $gen-name)))

  (define (gen-scoped-name $gen? $scope $name $item)
    (lets
      ($gen-name (if $gen? (generate-identifier $name) $name))
      ($scope (scope+id-item $scope $name $item))
      (scoped $scope $gen-name)))

  (define (gen?-scoped-syntaxes+input $gen? (scoped $scope $syntaxes) $input)
    (syntax-case $input (%input)
      ((%input id)
        (gen?-scoped-syntaxes+instr $gen? (scoped $scope $syntaxes) #`(%input 1 id)))
      ((%input type id)
        (lets
          ($type (type->syntax #'type))
          (scoped-map
            ($name (gen-scoped-binding-name $gen? $scope #'%wire $type (identifier id)))
            (push $syntaxes
              #`(%input #,$type #,$name)))))))

  (define (gen?-scoped-syntaxes+output $gen? (scoped $scope $syntaxes) $output)
    (syntax-case $output ()
      ((%output id expr)
        (lets
          ($typed (scope-expr->typed $scope #'expr))
          ($type (typed-type $typed))
          (scoped-map
            ($name (gen-scoped-binding-name $gen? $scope #'%wire $type (identifier id)))
            (push $syntaxes #`(%output #,$type id #,(typed-value $typed))))))))

  (define (gen?-scoped-syntaxes+wire $gen? (scoped $scope $syntaxes) $wire)
    (syntax-case $wire ()
      ((%wire id expr)
        (lets
          ($typed (scope-expr->typed $scope #'expr))
          ($type (typed-type $typed))
          (scoped-map
            ($name (gen-scoped-binding-name $gen? $scope #'%wire $type (identifier id)))
            (push $syntaxes #`(%wire #,$type id #,(typed-value $typed))))))))

  (define (gen?-scoped-syntaxes+register $gen? (scoped $scope $syntaxes) $register)
    (syntax-case $register ()
      ((%register id)
        (gen?-scoped-syntaxes+instr $gen? (scoped $scope $syntaxes) #`(%register 1 id)))
      ((%register type id)
        (lets
          ($type (type->syntax #'type))
          (scoped-map
            ($name (gen-scoped-binding-name $gen? $scope #'%register $type (identifier id)))
            (push $syntaxes #`(%register #,$type #,$name)))))))

  (define (gen?-scoped-syntaxes+set $gen? (scoped $scope $syntaxes) $set)
    (syntax-case $set ()
      ((%set id expr)
        (lets
          ($id-binding (scope-id-kinds->binding $scope (identifier id) (kinds %register)))
          ($id-kind (binding-kind $id-binding))
          ($id-type (binding-type $id-binding))
          ($id-size (type-size $id-type))
          ($id-name (binding-name $id-binding))
          ($typed (scope-expr->typed $scope #'expr))
          ($type (typed-type $typed))
          ($size (type-size $type))
          (if (<= $size $id-size)
            (scoped $scope
              (push $syntaxes #`(%set #,$id-type #,$id-name #,(typed-value $typed))))
            (syntax-error #'expr
              (format "invalid type ~a, expected <= ~a in" $size $id-size)))))))

  (define (gen?-scoped-syntaxes+cond $gen? (scoped $scope $syntaxes) $cond)
    (syntax-case $cond (%else)
      ((%cond clause ... (%else els ...))
        (scoped $scope
          (push $syntaxes
            #`(%cond
              #,@(map
                (partial gen?-scope-clause->typed-syntax $gen? $scope)
                (syntaxes clause ...))
              (%else
                #,@(syntax->list
                  (gen?-scope-instrs->typed-syntax $gen? $scope #'(els ...))))))))
      ((%cond clause clause* ...)
        (scoped $scope
          (push $syntaxes
            #`(%cond
              #,@(map
                (partial gen?-scope-clause->typed-syntax $gen? $scope)
                (syntaxes clause clause* ...))))))))

  (define (gen?-scoped-syntaxes+on $gen? (scoped $scope $syntaxes) $on)
    (syntax-case $on ()
      ((%on (edge name) body ...)
        (scoped $scope
          (push $syntaxes
            #`(%on
              (
                #,(edge->syntax #'edge)
                #,(typed-value (scope-type-expr->typed $scope #'1 #'name)))
              #,@(syntax->list (gen?-scope-instrs->typed-syntax $gen? $scope #'(body ...)))))))))

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

  (define (gen?-scoped-syntaxes+log $gen? (scoped $scope $syntaxes) $log)
    (syntax-case $log ()
      ((%log label expr)
        (lets
          ($typed (scope-expr->typed $scope #'expr))
          (scoped $scope
            (push $syntaxes
              #`(%log label
                #,(typed-type $typed)
                #,(typed-value $typed))))))))

  (define (gen?-scoped-syntaxes+macro $gen? (scoped $scope $syntaxes) $macro)
    (syntax-case $macro ()
      ((%macro (name param ...) body ...)
        (scoped-map
          ($name (gen-scoped-name $gen? $scope (identifier name)
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
      ((scoped $scope $syntaxes) $scoped)
      (syntax-case $instr ()
        ((id arg ...)
          (switch (scope-item $scope (identifier id))
            ((instr-typer? $instr-typer)
              (fluent $instr-typer
                (instr-typer-fn)
                (app $gen? $scoped $instr)))
            ((procedure? $transformer)
              (gen?-scoped-syntaxes+instrs #t $scoped
                (list->syntax
                  (unbegin-syntaxes
                    (transform $transformer $instr $scope)))))
            ((else $other)
              (syntax-error #'id "not statement")))))))

  (define (gen?-scope-clause->typed-syntax $gen? $scope $clause)
    (syntax-case $clause ()
      ((cond body ...)
        #`(
          #,(typed-value (scope-type-expr->typed $scope #'1 #'cond))
          #,@(syntax->list (gen?-scope-instrs->typed-syntax $gen? $scope #'(body ...)))))))

  (define (gen?-scoped-syntaxes+instrs $gen? $scoped $instrs)
    (fold-left (partial gen?-scoped-syntaxes+instr $gen?) $scoped (syntax->list $instrs)))

  (define (gen?-scope-instrs->typed-syntax $gen? $scope $instrs)
    (fluent $gen?
      (gen?-scoped-syntaxes+instrs (scoped $scope (stack)) (syntax->list $instrs))
      (scoped-value)
      (reverse)
      (list->syntax)))

  (define (scope-instrs->typed-syntax $scope $instrs)
    (gen?-scope-instrs->typed-syntax #f $scope $instrs))

  (define (module->typed-syntax $module)
    (scope-module->typed-syntax (scope+core (empty-scope)) $module))

  (define (scope-module->typed-syntax $scope $module)
    (syntax-case $module ()
      ((%module name body ...)
        #`(%module #,(identifier name)
          #,@(syntax->list
            (gen?-scope-instrs->typed-syntax
              #f
              $scope
              #'(body ...)))))))

  (define (gen?-scope-instr->typed-syntax $gen? $scope $instr)
    (syntax-single (gen?-scope-instrs->typed-syntax $gen? $scope #`(#,$instr))))

  (define (scope-instr->typed-syntax $scope $instr)
    (gen?-scope-instr->typed-syntax #f $scope $instr))

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
