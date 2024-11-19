(library (micalog verilog)
  (export
    name->verilog
    edge->verilog
    event->verilog
    value->verilog
    module->verilog
    expr->verilog
    register-declaration->verilog
    register-update->verilog
    parameter->verilog
    declaration->verilogs
    kind->verilog?
    type->verilog?)

  (import
    (micascheme)
    (micalog model)
    (prefix (micalog keywords) %)
    (prefix (verilog keywords) %%))

  (define (module->verilog $module)
    (syntax-case $module (%module)
      ((%module declaration ...)
        (lets
          ($inputs (declaration-syntaxes %input declaration ...))
          ($outputs (declaration-syntaxes %output declaration ...))
          ($internals (declaration-syntaxes %internal declaration ...))
          #`(module
            (micalog #,@(map parameter->verilog (append $inputs $outputs)))
            #,@(flatten (map declaration->verilogs (append $outputs $internals))))))))

  (define (parameter->verilog $parameter)
    (syntax-case $parameter (%input %output)
      ((%input type name)
        #`(
          %%input
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))
      ((%output type name _)
        #`(
          %%output
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))))

  (define (declaration->verilogs $declaration)
    (syntax-case $declaration (%internal)
      ((kind type name body)
        (body->verilogs #'kind #'type #'name #'body))))

  (define (kind->verilog? $kind)
    (syntax-case $kind (%input %output %internal)
      (%input #'%%input)
      (%output #'%%output)
      (%internal #f)))

  (define (type->verilog? $type)
    (syntax-case $type ()
      (number (positive-integer? (datum number))
        (and
          (not (= (datum number) 1))
          #`(#,(- (datum number) 1) %%to 0)))))

  (define (body->verilogs $kind $type $name $value)
    (syntax-case $value (%register)
      ((%register type init (on event update))
        (list
          (register-declaration->verilog #`(#,$name type init))
          (register-update->verilog #`(#,$name (on event update)))))
      (expr
        (non-false-list
          (wire->verilog-declaration? $kind $type $name)
          (assign->verilog $name #'expr)))))

  (define (register-declaration->verilog $declaration)
    (syntax-case $declaration ()
      ((name type init)
        #`(%%reg
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)
          #,@(opt->list (init->verilog? #'init))))))

  (define (register-update->verilog $body)
    (syntax-case $body (%on)
      ((name (%on event update))
        #`(%%always
          #,(event->verilog #'event)
          (%%set!
            #,(name->verilog #'name)
            #,(value->verilog #'update))))))

  (define (wire->verilog-declaration? $kind $type $name)
    (and
      (free-identifier=? $kind #'%internal)
      #`(%%wire
        #,@(opt->list (type->verilog? $type))
        #,(name->verilog $name))))

  (define (assign->verilog $name $expr)
    #`(%%assign
      #,(name->verilog $name)
      #,(expr->verilog $expr)))

  (define (expr->verilog $expr)
    (syntax-case $expr (%append %slice %= %!= %< %<= %> %>= %not %and %or %xor %nand %nor %xnor %add %sub %neg)
      ((%append type xs ...)
        #`(%%append
          #,@(map value->verilog (syntaxes xs ...))))
      ((%slice type a shift)
        (lets
          ($shift (datum shift))
          ($mask (datum type))
          #`(%%ref
            #,(value->verilog #'a)
            (
              #,(literal->syntax (+ $shift $mask -1))
              %%to
              #,(literal->syntax $shift)))))
      ((%= type a b)
        (op2->verilog #'%%= #'a #'b))
      ((%!= type a b)
        (op2->verilog #'%%!= #'a #'b))
      ((%< type a b)
        (op2->verilog #'%%< #'a #'b))
      ((%<= type a b)
        (op2->verilog #'%%<= #'a #'b))
      ((%> type a b)
        (op2->verilog #'%%> #'a #'b))
      ((%>= type a b)
        (op2->verilog #'%%>= #'a #'b))
      ((%not type a)
        (op->verilog #'%%inv #'a))
      ((%and type a b)
        (op2->verilog #'%%and #'a #'b))
      ((%or type a b)
        (op2->verilog #'%%or #'a #'b))
      ((%xor type a b)
        (op2->verilog #'%%xor #'a #'b))
      ((%nand type a b)
        (op2->verilog #'%%nand #'a #'b))
      ((%nor type a b)
        (op2->verilog #'%%nor #'a #'b))
      ((%xnor type a b)
        (op2->verilog #'%%xnor #'a #'b))
      ((%add type a b)
        (op2->verilog #'%%+ #'a #'b))
      ((%sub type a b)
        (op2->verilog #'%%- #'a #'b))
      ((%neg type a)
        (op->verilog #'%%- #'a))
      (value
        (value->verilog #'value))))

  (define (op->verilog $op $rhs)
    #`(
      #,$op
      #,(value->verilog $rhs)))

  (define (op2->verilog $op $lhs $rhs)
    #`(
      #,$op
      #,(value->verilog $lhs)
      #,(value->verilog $rhs)))

  (define (edge->verilog $edge)
    (syntax-case $edge (%posedge %negedge)
      (%posedge #'%%posedge)
      (%negedge #'%%negedge)))

  (define (event->verilog $event)
    (syntax-case $event ()
      ((edge value)
        #`(
          #,(edge->verilog #'edge)
          #,(value->verilog #'value)))))

  (define (value->verilog $value)
    (syntax-case $value ()
      (integer (integer? (datum integer))
        #'integer)
      (name
        (name->verilog #'name))))

  (define (init->verilog? $init)
    (syntax-case $init (%init)
      ((%init) #f)
      ((%init value) (value->verilog #'value))))

  (define (name->verilog $name)
    (syntax-case $name ()
      (name (identifier? #'name)
        #'name)))
)
