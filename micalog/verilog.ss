(library (micalog verilog)
  (export
    name->verilog
    edge->verilog
    value->verilog
    module->verilog
    expr->verilog
    parameter->verilog
    declaration->verilogs
    kind->verilog?
    type->verilog?
    value->verilog)

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
      ((%register type init domain edge update)
        (list
          (reg->verilog-declaration $type $name #'init)
          (reg->verilog-body $name #'domain #'edge #'update)))
      (expr
        (non-false-list
          (wire->verilog-declaration? $kind $type $name)
          (assign->verilog $name #'expr)))))

  (define (reg->verilog-declaration $type $name $init)
    #`(%%reg
      #,@(opt->list (type->verilog? $type))
      #,(name->verilog $name)
      #,(value->verilog $init)))

  (define (reg->verilog-body $name $domain $edge $update)
    #`(%%always
      (
        #,(edge->verilog $edge)
        #,(value->verilog $domain))
      (%%set!
        #,(name->verilog $name)
        #,(value->verilog $update))))

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
    (syntax-case $expr (%append %slice %not %and %or %add %sub)
      ((%append type a b)
        #`(%%append
          #,(value->verilog #'a)
          #,(value->verilog #'b)))
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
      ((%not type a)
        #`(%%inv
          #,(value->verilog #'a)))
      ((%and type a b)
        #`(%%and
          #,(value->verilog #'a)
          #,(value->verilog #'b)))
      ((%or type a b)
        #`(%%or
          #,(value->verilog #'a)
          #,(value->verilog #'b)))
      ((%add type a b)
        #`(%%+
          #,(value->verilog #'a)
          #,(value->verilog #'b)))
      ((%sub type a b)
        #`(%%-
          #,(value->verilog #'a)
          #,(value->verilog #'b)))
      (value
        (value->verilog #'value))))

  (define (edge->verilog $edge)
    (syntax-case $edge (%posedge %negedge)
      (%posedge #'%%posedge)
      (%negedge #'%%negedge)))

  (define (value->verilog $value)
    (syntax-case $value ()
      (integer (integer? (datum integer))
        #'integer)
      (name
        (name->verilog #'name))))

  (define (name->verilog $name)
    (syntax-case $name ()
      (name (identifier? #'name)
        #'name)))
)
