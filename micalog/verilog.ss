(library (micalog verilog)
  (export
    name->verilog
    edge->verilog
    value->verilog
    module->verilog
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

  (define (item-kind-of? $kind $item)
    (syntax-case $item ()
      ((kind body ...)
        (free-identifier=? #'kind $kind))))

  (define (module->verilog $module)
    (syntax-case $module (%module)
      ((%module declaration ...)
        (lets
          ($items (syntaxes declaration ...))
          ($inputs (filter (partial item-kind-of? #'%input) $items))
          ($outputs (filter (partial item-kind-of? #'%output) $items))
          ($internals (filter (partial item-kind-of? #'%internal) $items))
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
          #`(%%reg
            #,@(opt->list (type->verilog? $type))
            #,(name->verilog $name)
            #,(value->verilog #'init))
          #`(%%always
            (
              #,(edge->verilog #'edge)
              #,(value->verilog #'domain))
            (%%set!
              #,(name->verilog $name)
              #,(value->verilog #'update)))))
      (wire
        (filter-opts
          (list
            (and (free-identifier=? $kind #'%internal)
              #`(%%wire
                #,@(opt->list (type->verilog? $type))
                #,(name->verilog $name)))
            (syntax-case #'wire (%+)
              ((%+ type a b)
                #`(%%always %%*
                  (%%assign
                    #,(name->verilog $name)
                    (%%+
                      #,(value->verilog #'a)
                      #,(value->verilog #'b)))))
              (value
                #`(%%always %%*
                  (%%assign
                    #,(name->verilog $name)
                    #,(value->verilog #'value))))))))))

  (define (edge->verilog $edge)
    (syntax-case $edge ()
      (0 #'%%negedge)
      (1 #'%%posedge)))

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
