(library (micalog verilog)
  (export
    name->verilog
    edge->verilog
    event->verilog
    value->verilog
    module->verilog
    expr->verilog
    register-declaration->verilog
    input->verilog
    output->verilog
    declaration->init-names
    declaration-instrs->verilog
    declaration-declarations->verilog
    init-names->verilog
    init->verilog
    type->verilog?)

  (import
    (micascheme)
    (micalog model)
    (prefix (micalog keywords) %)
    (prefix (verilog keywords) %%))

  (define (module->verilog $module)
    (syntax-case $module (%module %input %internal %output)
      ((%module name
        (%input input ...)
        (%internal internal ...)
        (%output output ...))
        (lets
          ($init-names (flatten (map declaration->init-names (syntaxes internal ...))))
          #`(%%module
            (
              #,(name->verilog #'name)
              #,@(map input->verilog (syntaxes input ...))
              #,@(map output->verilog (syntaxes output ...)))
            #,@(flatten
              (map
                (partial declaration->verilog-declarations $init-names)
                (syntaxes internal ...)))
            #,@(flatten
              (map
                (partial declaration->verilog-instrs $init-names #t)
                (syntaxes internal ...)))
            #,@(map output-assign->verilog (syntaxes output ...)))))))

  (define (input->verilog $input)
    (syntax-case $input ()
      ((name type)
        #`(
          %%input
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))))

  (define (output->verilog $parameter)
    (syntax-case $parameter ()
      ((name type expr)
        #`(
          %%output
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))))

  (define (declaration->verilog-instrs $init-names $top-level? $declaration)
    (syntax-case $declaration (%on)
      ((name (%on process ...))
        (if $top-level?
          (map
            (partial process->verilog $init-names #'name)
            (syntaxes process ...))
          (list)))
      (instr
        (instr->verilogs $init-names #'instr))))

  (define (instr->verilogs $init-names $instr)
    (syntax-case $instr ()
      ((name type expr)
        (opt->list
          (and
            (name-init? $init-names #'name)
            #`(%%set!
              #,(name->verilog #'name)
              #,(expr->verilog #'expr)))))))

  (define (process->verilog $init-names $name $process)
    (syntax-case $process (%init %update)
      ((edge (%init init ...) (%update update ...))
        #`(%%always
          (
            #,(edge->verilog #'edge)
            #,(name->verilog $name))
          #,@(flatten
            (map
              (partial declaration->verilog-instrs $init-names #f)
              (syntaxes update ...)))))))

  (define (type->verilog? $type)
    (syntax-case $type ()
      (number (positive-integer? (datum number))
        (and
          (not (= (datum number) 1))
          #`(#,(- (datum number) 1) %%to 0)))))

  (define (register-declaration->verilog $declaration)
    (syntax-case $declaration ()
      ((name type init)
        #`(%%reg
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)
          #,@(opt->list (init->verilog? #'init))))))

  (define (wire->verilog $name $type)
    #`(%%wire
      #,@(opt->list (type->verilog? $type))
      #,(name->verilog $name)))

  (define (assign->verilog $name $expr)
    #`(%%assign
      #,(name->verilog $name)
      #,(expr->verilog $expr)))

  (define (expr->verilog $expr)
    (syntax-case $expr (%append %slice %= %!= %< %<= %> %>= %if %not %and %or %xor %nand %nor %xnor %add %sub %neg)
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
        (op->verilog #'%%not #'a))
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
      ((%if type cond true false)
        #`(%%if
          #,(expr->verilog #'cond)
          #,(expr->verilog #'true)
          #,(expr->verilog #'false)))
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

  (define (declaration->init-names $declaration)
    (syntax-case $declaration (%on)
      ((name (%on process ...))
        (flatten (map process->init-names (syntaxes process ...))))
      (_
        (list))))

  (define (process->init-names $process)
    (syntax-case $process (%init %update)
      ((edge (%init init ...) (%update update ...))
        (append
          (map init->name (syntaxes init ...))
          (flatten (map declaration->init-names (syntaxes update ...)))))))

  (define (init->name $init)
    (syntax-case $init ()
      ((name type expr) #'name)))

  (define (declaration->verilog-declarations $init-names $declaration)
    (syntax-case $declaration (%on)
      ((name (%on process ...))
        (flatten
          (map
            (partial process->verilog-declarations $init-names)
            (syntaxes process ...))))
      ((name type expr)
        (if (name-init? $init-names #'name)
          (list)
          (list
            (wire->verilog #'name #'type)
            (assign->verilog #'name #'expr))))))

  (define (output-assign->verilog $output)
    (syntax-case $output ()
      ((name type expr)
        (assign->verilog #'name #'expr))))

  (define (process->verilog-declarations $init-names $process)
    (syntax-case $process (%init %update)
      ((edge (%init init ...) (%update update ...))
        (append
          (map init->verilog (syntaxes init ...))
          (flatten
            (map
              (partial declaration->verilog-declarations $init-names)
              (syntaxes update ...)))))))

  (define (init->verilog $init)
    (syntax-case $init ()
      ((name type expr)
        #`(%%reg
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)
          #,(expr->verilog #'expr)))))

  (define (declaration-declarations->verilog $declaration)
    #`(
      #,@(declaration->verilog-declarations
        (declaration->init-names $declaration)
        $declaration)))

  (define (declaration-instrs->verilog $declaration)
    #`(
      #,@(declaration->verilog-instrs
        (declaration->init-names $declaration)
        #t
        $declaration)))

  (define (init-names->verilog $declaration)
    (declaration->init-names $declaration))

  (define (name-init? $init-names $name)
    (find (partial free-identifier=? $name) $init-names))
)
