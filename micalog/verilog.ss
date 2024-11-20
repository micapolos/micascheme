(library (micalog verilog)
  (export
    name->verilog
    edge->verilog
    event->verilog
    value->verilog
    module->verilog
    expr->verilog
    input->verilog
    output->verilog
    declaration-instrs->verilog
    declaration-declarations->verilog
    init->verilog)

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
        #`(%%module
          (
            #,(name->verilog #'name)
            #,@(map input->verilog (syntaxes input ...))
            #,@(map output->verilog (syntaxes output ...)))
          #,@(flatten
            (map
              declaration->verilog-declarations
              (syntaxes internal ...)))
          #,@(flatten
            (map
              (partial declaration->verilog-instrs #t)
              (syntaxes internal ...)))
          #,@(map output-assign->verilog (syntaxes output ...))))))

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

  (define (declaration->verilog-instrs $top-level? $declaration)
    (syntax-case $declaration (%on)
      ((%on body ...)
        (on->verilogs $top-level? #'(%on body ...)))
      (instr
        (instr->verilogs #'instr))))

  (define (on->verilogs $top-level? $on)
    (syntax-case $on (%on)
      ((%on name process)
        (opt->list
          (and $top-level?
            (process->verilog #'name #'process))))
      ((%on name process other-process)
        (opposite-processes? #'process #'other-process)
        (non-false-list
          (and $top-level?
            (process->verilog #'name #'process))
          (and $top-level?
            (process->verilog #'name #'other-process))))))

  (define (process->verilog $name $process)
    (syntax-case $process (%init %update)
      ((edge (%init init ...) (%update update ...))
        (always->verilog $name #'edge
          (syntaxes update ...)))))

  (define (always->verilog $name $edge $instrs)
    #`(%%always
      (
        #,(edge->verilog $edge)
        #,(name->verilog $name))
      #,@(flatten
        (map
          (partial declaration->verilog-instrs #f)
          $instrs))))

  (define (instr->verilogs $instr)
    (syntax-case $instr (%set)
      ((%set type name expr)
        (list
          #`(%%set!
            #,(name->verilog #'name)
            #,(expr->verilog #'expr))))
      (_ ; TODO: Detect assign here
        (list))))

  (define (type->verilog? $type)
    (syntax-case $type ()
      (number (positive-integer? (datum number))
        (and
          (not (= (datum number) 1))
          #`(#,(- (datum number) 1) %%to 0)))))

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

  (define (name->verilog $name)
    (syntax-case $name ()
      (name (identifier? #'name)
        #'name)))

  (define (declaration->verilog-declarations $declaration)
    (syntax-case $declaration (%on %wire)
      ((%on name process)
        (process->verilog-declarations #'process))
      ((%on name process other-process)
        (opposite-processes? #'process #'other-process)
        (append
          (process->verilog-declarations #`process)
          (process->verilog-declarations #`other-process)))
      ((%wire type name expr)
        (list
          (wire->verilog #'name #'type)
          (assign->verilog #'name #'expr)))
      (_
        ; TODO: Implement assign and wire
        (list))))

  (define (output-assign->verilog $output)
    (syntax-case $output ()
      ((name type expr)
        (assign->verilog #'name #'expr))))

  (define (process->verilog-declarations $process)
    (syntax-case $process (%init %update)
      ((edge (%init init ...) (%update update ...))
        (append
          (map init->verilog (syntaxes init ...))
          (flatten
            (map
              (partial declaration->verilog-declarations)
              (syntaxes update ...)))))))

  (define (init->verilog $init)
    (syntax-case $init (%register)
      ((%register type name)
        #`(%%reg
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))))

  (define (declaration-declarations->verilog $declaration)
    #`(#,@(declaration->verilog-declarations $declaration)))

  (define (declaration-instrs->verilog $declaration)
    (list->syntax
      (declaration->verilog-instrs
        #t
        $declaration)))
)
