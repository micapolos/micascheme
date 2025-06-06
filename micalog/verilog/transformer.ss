(library (micalog verilog transformer)
  (export
    name->verilog
    edge->verilog
    value->verilog
    module->verilog
    expr->verilog
    input->verilog
    output->verilog
    instr->verilog
    declaration->verilog)

  (import
    (micascheme)
    (micalog core utils)
    (prefix (micalog keywords) %)
    (prefix (verilog keywords) %%))

  (define (module->verilog $module)
    (syntax-case $module (%module %input %internal %output)
      ((%module name body ...)
        #`(%%module
          (
            #,(name->verilog #'name)
            #,@(map input->verilog (declaration-syntaxes-of %input body ...))
            #,@(map output->verilog (declaration-syntaxes-of %output body ...)))
          #,@(filter-opts (map declaration->verilog? (syntaxes body ...)))
          #,@(filter-opts (map instr->verilog? (syntaxes body ...)))))))

  (define (input->verilog $input)
    (syntax-case $input (%input)
      ((%input type name)
        #`(%%input
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))))

  (define (output->verilog $output)
    (syntax-case $output (%output)
      ((%output type name)
        #`(%%output
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))))

  (define (declaration->verilog? $declaration)
    (syntax-case $declaration (%wire %register %assign %on)
      ((%wire type name)
        #`(%%wire
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))
      ((%register type name)
        #`(%%reg
          #,@(opt->list (type->verilog? #'type))
          #,(name->verilog #'name)))
      ((%assign type name expr)
        #`(%%assign
          #,(name->verilog #'name)
          #,(expr->verilog #'expr)))
      ((%on (edge name) body ...)
        #`(%%always
          (#,(edge->verilog #'edge) #,(name->verilog #'name))
          #,@(filter-opts (map instr->verilog? (syntaxes body ...)))))
      (_ #f)))

  (define (declaration->verilog $declaration)
    (or
      (declaration->verilog? $declaration)
      (syntax-error $declaration)))

  (define (instr->verilog? $declaration)
    (syntax-case $declaration (%set %log %cond %else %log %display)
      ((%set type name expr)
        #`(%%set!
          #,(name->verilog #'name)
          #,(expr->verilog #'expr)))
      ((%log label type expr)
        #`(%%display
          #,(literal->syntax (format "~a %d" (syntax->datum (identifier label))))
          #,(expr->verilog #'expr)))
      ((%cond clause ... (%else body ...))
        #`(%%cond
          #,@(map clause->verilog (syntaxes clause ...))
          (%%else #,@(filter-opts (map instr->verilog? (syntaxes body ...))))))
      ((%cond clause-1 clause ...)
        #`(%%cond
          #,@(map clause->verilog (syntaxes clause-1 clause ...))))
      (_ #f)))

  (define (clause->verilog $clause)
    (syntax-case $clause ()
      ((cond body ...)
        #`(
          #,(expr->verilog #'cond)
          #,@(filter-opts (map instr->verilog? (syntaxes body ...)))))))

  (define (instr->verilog $instr)
    (or
      (instr->verilog? $instr)
      (syntax-error $instr)))

  (define (type->verilog? $type)
    (syntax-case $type ()
      (number (positive-integer? (datum number))
        (and
          (not (= (datum number) 1))
          #`(#,(- (datum number) 1) %%to 0)))))

  (define (expr->verilog $expr)
    (syntax-case $expr (%append %take %drop %= %!= %< %<= %> %>= %if %not %and %or %xor %nand %nor %xnor %wrap+ %wrap- %wrap* %+ %- %*)
      ((%append (type expr) ...)
        #`(%%append
          #,@(map expr->verilog (syntaxes expr ...))))
      ((%take type a size)
        (lets
          ($size (datum size))
          #`(%%ref
            #,(expr->verilog #'a)
            (
              #,(literal->syntax (+ $size -1))
              %%to
              0))))
      ((%drop type a drop)
        (lets
          ($drop (datum drop))
          ($mask (datum type))
          #`(%%ref
            #,(expr->verilog #'a)
            (
              #,(literal->syntax (+ $drop $mask -1))
              %%to
              #,(literal->syntax $drop)))))
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
      ; TODO: Select bits from type
      ((%wrap+ type a b)
        (op2->verilog #'%%+ #'a #'b))
      ((%wrap- type a b)
        (op2->verilog #'%%- #'a #'b))
      ((%wrap* type a b)
        (op2->verilog #'%%* #'a #'b))
      ((%wrap- type a)
        (op->verilog #'%%- #'a))
      ((%+ type a b)
        (op2->verilog #'%%+ #'a #'b))
      ((%- type a b)
        (op2->verilog #'%%- #'a #'b))
      ((%* type a b)
        (op2->verilog #'%%* #'a #'b))
      ((%- type a)
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
      #,(expr->verilog $rhs)))

  (define (op2->verilog $op $lhs $rhs)
    #`(
      #,$op
      #,(expr->verilog $lhs)
      #,(expr->verilog $rhs)))

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
