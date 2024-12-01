(library (micalog scheme transformer)
  (export
    module->scheme
    register->scheme
    instruction->scheme
    expr->scheme
    value->scheme)
  (import
    (micascheme)
    (micalog core utils)
    (prefix (micalog keywords) %)
    (micalog scheme keywords))

  ; Requirements:
  ; - module with explicit previous-clock and clock names
  ; - fully typed
  ; - "on" statement with explicit previous value
  ; - declarations before instructions
  (define (module->scheme $module)
    (syntax-case $module (%module)
      ((%module name statement ...)
        (lets
          ($statements (syntaxes statement ...))
          ($registers (declaration-syntaxes-of %register statement ...))
          ($instructions
            (filter
              (lambda ($statement)
                (and
                  (not (declaration-kind-of? #'%input $statement))
                  (not (declaration-kind-of? #'%register $statement))))
              (syntaxes statement ...)))
          ($clock-input? (kind-name-find-statement #'%input #'1 #'clock $statements))
          ($reset?-input? (kind-name-find-statement #'%input #'1 #'reset? $statements))
          ($exit?-output? (kind-name-find-statement #'%output #'1 #'exit? $statements))
          #`(let ()
            #,@(opt->list (and $clock-input? #`(define clock 0)))
            #,@(opt->list (and $reset?-input? #`(define reset-counter 32)))
            #,@(opt->list (and $reset?-input? #`(define reset? 1)))
            #,@(map register->scheme $registers)
            (do
              ((exit? 0 exit?))
              ((= exit? 1) (void))
              #,@(opt->list (and $clock-input? #`(set! clock (xor clock 1))))
              #,@(opt->list
                (and $reset?-input?
                  #`(if (= reset-counter 0)
                    (set! reset? 0)
                    (set! reset-counter (- reset-counter 1)))))
              (let ()
                #,@(map instruction->scheme $instructions))))))))

  (define (register->scheme $statement)
    (syntax-case $statement (%register)
      ((%register type name)
        #`(define #,(name->scheme #'name) 0))))

  (define (instruction->scheme $statement)
    (syntax-case $statement (%output %wire %set %log %on %cond %else)
      ((%output type name expr)
        #`(define
          #,(name->scheme #'name)
          #,(expr->scheme #'expr)))
      ((%wire type name expr)
        #`(define
          #,(name->scheme #'name)
          #,(expr->scheme #'expr)))
      ((%set type name expr)
        #`(set!
          #,(name->scheme #'name)
          #,(expr->scheme #'expr)))
      ((%log label type expr)
        #`(display
          (format "~a: ~a\\n"
            #,(expr->scheme #'expr))))
      ((%cond clause ... (%else els ...))
        #`(cond
          #,@(map clause->scheme (syntaxes clause ...))
          (else #,@(map instruction->scheme (syntaxes els ...)))))
      ((%cond clause-1 clause ...)
        #`(cond
          #,@(map clause->scheme (syntaxes clause-1 clause ...))))
      ((%on (edge previous-name name) statement ...)
        #`(when (not (= #,(name->scheme #'previous-name) #,(name->scheme #'name)))
          (when (= #,(name->scheme #'name) #,(edge->scheme #'edge))
            #,@(map instruction->scheme (syntaxes statement ...)))))))

  (define (clause->scheme $clause)
    (syntax-case $clause ()
      ((cond instruction ...)
        #`(
          #,(expr->scheme #'cond)
          #,@(map instruction->scheme (syntaxes instruction ...))))))

  (define (edge->scheme $edge)
    (syntax-case $edge (%posedge %negedge)
      (%posedge #'1)
      (%negedge #'0)))

  (define (value->scheme $value)
    (syntax-case $value ()
      (id (identifier? #'id)
        (name->scheme #'id))
      (integer (integer? (datum integer))
        #'integer)))

  (define (expr->scheme $expr)
    (syntax-case $expr (%= %!= %< %<= %> %>= %append %take %drop %wrap+ %wrap- %wrap* %+ %- %* %and %or %xor %nand %nor %xnor %not %if)
      ((%= type lhs rhs)
        #`(=
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%!= type lhs rhs)
        #`(not
          (=
            #,(expr->scheme #'lhs)
            #,(expr->scheme #'rhs))))
      ((%< type lhs rhs)
        #`(<
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%<= type lhs rhs)
        #`(<=
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%> type lhs rhs)
        #`(>
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%>= type lhs rhs)
        #`(>=
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%append (lhs-type lhs))
        (expr->scheme #'lhs))
      ((%append (lhs-type lhs) (rhs-type rhs))
        #`(bitwise-ior
          (bitwise-arithmetic-shift-left
            #,(expr->scheme #'lhs)
            #,(type-size #'rhs-type))
          #,(expr->scheme #'rhs)))
      ((%append (lhs-type lhs-expr) rhs ...)
        (fold-left
          scheme-append
          (expr->scheme #'lhs-expr)
          (syntaxes rhs ...)))
      ((%take type rhs size)
        (type-scheme-mask #'size
          (expr->scheme #'rhs)))
      ((%drop type rhs drop)
        #`(bitwise-arithmetic-shift-right
          #,(expr->scheme #'rhs)
          drop))
      ((%wrap+ type lhs rhs)
        (type-scheme-mask #'type
          #`(+
            #,(expr->scheme #'lhs)
            #,(expr->scheme #'rhs))))
      ((%wrap- type lhs rhs)
        (type-scheme-mask #'type
          #`(-
            #,(expr->scheme #'lhs)
            #,(expr->scheme #'rhs))))
      ((%wrap- type rhs)
        (type-scheme-mask #'type
          #`(-
            #,(expr->scheme #'rhs))))
      ((%wrap* type lhs rhs)
        (type-scheme-mask #'type
          #`(*
            #,(expr->scheme #'lhs)
            #,(expr->scheme #'rhs))))
      ((%+ type lhs rhs)
        #`(+
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%- type lhs rhs)
        #`(-
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%- type rhs)
        #`(-
          #,(expr->scheme #'rhs)))
      ((%* type lhs rhs)
        #`(*
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%and type lhs rhs)
        #`(
          #,(if (type-boolean? #'type) #'and #'bitwise-and)
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%or type lhs rhs)
        #`(
          #,(if (type-boolean? #'type) #'or #'bitwise-ior)
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%xor type lhs rhs)
        #`(
          bitwise-xor
          #,(expr->scheme #'lhs)
          #,(expr->scheme #'rhs)))
      ((%nand type lhs rhs)
        (expr->scheme #`(%not type (%and type lhs rhs))))
      ((%nor type lhs rhs)
        (expr->scheme #`(%not type (%or type lhs rhs))))
      ((%xnor type lhs rhs)
        (expr->scheme #`(%not type (%xor type lhs rhs))))
      ((%not type rhs)
        (type-scheme-mask #'type
          #`(
            #,(if (type-boolean? #'type) #'not #'bitwise-not)
            #,(expr->scheme #'rhs))))
      ((%if type cond true false)
        #`(if
          #,(expr->scheme #'cond)
          #,(expr->scheme #'true)
          #,(expr->scheme #'false)))
      (other
        (value->scheme #'other))))

  (define (scheme-append $lhs-micac $rhs)
    (syntax-case $rhs ()
      ((type expr)
        #`(bitwise-ior
          (bitwise-arithmetic-shift-left
            #,$lhs-micac
            #,(type-size #'type))
          #,(expr->scheme #'expr)))))

  (define (boolean->number $boolean?)
    #`(if #,$boolean? 1 0))

  (define (reg-value->scheme? $reg)
    (syntax-case $reg (%reg)
      ((%reg) #f)
      ((%reg expr) (expr->scheme #'expr))))

  (define (name->scheme $id) $id)

  (define (size-scheme-mask $size $micac)
    #`(bitwise-and
      #,$micac
      #,(literal->syntax
        (- (bitwise-arithmetic-shift-left 1 (syntax->datum $size)) 1))))

  (define (type-boolean? $type)
    (syntax-case $type ()
      (1 #t)
      (_ #f)))

  (define (type-scheme-mask $type $micac)
    (size-scheme-mask (type-size $type) $micac))

  (define (kind-name-find-statement $kind $type $name $statements)
    (find
      (lambda ($statement)
        (syntax-case $statement ()
          ((kind type name body ...)
            (and
              (free-identifier=? #'kind $kind)
              (free-identifier=? #'name $name)
              (or
                (syntax=? #'type $type)
                (syntax-error $statement
                  (format "invalid type ~a, expected ~a in"
                    (syntax->datum #'type)
                    (syntax->datum $type))))
              $statement))))
       $statements))
)
