(library (micac expand)
  (export
    expand-expr
    expand-lhs
    expand-instr
    expand-instrs)
  (import (micascheme) (syntax scope) (micac syntax) (micac scoped))

  (define (scoped+syntax $scoped $syntax)
    (scoped-map
      ($syntaxes $scoped)
      (push $syntaxes $syntax)))

  (define (expand-instrs $scope $syntax)
    (fluent $scope
      (scoped (stack))
      (scoped+instrs $syntax)
      (scoped-value)
      (reverse)))

  (define (expand-instr $scope $syntax)
    (force-single (expand-instrs $scope #`(#,$syntax))))

  (define (scoped+instrs $scoped $instrs)
    (syntax-case $instrs (defer break-if)
      (() $scoped)
      (((defer deferred ...) body ...)
        (scoped+instrs $scoped
          #`(body ... deferred ...)))
      (((break-if expr break-body ...) body ...)
        (scoped+instr $scoped
          #`(if expr
            (then break-body ...)
            (else body ...))))
      (((id arg ...) body ...) (identifier? #'id)
        (switch (scoped-transformer $scoped #'id)
          ((false? _)
            (scoped+instrs
              (scoped+instr $scoped #'(id arg ...))
              #'(body ...)))
          ((else $transformer)
            (scoped+instrs $scoped
              #`(
                #,@(unbegin-syntaxes
                  (scoped-transform $scoped $transformer #'(id arg ...)))
              body ...)))))
      ((instr instrs ...)
        (scoped+instrs
          (scoped+instr $scoped #'instr)
          #'(instrs ...)))))

  (define (declarator->scoped-syntax $scope $declarator)
    (syntax-case $declarator (*)
      (id (identifier? #'id)
        (lets
          ((pair $scope $identifier) (scope-gen $scope #'id))
          (scoped $scope $identifier)))
      ((* decl)
        (scoped-map
          ($scoped-decl (declarator->scoped-syntax $scope #'decl))
          #`(* #,$scoped-decl)))
      ((* decl expr)
        (lets
          ($expr (expand-expr $scope #'expr))
          (scoped-map
            ($scoped-decl (declarator->scoped-syntax $scope #'decl))
            #`(* #,$scoped-decl #,$expr))))))

  (define (scoped+instr $scoped $instr)
    (lets
      ((scoped $scope $syntaxes) $scoped)
      (syntax-case $instr (extern macro begin var const if when cond while then else set return)
        ((extern id)
          (scoped+ $scoped (identifier id) #f))
        ((macro (id param ...) body ...)
          (scoped+ $scoped (identifier id)
            (lambda ($syntax)
              (lambda ($scope)
                (syntax-case $syntax ()
                  ((_ arg ...)
                    (syntax-subst
                      #'(param ...)
                      #'(arg ...)
                      #'(begin body ...))))))))
        ((macro id expr)
          (scoped+ $scoped (identifier id)
            (lambda ($syntax)
              (lambda ($scope)
                #'expr))))
        ((begin instr ...)
          (scoped+syntax $scoped
            #`(begin
              #,@(expand-instrs $scope #'(instr ...)))))
        ((var type decl)
          (scoped-map
            ($decl (declarator->scoped-syntax $scope #'decl))
            (push $syntaxes
              #`(var type #,$decl))))
        ((var type decl expr)
          (scoped-map
            ($decl (declarator->scoped-syntax $scope #'decl))
            (push $syntaxes
              #`(var type #,$decl #,(expand-expr $scope #'expr)))))
        ((const type decl expr)
          (scoped-map
            ($decl (declarator->scoped-syntax $scope #'decl))
            (push $syntaxes
              #`(const type #,$decl #,(expand-expr $scope #'expr)))))
        ((if expr (then then-body ...) (else else-body ...))
          (scoped+syntax $scoped
            #`(if
              #,(expand-expr $scope #'expr)
              (then #,@(expand-instrs $scope #'(then-body ...)))
              (else #,@(expand-instrs $scope #'(else-body ...))))))
        ((when expr body ...)
          (scoped+syntax $scoped
            #`(when
              #,(expand-expr $scope #'expr)
              #,@(expand-instrs $scope #'(body ...)))))
        ((cond clause clause* ... (else else-body ...))
          (scoped+syntax $scoped
            #`(cond
              #,@(map (partial expand-clause $scope) (syntaxes clause clause* ...))
              (else #,@(expand-instrs $scope #'(else-body ...))))))
        ((cond clause clause* ...)
          (scoped+syntax $scoped
            #`(cond
              #,@(map (partial expand-clause $scope) (syntaxes clause clause* ...)))))
        ((while expr body ...)
          (scoped+syntax $scoped
            #`(while
              #,(expand-expr $scope #'expr)
              #,@(expand-instrs $scope #'(body ...)))))
        ((set lhs expr)
          (scoped+syntax $scoped
            #`(set
              #,(expand-lhs $scope #'lhs)
              #,(expand-expr $scope #'expr))))
        ((set lhs op expr)
          (scoped+syntax $scoped
            #`(set
              #,(expand-lhs $scope #'lhs)
              op
              #,(expand-expr $scope #'expr))))
        ((return expr)
          (scoped+syntax $scoped
            #`(return #,(expand-expr $scope #'expr))))
        ((id arg ...) (identifier? #'id)
          (switch (scope-ref $scope #'id)
            ((identifier? $identifier)
              (scoped+syntax $scoped
                #`(
                  #,$identifier
                  #,@(map
                    (partial expand-expr $scope)
                    (syntaxes arg ...)))))
            ((else $transformer)
              (scoped+instrs $scoped
                #`(
                  #,@(unbegin-syntaxes
                    (scope-transform $scope $transformer $instr))))))))))

  (define (expand-clause $scope $clause)
    (syntax-case $clause ()
      ((expr body ...)
        #`(
          #,(expand-expr $scope #'expr)
          #,@(expand-instrs $scope #'(body ...))))))

  (define (expand-expr $scope $expr)
    (syntax-case $expr
      (
        cast
        = not
        < <= > >=
        + - * div
        not and or
        bitwise-not bitwise-and bitwise-ior bitwise-xor
        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        if ref &ref)
      ((cast type rhs)
        #`(cast type #,(expand-expr $scope #'rhs)))
      ((= arg ...)
        (expand-op2 $scope $expr boolean? =))
      ((< arg ...)
        (expand-op2 $scope $expr number? <))
      ((<= arg ...)
        (expand-op2 $scope $expr number? <=))
      ((> arg ...)
        (expand-op2 $scope $expr number? >))
      ((>= arg ...)
        (expand-op2 $scope $expr number? >=))
      ((+ arg ...)
        (expand-op2-fold-default $scope $expr #'0 number? +))
      ((- arg ...)
        (expand-op2-fold $scope $expr number? -))
      ((* arg ...)
        (expand-op2-fold-default $scope $expr #'1 number? *))
      ((div arg ...)
        (expand-op2 $scope $expr number? div))
      ((and arg ...)
        (expand-op2-fold-default $scope $expr #'#t boolean? and-proc))
      ((or arg ...)
        (expand-op2-fold-default $scope $expr #'#f boolean? or-proc))
      ((not arg ...)
        (expand-op1 $scope $expr boolean? not))
      ((bitwise-and arg ...)
        (expand-op2-fold-default $scope $expr #'-1 integer? bitwise-and))
      ((bitwise-ior arg ...)
        (expand-op2-fold-default $scope $expr #'0 integer? bitwise-ior))
      ((bitwise-xor arg ...)
        (expand-op2-fold-default $scope $expr #'0 integer? bitwise-xor))
      ((bitwise-not arg ...)
        (expand-op1 $scope $expr integer? bitwise-not))
      ((bitwise-arithmetic-shift-left arg ...)
        (expand-op2 $scope $expr integer? bitwise-arithmetic-shift-left))
      ((bitwise-arithmetic-shift-right arg ...)
        (expand-op2 $scope $expr integer? bitwise-arithmetic-shift-right))
      ((ref var x ...)
        #`(ref
          #,(expand-expr $scope #'var)
          #,@(map (partial expand-accessor $scope) (syntaxes x ...))))
      ((&ref var x ...)
        #`(&ref
          #,(expand-expr $scope #'var)
          #,@(map (partial expand-accessor $scope) (syntaxes x ...))))
      ((if cond then els)
        (lets
          ($cond (expand-expr $scope #'cond))
          ($then (expand-expr $scope #'then))
          ($else (expand-expr $scope #'els))
          (switch (syntax->datum $cond)
            ((boolean? $boolean)
              (if $boolean $then $else))
            ((else $other)
              #`(if #,$cond #,$then #,$else)))))
      ((id arg ...) (identifier? #'id)
        (switch (scope-ref $scope #'id)
          ((identifier? $identifier)
            #`(
              #,$identifier
              #,@(map
                (partial expand-expr $scope)
                (syntaxes arg ...))))
          ((else $transformer)
            (expand-expr $scope
              (unbegin-syntax
                (scope-transform $scope $transformer $expr))))))
      (id (identifier? #'id)
        (switch (scope-ref $scope #'id)
          ((identifier? $identifier)
            $identifier)
          ((else $transformer)
            (expand-expr $scope
              (unbegin-syntax
                (scope-transform $scope $transformer $expr))))))
      (other #'other)))

  (define (expand-op1 $scope $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a)
        (lets
          ($a (expand-expr $scope #'a))
          ($datum (syntax->datum $a))
          (if ($test? $datum)
            (datum->syntax #'op ($proc $datum))
            #`(op #,$a))))))

  (define (expand-op2 $scope $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a b)
        (lets
          ($a (expand-expr $scope #'a))
          ($b (expand-expr $scope #'b))
          ($datum-a (syntax->datum $a))
          ($datum-b (syntax->datum $b))
          (if (and ($test? $datum-a) ($test? $datum-b))
            (datum->syntax #'op ($proc $datum-a $datum-b))
            #`(op #,$a #,$b))))))

  (define (expand-op2-fold $scope $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a)
        (expand-op1 $scope $syntax $test? $proc))
      ((op a b)
        (expand-op2 $scope $syntax $test? $proc))
      ((op a b c cs ...)
        (expand-op2-fold $scope
          #`(op (op a b) c cs ...)
          $test? $proc))))

  (define (expand-op2-fold-default $scope $syntax $default $test? $proc)
    (syntax-case $syntax ()
      ((op) $default)
      ((op a)
        (lets
          ($a (expand-expr $scope #'a))
          ($datum (syntax->datum $a))
          (if ($test? $datum)
            (datum->syntax #'op ($proc $datum))
            $a)))
      (_ (expand-op2-fold $scope $syntax $test? $proc))))

  (define (expand-lhs $scope $lhs)
    (syntax-case $lhs ()
      ((expr accessor accessors ...)
        #`(
          #,(expand-expr $scope #'expr)
          #,@(map
            (partial expand-accessor $scope)
            (syntaxes accessor accessors ...))))
      ((id)
        (expand-lhs $scope #'id))
      (id (identifier? #'id)
        (expand-identifier $scope #'id))))

  (define (expand-identifier $scope $id)
    (switch (scope-ref $scope $id)
      ((identifier? $identifier)
        $identifier)
      ((else $transformer)
        $id)))

  (define (expand-accessor $scope $accessor)
    (syntax-case $accessor (*)
      ((expr) #`(#,(expand-expr $scope #'expr)))
      (* #'*)
      (field (identifier? #'field) #'field)))
)
