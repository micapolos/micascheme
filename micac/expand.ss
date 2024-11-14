(library (micac expand)
  (export
    expand-expr
    expand-lhs
    expand-instr
    expand-instrs)
  (import (micascheme) (micac env) (micac syntax) (micac compiled))

  (define (compiled+syntax $compiled $syntax)
    (compiled-map
      ($syntaxes $compiled)
      (push $syntaxes $syntax)))

  (define (expand-instrs $env $syntax)
    (fluent $env
      (compiled (stack))
      (compiled+instrs $syntax)
      (compiled-value)
      (reverse)))

  (define (expand-instr $env $syntax)
    (force-single (expand-instrs $env #`(#,$syntax))))

  (define (compiled+instrs $compiled $instrs)
    (syntax-case $instrs (defer break-if)
      (() $compiled)
      (((defer deferred ...) body ...)
        (compiled+instrs $compiled
          #`(body ... deferred ...)))
      (((break-if expr break-body ...) body ...)
        (compiled+instr $compiled
          #`(if expr
            (then break-body ...)
            (else body ...))))
      ((instr instrs ...)
        (compiled+instrs
          (compiled+instr $compiled #'instr)
          #'(instrs ...)))))

  (define (declarator->compiled-syntax $env $declarator)
    (syntax-case $declarator (*)
      (id (identifier? #'id)
        (lets
          ((pair $env $identifier) (env-gen $env #'id))
          (compiled $env $identifier)))
      ((* decl)
        (compiled-map
          ($compiled-decl (declarator->compiled-syntax $env #'decl))
          #`(* #,$compiled-decl)))
      ((* decl expr)
        (lets
          ($expr (expand-expr $env #'expr))
          (compiled-map
            ($compiled-decl (declarator->compiled-syntax $env #'decl))
            #`(* #,$compiled-decl #,$expr))))))

  (define (compiled+instr $compiled $instr)
    (lets
      ((compiled $env $syntaxes) $compiled)
      (syntax-case $instr (extern macro begin var const if when while then else set)
        ((extern id)
          (compiled+ $compiled (identifier id) #f))
        ((macro (id param ...) body ...)
          (compiled+ $compiled (identifier id)
            (lambda ($syntax)
              (lambda ($env)
                (syntax-case $syntax ()
                  ((_ arg ...)
                    (syntax-subst
                      #'(param ...)
                      #'(arg ...)
                      #'(begin body ...))))))))
        ((macro id expr)
          (compiled+ $compiled (identifier id)
            (lambda ($syntax)
              (lambda ($env)
                #'expr))))
        ((begin instr ...)
          (compiled+syntax $compiled
            #`(begin
              #,@(expand-instrs $env #'(instr ...)))))
        ((var type decl)
          (compiled-map
            ($decl (declarator->compiled-syntax $env #'decl))
            (push $syntaxes
              #`(var type #,$decl))))
        ((var type decl expr)
          (compiled-map
            ($decl (declarator->compiled-syntax $env #'decl))
            (push $syntaxes
              #`(var type #,$decl #,(expand-expr $env #'expr)))))
        ((const type decl expr)
          (compiled-map
            ($decl (declarator->compiled-syntax $env #'decl))
            (push $syntaxes
              #`(const type #,$decl #,(expand-expr $env #'expr)))))
        ((if expr (then then-body ...) (else else-body ...))
          (compiled+syntax $compiled
            #`(if
              #,(expand-expr $env #'expr)
              (then #,@(expand-instrs $env #'(then-body ...)))
              (else #,@(expand-instrs $env #'(else-body ...))))))
        ((when expr body ...)
          (compiled+syntax $compiled
            #`(when
              #,(expand-expr $env #'expr)
              #,@(expand-instrs $env #'(body ...)))))
        ((while expr body ...)
          (compiled+syntax $compiled
            #`(while
              #,(expand-expr $env #'expr)
              #,@(expand-instrs $env #'(body ...)))))
        ((set lhs expr)
          (compiled+syntax $compiled
            #`(set
              #,(expand-lhs $env #'lhs)
              #,(expand-expr $env #'expr))))
        ((set lhs op expr)
          (compiled+syntax $compiled
            #`(set
              #,(expand-lhs $env #'lhs)
              op
              #,(expand-expr $env #'expr))))
        ((id arg ...) (identifier? #'id)
          (switch (env-ref $env #'id)
            ((identifier? $identifier)
              (compiled+syntax $compiled
                #`(
                  #,$identifier
                  #,@(map
                    (partial expand-expr $env)
                    (syntaxes arg ...)))))
            ((else $transformer)
              (fold-left
                compiled+instr
                $compiled
                (begin-syntaxes
                  (env-transform $env $transformer $instr)))))))))

  (define (expand-expr $env $expr)
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
        #`(cast type #,(expand-expr $env #'rhs)))
      ((= arg ...)
        (expand-op2 $env $expr boolean? =))
      ((< arg ...)
        (expand-op2 $env $expr number? <))
      ((<= arg ...)
        (expand-op2 $env $expr number? <=))
      ((> arg ...)
        (expand-op2 $env $expr number? >))
      ((>= arg ...)
        (expand-op2 $env $expr number? >=))
      ((+ arg ...)
        (expand-op2-fold-default $env $expr #'0 number? +))
      ((- arg ...)
        (expand-op2-fold $env $expr number? -))
      ((* arg ...)
        (expand-op2-fold-default $env $expr #'1 number? *))
      ((div arg ...)
        (expand-op2 $env $expr number? div))
      ((and arg ...)
        (expand-op2-fold-default $env $expr #'#t boolean? and-proc))
      ((or arg ...)
        (expand-op2-fold-default $env $expr #'#f boolean? or-proc))
      ((not arg ...)
        (expand-op1 $env $expr boolean? not))
      ((bitwise-and arg ...)
        (expand-op2-fold-default $env $expr #'-1 integer? bitwise-and))
      ((bitwise-ior arg ...)
        (expand-op2-fold-default $env $expr #'0 integer? bitwise-ior))
      ((bitwise-xor arg ...)
        (expand-op2-fold-default $env $expr #'0 integer? bitwise-xor))
      ((bitwise-not arg ...)
        (expand-op1 $env $expr integer? bitwise-not))
      ((bitwise-arithmetic-shift-left arg ...)
        (expand-op2 $env $expr integer? bitwise-arithmetic-shift-left))
      ((bitwise-arithmetic-shift-right arg ...)
        (expand-op2 $env $expr integer? bitwise-arithmetic-shift-right))
      ((ref var x ...)
        #`(ref
          #,(expand-expr $env #'var)
          #,@(map (partial expand-accessor $env) (syntaxes x ...))))
      ((&ref var x ...)
        #`(&ref
          #,(expand-expr $env #'var)
          #,@(map (partial expand-accessor $env) (syntaxes x ...))))
      ((if cond then els)
        (lets
          ($cond (expand-expr $env #'cond))
          ($then (expand-expr $env #'then))
          ($else (expand-expr $env #'els))
          (switch (syntax->datum $cond)
            ((boolean? $boolean)
              (if $boolean $then $else))
            ((else $other)
              #`(if #,$cond #,$then #,$else)))))
      ((id arg ...) (identifier? #'id)
        (switch (env-ref $env #'id)
          ((identifier? $identifier)
            #`(
              #,$identifier
              #,@(map
                (partial expand-expr $env)
                (syntaxes arg ...))))
          ((else $transformer)
            (expand-expr $env
              (begin-syntax
                (env-transform $env $transformer $expr))))))
      (id (identifier? #'id)
        (switch (env-ref $env #'id)
          ((identifier? $identifier)
            $identifier)
          ((else $transformer)
            (expand-expr $env
              (begin-syntax
                (env-transform $env $transformer $expr))))))
      (other #'other)))

  (define (expand-op1 $env $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a)
        (lets
          ($a (expand-expr $env #'a))
          ($datum (syntax->datum $a))
          (if ($test? $datum)
            (datum->syntax #'op ($proc $datum))
            #`(op #,$a))))))

  (define (expand-op2 $env $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a b)
        (lets
          ($a (expand-expr $env #'a))
          ($b (expand-expr $env #'b))
          ($datum-a (syntax->datum $a))
          ($datum-b (syntax->datum $b))
          (if (and ($test? $datum-a) ($test? $datum-b))
            (datum->syntax #'op ($proc $datum-a $datum-b))
            #`(op #,$a #,$b))))))

  (define (expand-op2-fold $env $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a)
        (expand-op1 $env $syntax $test? $proc))
      ((op a b)
        (expand-op2 $env $syntax $test? $proc))
      ((op a b c cs ...)
        (expand-op2-fold $env
          #`(op (op a b) c cs ...)
          $test? $proc))))

  (define (expand-op2-fold-default $env $syntax $default $test? $proc)
    (syntax-case $syntax ()
      ((op) $default)
      ((op a)
        (lets
          ($a (expand-expr $env #'a))
          ($datum (syntax->datum $a))
          (if ($test? $datum)
            (datum->syntax #'op ($proc $datum))
            $a)))
      (_ (expand-op2-fold $env $syntax $test? $proc))))

  (define (expand-lhs $env $lhs)
    (syntax-case $lhs ()
      ((expr accessor accessors ...)
        #`(
          #,(expand-expr $env #'expr)
          #,@(map
            (partial expand-accessor $env)
            (syntaxes accessor accessors ...))))
      ((id)
        (expand-lhs $env #'id))
      (id (identifier? #'id)
        (expand-identifier $env #'id))))

  (define (expand-identifier $env $id)
    (switch (env-ref $env $id)
      ((identifier? $identifier)
        $identifier)
      ((else $transformer)
        $id)))

  (define (expand-accessor $env $accessor)
    (syntax-case $accessor (*)
      ((expr) #`(#,(expand-expr $env #'expr)))
      (* #'*)
      (field (identifier? #'field) #'field)))
)
