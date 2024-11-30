(library (micac expand)
  (export
    expand-expr
    expand-lhs
    expand-instr
    expand-instrs
    expand-top-level)
  (import (micascheme) (syntax lookup) (micac keywords) (syntax scoped))

  (define (scoped+syntax $scoped $syntax)
    (scoped-map
      ($syntaxes $scoped)
      (push $syntaxes $syntax)))

  (define (expand-top-level $lookup $syntax)
    (syntax-case $syntax (include)
      ((type (name param ...) body ...)
        (lets
          ($scoped-params
            (fold-left
              scoped-syntaxes+param
              (scoped $lookup (stack))
              (syntaxes param ...)))
          #`(type (name #,@(reverse (scoped-value $scoped-params)))
            #,@(expand-instrs
              (scoped-scope $scoped-params)
              #'(body ...)))))
      ((include x ...)
        $syntax)))

  (define (scoped-syntaxes+param $scoped $param)
    (parameterize ((lookup-gen? #f))
      (syntax-case $param ()
        ((type declarator)
          (lets
            ($scoped-declarator (declarator->scoped-syntax (scoped-scope $scoped) #'declarator))
            (scoped
              (scoped-scope $scoped-declarator)
              (push
                (scoped-value $scoped)
                #`(type #,(scoped-value $scoped-declarator)))))))))

  (define (expand-instrs $lookup $syntax)
    (fluent $lookup
      (scoped (stack))
      (scoped+instrs $syntax)
      (scoped-value)
      (reverse)))

  (define (expand-instr $lookup $syntax)
    (force-single (expand-instrs $lookup #`(#,$syntax))))

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

  (define (declarator->scoped-syntax $lookup $declarator)
    (syntax-case $declarator (*)
      (id (identifier? #'id)
        (lets
          ((pair $lookup $identifier) (lookup-gen $lookup #'id))
          (scoped $lookup $identifier)))
      ((* decl)
        (scoped-map
          ($scoped-decl (declarator->scoped-syntax $lookup #'decl))
          #`(* #,$scoped-decl)))
      ((* decl expr)
        (lets
          ($expr (expand-expr $lookup #'expr))
          (scoped-map
            ($scoped-decl (declarator->scoped-syntax $lookup #'decl))
            #`(* #,$scoped-decl #,$expr))))))

  (define (scoped+instr $scoped $instr)
    (lets
      ((scoped $lookup $syntaxes) $scoped)
      (syntax-case $instr (extern macro begin var const if when cond while then else set return)
        ((extern id)
          (scoped+ $scoped (identifier id) #f))
        ((macro (id param ...) body ...)
          (scoped+ $scoped (identifier id)
            (lambda ($syntax)
              (lambda ($lookup)
                (syntax-case $syntax ()
                  ((_ arg ...)
                    (syntax-subst
                      #'(param ...)
                      #'(arg ...)
                      #'(begin body ...))))))))
        ((macro id expr)
          (scoped+ $scoped (identifier id)
            (lambda ($syntax)
              (lambda ($lookup)
                #'expr))))
        ((begin instr ...)
          (scoped+syntax $scoped
            #`(begin
              #,@(expand-instrs $lookup #'(instr ...)))))
        ((var type decl)
          (scoped-map
            ($decl (declarator->scoped-syntax $lookup #'decl))
            (push $syntaxes
              #`(var type #,$decl))))
        ((var type decl expr)
          (scoped-map
            ($decl (declarator->scoped-syntax $lookup #'decl))
            (push $syntaxes
              #`(var type #,$decl #,(expand-expr $lookup #'expr)))))
        ((const type decl expr)
          (scoped-map
            ($decl (declarator->scoped-syntax $lookup #'decl))
            (push $syntaxes
              #`(const type #,$decl #,(expand-expr $lookup #'expr)))))
        ((if expr (then then-body ...) (else else-body ...))
          (scoped+syntax $scoped
            #`(if
              #,(expand-expr $lookup #'expr)
              (then #,@(expand-instrs $lookup #'(then-body ...)))
              (else #,@(expand-instrs $lookup #'(else-body ...))))))
        ((when expr body ...)
          (scoped+syntax $scoped
            #`(when
              #,(expand-expr $lookup #'expr)
              #,@(expand-instrs $lookup #'(body ...)))))
        ((cond clause clause* ... (else else-body ...))
          (scoped+syntax $scoped
            #`(cond
              #,@(map (partial expand-clause $lookup) (syntaxes clause clause* ...))
              (else #,@(expand-instrs $lookup #'(else-body ...))))))
        ((cond clause clause* ...)
          (scoped+syntax $scoped
            #`(cond
              #,@(map (partial expand-clause $lookup) (syntaxes clause clause* ...)))))
        ((while expr body ...)
          (scoped+syntax $scoped
            #`(while
              #,(expand-expr $lookup #'expr)
              #,@(expand-instrs $lookup #'(body ...)))))
        ((set lhs expr)
          (scoped+syntax $scoped
            #`(set
              #,(expand-lhs $lookup #'lhs)
              #,(expand-expr $lookup #'expr))))
        ((set lhs op expr)
          (scoped+syntax $scoped
            #`(set
              #,(expand-lhs $lookup #'lhs)
              op
              #,(expand-expr $lookup #'expr))))
        ((return expr)
          (scoped+syntax $scoped
            #`(return #,(expand-expr $lookup #'expr))))
        ((id arg ...) (identifier? #'id)
          (switch ($lookup #'id)
            ((identifier? $identifier)
              (scoped+syntax $scoped
                #`(
                  #,$identifier
                  #,@(map
                    (partial expand-expr $lookup)
                    (syntaxes arg ...)))))
            ((else $transformer)
              (scoped+instrs $scoped
                #`(
                  #,@(unbegin-syntaxes
                    (lookup-transform $lookup $transformer $instr))))))))))

  (define (expand-clause $lookup $clause)
    (syntax-case $clause ()
      ((expr body ...)
        #`(
          #,(expand-expr $lookup #'expr)
          #,@(expand-instrs $lookup #'(body ...))))))

  (define (expand-expr $lookup $expr)
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
        #`(cast type #,(expand-expr $lookup #'rhs)))
      ((= arg ...)
        (expand-op2 $lookup $expr boolean? =))
      ((< arg ...)
        (expand-op2 $lookup $expr number? <))
      ((<= arg ...)
        (expand-op2 $lookup $expr number? <=))
      ((> arg ...)
        (expand-op2 $lookup $expr number? >))
      ((>= arg ...)
        (expand-op2 $lookup $expr number? >=))
      ((+ arg ...)
        (expand-op2-fold-default $lookup $expr #'0 number? +))
      ((- arg ...)
        (expand-op2-fold $lookup $expr number? -))
      ((* arg ...)
        (expand-op2-fold-default $lookup $expr #'1 number? *))
      ((div arg ...)
        (expand-op2 $lookup $expr number? div))
      ((and arg ...)
        (expand-op2-fold-default $lookup $expr #'#t boolean? and-proc))
      ((or arg ...)
        (expand-op2-fold-default $lookup $expr #'#f boolean? or-proc))
      ((not arg ...)
        (expand-op1 $lookup $expr boolean? not))
      ((bitwise-and arg ...)
        (expand-op2-fold-default $lookup $expr #'-1 integer? bitwise-and))
      ((bitwise-ior arg ...)
        (expand-op2-fold-default $lookup $expr #'0 integer? bitwise-ior))
      ((bitwise-xor arg ...)
        (expand-op2-fold-default $lookup $expr #'0 integer? bitwise-xor))
      ((bitwise-not arg ...)
        (expand-op1 $lookup $expr integer? bitwise-not))
      ((bitwise-arithmetic-shift-left arg ...)
        (expand-op2 $lookup $expr integer? bitwise-arithmetic-shift-left))
      ((bitwise-arithmetic-shift-right arg ...)
        (expand-op2 $lookup $expr integer? bitwise-arithmetic-shift-right))
      ((ref var x ...)
        #`(ref
          #,(expand-expr $lookup #'var)
          #,@(map (partial expand-accessor $lookup) (syntaxes x ...))))
      ((&ref var x ...)
        #`(&ref
          #,(expand-expr $lookup #'var)
          #,@(map (partial expand-accessor $lookup) (syntaxes x ...))))
      ((if cond then els)
        (lets
          ($cond (expand-expr $lookup #'cond))
          ($then (expand-expr $lookup #'then))
          ($else (expand-expr $lookup #'els))
          (switch (syntax->datum $cond)
            ((boolean? $boolean)
              (if $boolean $then $else))
            ((else $other)
              #`(if #,$cond #,$then #,$else)))))
      ((id arg ...) (identifier? #'id)
        (switch (lookup-ref $lookup #'id)
          ((identifier? $identifier)
            #`(
              #,$identifier
              #,@(map
                (partial expand-expr $lookup)
                (syntaxes arg ...))))
          ((else $transformer)
            (expand-expr $lookup
              (unbegin-syntax
                (lookup-transform $lookup $transformer $expr))))))
      (id (identifier? #'id)
        (switch (lookup-ref $lookup #'id)
          ((identifier? $identifier)
            $identifier)
          ((else $transformer)
            (expand-expr $lookup
              (unbegin-syntax
                (lookup-transform $lookup $transformer $expr))))))
      (other #'other)))

  (define (expand-op1 $lookup $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a)
        (lets
          ($a (expand-expr $lookup #'a))
          ($datum (syntax->datum $a))
          (if ($test? $datum)
            (datum->syntax #'op ($proc $datum))
            #`(op #,$a))))))

  (define (expand-op2 $lookup $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a b)
        (lets
          ($a (expand-expr $lookup #'a))
          ($b (expand-expr $lookup #'b))
          ($datum-a (syntax->datum $a))
          ($datum-b (syntax->datum $b))
          (if (and ($test? $datum-a) ($test? $datum-b))
            (datum->syntax #'op ($proc $datum-a $datum-b))
            #`(op #,$a #,$b))))))

  (define (expand-op2-fold $lookup $syntax $test? $proc)
    (syntax-case $syntax ()
      ((op a)
        (expand-op1 $lookup $syntax $test? $proc))
      ((op a b)
        (expand-op2 $lookup $syntax $test? $proc))
      ((op a b c cs ...)
        (expand-op2-fold $lookup
          #`(op (op a b) c cs ...)
          $test? $proc))))

  (define (expand-op2-fold-default $lookup $syntax $default $test? $proc)
    (syntax-case $syntax ()
      ((op) $default)
      ((op a)
        (lets
          ($a (expand-expr $lookup #'a))
          ($datum (syntax->datum $a))
          (if ($test? $datum)
            (datum->syntax #'op ($proc $datum))
            $a)))
      (_ (expand-op2-fold $lookup $syntax $test? $proc))))

  (define (expand-lhs $lookup $lhs)
    (syntax-case $lhs ()
      ((expr accessor accessors ...)
        #`(
          #,(expand-expr $lookup #'expr)
          #,@(map
            (partial expand-accessor $lookup)
            (syntaxes accessor accessors ...))))
      ((id)
        (expand-lhs $lookup #'id))
      (id (identifier? #'id)
        (expand-identifier $lookup #'id))))

  (define (expand-identifier $lookup $id)
    (switch ($lookup $id)
      ((identifier? $identifier)
        $identifier)
      ((else $transformer)
        $id)))

  (define (expand-accessor $lookup $accessor)
    (syntax-case $accessor (*)
      ((expr) #`(#,(expand-expr $lookup #'expr)))
      (* #'*)
      (field (identifier? #'field) #'field)))
)
