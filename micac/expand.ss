(library (micac expand)
  (export
    expand-expr
    expand-instr
    expand-instrs)
  (import (micascheme) (micac env) (micac syntax))

  (data (expanded lookup value))

  (define (lookup+ $lookup $id $value)
    (lambda ($lookup-id)
      (if (free-identifier=? $lookup-id $id)
        $value
        ($lookup $lookup-id))))

  (define (expanded+binding (expanded $lookup $expanded-value) $id $value)
    (expanded
      (lookup+ $lookup $id $value)
      $expanded-value))

  (define (expanded+syntax (expanded $lookup $syntaxes) $syntax)
    (expanded
      $lookup
      (push $syntaxes $syntax)))

  (define-rules-syntax
    ((expanded-map (value expanded-expr) body)
      (lets
        ((expanded $lookup value) expanded-expr)
        (expanded $lookup body)))
    ((expanded-map decl decls ... body)
      (expanded-map decls ...
        (expanded-value (expanded-map decl body)))))

  (define (expand-instrs $lookup $syntax)
    (fluent $lookup
      (expanded (stack))
      (expanded+instrs $syntax)
      (expanded-value)
      (reverse)))

  (define (expand-instr $lookup $syntax)
    (force-single (expand-instrs $lookup #`(#,$syntax))))

  (define (expanded+instrs $expanded $instrs)
    (syntax-case $instrs (defer break-if)
      (() $expanded)
      (((defer deferred ...) body ...)
        (expanded+instrs $expanded
          #`(body ... deferred ...)))
      (((break-if expr break-body ...) body ...)
        (expanded+instr $expanded
          #`(if expr
            (then break-body ...)
            (else body ...))))
      ((instr instrs ...)
        (expanded+instrs
          (expanded+instr $expanded #'instr)
          #'(instrs ...)))))

  (define (declarator->expanded-syntax $lookup $declarator)
    (syntax-case $declarator (*)
      (id (identifier? #'id)
        (expanded
          (lookup+ $lookup #'id #f)
          #'id))
      ((* decl)
        (expanded-map
          ($expanded-decl (declarator->expanded-syntax $lookup #'decl))
          #`(* #,$expanded-decl)))
      ((* decl expr)
        (lets
          ($expr (expand-expr $lookup #'expr))
          (expanded-map
            ($expanded-decl (declarator->expanded-syntax $lookup #'decl))
            #`(* #,$expanded-decl #,$expr))))))

  (define (expanded+instr $expanded $instr)
    (lets
      ((expanded $lookup $syntaxes) $expanded)
      (syntax-case $instr (extern macro begin var const if when while then else)
        ((extern id)
          (expanded+binding $expanded (identifier id) #f))
        ((macro (id param ...) body ...)
          (expanded+binding $expanded (identifier id)
            (lambda ($syntax)
              (lambda ($lookup)
                (syntax-case $syntax ()
                  ((_ arg ...)
                    (syntax-subst
                      #'(param ...)
                      #'(arg ...)
                      #'(begin body ...))))))))
        ((macro id expr)
          (expanded+binding $expanded (identifier id)
            (lambda ($syntax)
              (lambda ($lookup)
                #'expr))))
        ((begin instr ...)
          (expanded+syntax $expanded
            #`(begin
              #,@(expand-instrs $lookup #'(instr ...)))))
        ((var type decl)
          (expanded-map
            ($decl (declarator->expanded-syntax $lookup #'decl))
            (push $syntaxes
              #`(var type #,$decl))))
        ((var type decl expr)
          (expanded-map
            ($decl (declarator->expanded-syntax $lookup #'decl))
            (push $syntaxes
              #`(var type #,$decl #,(expand-expr $lookup #'expr)))))
        ((const type decl expr)
          (expanded-map
            ($decl (declarator->expanded-syntax $lookup #'decl))
            (push $syntaxes
              #`(const type #,$decl #,(expand-expr $lookup #'expr)))))
        ((if expr (then then-body ...) (else else-body ...))
          (expanded+syntax $expanded
            #`(if
              #,(expand-expr $lookup #'expr)
              (then #,@(expand-instrs $lookup #'(then-body ...)))
              (else #,@(expand-instrs $lookup #'(else-body ...))))))
        ((when expr body ...)
          (expanded+syntax $expanded
            #`(when
              #,(expand-expr $lookup #'expr)
              #,@(expand-instrs $lookup #'(body ...)))))
        ((while expr body ...)
          (expanded+syntax $expanded
            #`(while
              #,(expand-expr $lookup #'expr)
              #,@(expand-instrs $lookup #'(body ...)))))
        ((id arg ...) (identifier? #'id)
          (switch ($lookup #'id)
            ((false? _)
              (expanded+syntax $expanded
                #`(id
                  #,@(map
                    (partial expand-expr $lookup)
                    (syntaxes arg ...)))))
            ((else $transformer)
              (fold-left
                expanded+instr
                $expanded
                (begin-syntaxes
                  (transform $transformer $instr $lookup)))))))))

  (define (expand-expr $lookup $expr)
    (syntax-case $expr
      (
        cast + - * div
        not and or
        bitwise-not bitwise-and bitwise-ior bitwise-xor
        bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
        ref &ref)
      ((cast type rhs)
        #`(cast type #,(expand-expr $lookup #'rhs)))
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
      ((id arg ...) (identifier? #'id)
        (switch ($lookup #'id)
          ((false? _)
            #`(id
              #,@(map
                (partial expand-expr $lookup)
                (syntaxes arg ...))))
          ((else $transformer)
            (expand-expr $lookup
              (begin-syntax
                (transform $transformer $expr $lookup))))))
      (id (identifier? #'id)
        (switch ($lookup #'id)
          ((false? _) #'id)
          ((else $transformer)
            (expand-expr $lookup
              (begin-syntax
                (transform $transformer $expr $lookup))))))
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
          #`(op #,(expand-op2 $lookup #'(op a b) $test? $proc) c cs ...)
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

  (define (expand-accessor $lookup $ref)
    (syntax-case $ref ()
      ((expr) #`(#,(expand-expr $lookup #'expr)))
      (other #'other)))
)
