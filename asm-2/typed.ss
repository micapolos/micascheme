(library (asm-2 typed)
  (export
    void type boolean integer char string function macro
    label equ db
    typed typed-type typed-value
    syntax->typed
    define-typed
    type=? asm-bytevector)
  (import (micascheme) (syntax lookup) (asm-2 block))

  (define-keywords typed type boolean integer char function macro asm-bytevector label equ db)

  (define-rules-syntax (literals typed)
    ((define-typed id (typed type expr))
      (define-typed id
        (lambda ($lookup $syntax)
          (syntax-case $syntax (id)
            (id #'(typed type expr))))))
    ((define-typed (id $lookup $syntax) body)
      (and (identifier? #'$lookup) (identifier? #'$syntax))
      (define-syntax id
        (lambda ($lookup $syntax) body)))
    ((define-typed id type/proc)
      (identifier? #'id)
      (define-syntax id
        (make-compile-time-value type/proc))))

  (define (syntax->typed $lookup $syntax)
    (syntax-case $syntax
      (
        typed void type boolean integer char string function lambda macro
        bytevector asm-bytevector label equ db)
      ((typed typ expr)
        #`(typed #,(syntax->expr $lookup #'type #'typ) expr))
      (void #`(typed type void))
      (type #`(typed type type))
      (boolean #`(typed type boolean))
      (integer #`(typed type integer))
      (char #`(typed type char))
      (string #`(typed type string))
      ((function params result)
        #`(typed type
          (function
            #,(map*
              (partial syntax->expr $lookup #'type)
              (partial syntax->expr $lookup #'type)
              (syntax->list* #'params))
            #,(syntax->expr $lookup #'type #'result))))
      ((void)
        #`(typed void (void)))
      (b
        (boolean? (datum b))
        #`(typed boolean b))
      (i
        (integer? (datum i))
        #`(typed integer i))
      (ch
        (char? (datum ch))
        #`(typed char ch))
      (str
        (string? (datum str))
        #`(typed string str))
      ((macro proc)
        #`(typed
          (macro
            #,(eval
              (syntax->datum/annotation #'proc)
              (environment '(micascheme) '(asm-2 typed) '(asm-2 block))))
          #f))
      ((lambda ((typ id) ...) body)
        (for-all identifier? #'(id ...))
        (lets
          ($types (map (partial syntax->expr $lookup #'type) #'(typ ...)))
          ($ids #'(id ...))
          ($typed-body
            (syntax->typed
              (fold-left lookup+undefined $lookup $ids $types)
              #'body))
          #`(typed
            (function (#,@$types) #,(typed-type $typed-body))
            (lambda (#,@$ids) #,(typed-value $typed-body)))))
      ((asm-bytevector blk ...)
        #`(typed bytevector
          (block-bytevector
            (fold-left
              block-apply
              (empty-block)
              (list #,@(map (partial syntax->expr $lookup #'(function (block) block)) #'(blk ...)))))))
      ((label id)
        (identifier? #'id)
        #`(typed
          (function (block) block)
          (lambda ($block)
            (block+label $block #'id))))
      ((equ id expr)
        (identifier? #'id)
        #`(typed
          (function (block) block)
          (lambda ($block)
            (block+equ $block #'id
              #'#,(syntax->expr $lookup #'integer #'expr)))))
      ((db u8)
        #`(typed
          (function (block) block)
          (lambda ($block) $block)))
      ((fn arg ...)
        (syntax-case (syntax->typed $lookup #'fn) (typed function)
          ((typed (function params result) fn-expr)
            (syntax-case
              (map*
                (partial syntax->expr $lookup)
                (partial syntaxes->exprs $lookup)
                (syntax->list* #'params)
                #'(arg ...))
              ()
              ((arg-expr ...)
                #`(typed result (fn-expr arg-expr ...)))))
          ((typed _ _)
            (syntax-error #'fn "not an function"))))
      (id
        (identifier? #'id)
        (switch (lookup-ref $lookup #'id)
          ((procedure? $function)
            ($function $lookup #'id))
          ((else $type)
            #`(typed #,$type id))))))

  (define (syntax->expr $lookup $type $syntax)
    (syntax-case (syntax->typed $lookup $syntax) (typed)
      ((typed type expr)
        (cond
          ((type=? #'type $type) #'expr)
          (else
            (syntax-error $syntax
              (format "invalid type ~s, expected ~s, in"
                (syntax->datum #'type)
                (syntax->datum $type))))))))

  (define (syntaxes->exprs $lookup $type $syntaxes)
    (map (partial syntax->expr $lookup $type) $syntaxes))

  (define (type=? $type-a $type-b)
    (equal?
      (syntax->datum $type-a)
      (syntax->datum $type-b)))

  (define (typed-type $typed)
    (syntax-case $typed (typed)
      ((typed type value) #'type)))

  (define (typed-value $typed)
    (syntax-case $typed (typed)
      ((typed type value) #'value)))
)
