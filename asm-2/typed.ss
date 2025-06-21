(library (asm-2 typed)
  (export
    typed typed? typed-type typed-value typed->datum
    void type boolean integer char string function
    label db dw
    binary
    typed typed-type typed-value
    syntax->typed syntax->typed-noexpand syntax->expr
    define-typed define-asm
    type=? asm-binary
    db-block-function dw-block-function label-block-function block-function-append assembly)
  (import
    (micascheme)
    (syntax lookup)
    (asm-2 u)
    (asm-2 block)
    (asm-2 binary))

  (data (typed type value))

  (define-keywords type boolean integer char function asm-binary label db dw assembly)

  (define-rule-syntax (db-block-function expr)
    (lambda ($block)
      (block+binary-syntax $block 1
        #'(db-binary expr))))

  (define-rule-syntax (dw-block-function expr)
    (lambda ($block)
      (block+binary-syntax $block 2
        #'(dw-binary expr))))

  (define-rule-syntax (label-block-function label)
    (lambda ($block)
      (block+label $block #'label)))

  (define (block-function-append . $fns)
    (lambda ($block)
      (fold-left
        (lambda ($block $fn) ($fn $block))
        $block
        $fns)))

  (define-rules-syntax (literals typed)
    ((define-typed id (typed type expr))
      (define-typed id
        (lambda ($lookup $syntax)
          (syntax-case $syntax (id)
            (id
              (identifier? #'id)
              (typed #'type #'expr))
            (other
              (syntax->typed-noexpand $lookup #'other))))))
    ((define-typed (id $lookup $syntax) body)
      (and (identifier? #'$lookup) (identifier? #'$syntax))
      (define-typed id
        (lambda ($lookup $syntax) body)))
    ((define-typed id type/proc)
      (identifier? #'id)
      (define-syntax id
        (make-compile-time-value type/proc))))

  (define (syntax->typed $lookup $syntax)
    (switch (syntax-selector $syntax)
      ((identifier? $identifier)
        (switch ($lookup $identifier)
          ((false? _)
            (syntax->typed-noexpand $lookup $syntax))
          ((else $procedure)
            ($procedure $lookup $syntax))))
      ((else _)
        (syntax->typed-noexpand $lookup $syntax))))

  (define (lookup+type $lookup $id $type)
    (lookup+undefined $lookup $id
      (lambda ($lookup $syntax)
        (syntax-case $syntax ()
          (id
            (identifier? #'id)
            (typed $type $id))))))

  (define (syntax->typed-noexpand $lookup $syntax)
    (syntax-case $syntax
      (
        typed void type boolean integer char string function lambda
        db-binary dw-binary binary-append binary->bytevector
        bytevector asm-binary block label db dw org let
        u2 u3 u8 u16)
      ((typed typ expr)
        (typed (syntax->expr $lookup #'type #'typ) #'expr))
      (void (typed #'type #'void))
      (type (typed #'type #'type))
      (boolean (typed #'type #'boolean))
      (integer (typed #'type #'integer))
      (char (typed #'type #'char))
      (string (typed #'type #'string))
      ((function params result)
        (typed #'type
          #`(function
            #,(map*
              (partial syntax->expr $lookup #'type)
              (partial syntax->expr $lookup #'type)
              (syntax->list* #'params))
            #,(syntax->expr $lookup #'type #'result))))
      ((void)
        (typed #'void #'(void)))
      (b
        (boolean? (datum b))
        (typed #'boolean #'b))
      (i
        (integer? (datum i))
        (typed #'integer #'i))
      (i
        (number? (datum i))
        (syntax-error $syntax "non-integer number"))
      (ch
        (char? (datum ch))
        (typed #'char #'ch))
      (str
        (string? (datum str))
        (typed #'string #'str))
      ((lambda ((typ id) ...) body)
        (for-all identifier? #'(id ...))
        (lets
          ($types (map (partial syntax->expr $lookup #'type) #'(typ ...)))
          ($ids #'(id ...))
          ($typed-body
            (syntax->typed
              (fold-left lookup+type $lookup $ids $types)
              #'body))
          (typed
            #`(function (#,@$types) #,(typed-type $typed-body))
            #`(lambda (#,@$ids) #,(typed-value $typed-body)))))
      ((let ((id expr) ...) body)
        (for-all identifier? #'(id ...))
        (lets
          ($typeds (map (partial syntax->typed $lookup) #'(expr ...)))
          ($ids #'(id ...))
          ($types (map typed-type $typeds))
          ($values (map typed-value $typeds))
          ($typed-body
            (syntax->typed
              (fold-left lookup+type $lookup $ids $types)
              #'body))
          (typed
            (typed-type $typed-body)
            #`(let
              (
                #,@(map
                  (lambda ($id $value) #`(#,$id #,$value))
                  $ids $values))
              #,(typed-value $typed-body)))))
      ((db-binary expr)
        (typed #'binary
          #`(db-binary #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((dw-binary expr)
        (typed #'binary
          #`(dw-binary #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((binary-append expr ...)
        (typed #'binary
          #`(binary-append
            #,@(map (partial syntax->expr $lookup #'binary) #'(expr ...)))))
      ((binary->bytevector expr)
        (typed #'bytevector
          #`(binary->bytevector #,(syntax->expr $lookup #'binary #'expr))))
      ((asm-binary (org $org) body ...)
        (typed #'binary
          (syntax->expr $lookup #'binary
            (block-binary-syntax
              (app (syntax->expr $lookup #'assembly #'(block body ...)) (empty-block))
              (datum $org)))))
      ((block b ...)
        (typed #'assembly
          (apply block-function-append
            (map (partial syntax->expr $lookup #'assembly) #'(b ...)))))
      ((label id)
        (identifier? #'id)
        (typed #'assembly (label-block-function id)))
      ((db expr)
        (typed #'assembly (db-block-function expr)))
      ((dw expr)
        (typed #'assembly (dw-block-function expr)))
      ((u2 expr)
        (typed #'integer #`(u2 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((u3 expr)
        (typed #'integer #`(u3 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((u8 expr)
        (typed #'integer #`(u8 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((u16 expr)
        (typed #'integer #`(u16 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((fn arg ...)
        (lets
          ((typed $type $expr) (syntax->typed $lookup #'fn))
          (syntax-case $type (function)
            ((function params result)
              (syntax-case
                (map*
                  (partial syntax->expr $lookup)
                  (partial syntaxes->exprs $lookup)
                  (syntax->list* #'params)
                  #'(arg ...))
                ()
                ((arg-expr ...)
                  (typed #'result #`(#,$expr arg-expr ...)))))
            (_
              (syntax-error #'fn "not an function")))))))

  (define (syntax->expr $lookup $expected-type $syntax)
    (lets
      ((typed $type $expr) (syntax->typed $lookup $syntax))
      (cond
        ((type=? $type $expected-type) $expr)
        (else
          (syntax-error $syntax
            (format "invalid type ~s, expected ~s, in"
              (syntax->datum $type)
              (syntax->datum $expected-type)))))))

  (define (syntaxes->exprs $lookup $type $syntaxes)
    (map (partial syntax->expr $lookup $type) $syntaxes))

  (define (type=? $type-a $type-b)
    (equal?
      (syntax->datum $type-a)
      (syntax->datum $type-b)))

  (define (typed->datum $typed)
    `(typed
      ,(syntax->datum (typed-type $typed))
      ,(syntax->datum (typed-value $typed))))

  (define-syntax (define-asm $syntax)
    (syntax-case $syntax (keywords)
      ((_ (keywords keyword ...) clause ...)
        #`(begin
          #,@(map-with
            ($group (group-by syntax-clause-id free-identifier=? #'(clause ...)))
            (lets
              ((pair $id $clauses) $group)
              #`(define-typed (#,$id $lookup $syntax)
                (syntax-case $syntax (keyword ...)
                  #,@(map-with ($clause $clauses)
                    (syntax-case $clause ()
                      ((pattern body ...)
                        #`(pattern (syntax->typed $lookup #'(block body ...))))))))))))
      ((_ clause ...)
        #`(define-asm (keywords) clause ...))))
)
