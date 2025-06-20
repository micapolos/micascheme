(library (asm typed)
  (export
    typed typed? typed-type typed-value typed->datum
    void type boolean integer char string function
    binary
    typed typed-type typed-value
    syntax->typed syntax->typed-noexpand syntax->expr
    define-typed
    type=?)
  (import
    (micascheme)
    (syntax lookup)
    (asm u)
    (asm block)
    (asm binary))

  (data (typed type value))

  (define-keywords type boolean integer char function)

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
    (lookup+ $lookup $id
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
        bytevector let
        u2 u3 u8 u16 s8)
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
      ((lambda ((id typ) ...) body)
        (for-all identifier? #'(id ...))
        (lets
          ($ids #'(id ...))
          ($types (map (partial syntax->expr $lookup #'type) #'(typ ...)))
          ($typed-body
            (syntax->typed
              (fold-left lookup+type $lookup $ids $types)
              #'body))
          (typed
            #`(function (#,@$types) #,(typed-type $typed-body))
            #`(lambda (#,@$ids) #,(typed-value $typed-body)))))
      ((let ((id expr) ...) body)
        (lets
          ($ids #'(id ...))
          ($typeds (map (partial syntax->typed $lookup) #'(expr ...)))
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
      ((u2 expr)
        (typed #'integer #`(u2 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((u3 expr)
        (typed #'integer #`(u3 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((u8 expr)
        (typed #'integer #`(u8 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((u16 expr)
        (typed #'integer #`(u16 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
      ((s8 expr)
        (typed #'integer #`(s8 #,(syntax->expr $lookup #'integer #'expr) #'expr)))
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
)
