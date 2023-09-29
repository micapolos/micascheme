(library (tico)
  (export
    empty-context context-ref

    typed typed? typed-box-opt typed-syntax typed-type
    typed-unsyntax

    type-type type-type?
    boolean-type boolean-type?
    number-type number-type?
    string-type string-type?
    struct struct? struct-name struct-items

    parse-typed-list parse-typed

    get type boolean number)
  (import (micascheme))

  (data (typed box-opt syntax type))

  (data (type-type))
  (data (boolean-type))
  (data (number-type))
  (data (string-type))
  (data (struct name items))
  (data (function-type params result))

  (define-aux-keyword get)
  (define-aux-keyword type)
  (define-aux-keyword boolean)
  (define-aux-keyword number)

  (define (empty-context)
    (lambda ($type) #f))

  (define (context-ref $context $type)
    ($context $type))

  (define (context-push $context $typed)
    (lambda ($type)
      (cond
        ((equal? $type (typed-type $typed)) $typed)
        (else (context-ref $context $type)))))

  (define (typed-unsyntax $typed)
    (typed
      (typed-box-opt $typed)
      (syntax->datum (typed-syntax $typed))
      (typed-type $typed)))

  (define (parse-typed-list $context $syntaxes)
    (map (partial parse-typed $context) $syntaxes))

  (define parse-typed
    (case-lambda
      (($syntax)
        (parse-typed (empty-context) $syntax))
      (($context $syntax)
        (syntax-case $syntax (type boolean string number get begin lambda)
          (type (typed (box (type-type)) #`(type-type) (type-type)))
          (boolean (typed (box (boolean-type)) #`(boolean-type) (boolean-type)))
          (number (typed (box (number-type)) #`(number-type) (number-type)))
          (string (typed (box (string-type)) #`(string-type) (string-type)))
          ((get $type)
            (or
              (context-ref $context (parse-type $context #`$type))
              (syntax-error $syntax "not found")))
          ((begin $expr)
            (parse-typed $context #`$expr))
          ((begin $decl $decl2 ... $expr)
            (parse-typed
              (parse-declaration $context #`$decl)
              #`(begin $decl2 ... $expr)))
          ; ((lambda ($param ...) $body)
          ;   (lets
          ;     ($param-types (map (partial parse-type $context) (syntax->list #`($param ...))))
          ;     ($param-typed-list
          ;       (map
          ;         (lambda ($param-type)
          ;           (typed
          ;             #f
          ;             #`???
          ;             $param-type)
          ;         $param-types)))
          ;     ($context (fold-left context-push $context $param-typed-list))
          ;     ($typed-body (parse-typed $context #`$body))
          ;     (typed
          ;       #f ; todo make it compile-time constant if possible
          ;       #`(lambda ($param ...) ...)
          ;       (function-type $param-types (typed-type $typed-body)))))
          (($name $arg ...) (identifier? #`$name)
            (lets
              ($symbol (syntax->datum #`$name))
              ($typed-list (parse-typed-list $context (syntax->list #`($arg ...))))
              (typed
                (and
                  (for-all typed-box-opt $typed-list)
                  (box (struct $symbol (map unbox (map typed-box-opt $typed-list)))))
                #`(struct (quote $name) (list #,@(map typed-syntax $typed-list)))
                (struct $symbol (map typed-type $typed-list)))))
          ($item
            (switch (syntax->datum #`$item)
              ((boolean? $boolean) (typed (box $boolean) #`$item (boolean-type)))
              ((number? $number) (typed (box $number) #`$item (number-type)))
              ((string? $string) (typed (box $string) #`$item (string-type)))))
          (else (syntax-error $syntax "dupa"))))))

  (define (parse-declaration $context $syntax)
    (syntax-case $syntax (define)
      ((define $item)
        (context-push $context (parse-typed $context #`$item)))
      (else
        (syntax-error $syntax "invalid declaration"))))

  (define (parse-type $context $syntax)
    (lets
      ($typed (parse-typed $context $syntax))
      ($box-opt (typed-box-opt $typed))
      (cond
        ((not $box-opt) (syntax-error $syntax "not compile-time constant"))
        (else (typed-type $typed)))))
)
