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
    function-type function-type? function-type-params function-type-result

    parse-typed-list parse-typed

    get type boolean number bind)
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

  (define tico-environment
    (copy-environment (environment `(micascheme))))

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
          ((begin $body)
            (parse-typed $context #`$body))
          ((begin $expr $expr2 ... $body)
            (lets
              ($typed-expr (parse-typed $context #`$expr))
              ($tmp (generate-temporary))
              ($expr-box-opt (typed-box-opt $typed-expr))
              (_ (when $expr-box-opt
                (define-top-level-value
                  (syntax->datum $tmp)
                  (unbox $expr-box-opt)
                  tico-environment)))
              ($binding (typed $expr-box-opt $tmp (typed-type $typed-expr)))
              ($context (context-push $context $binding))
              ($typed-body (parse-typed $context #`(begin $expr2 ... $body)))
              (typed
                (typed-box-opt $typed-body)
                #`(let ((#,$tmp #,(typed-syntax $typed-expr)))
                  #,(typed-syntax $typed-body))
                (typed-type $typed-body))))
          ((lambda ($param ...) $body)
            (lets
              ($param-types (map (partial parse-type $context) (syntax->list #`($param ...))))
              ($param-tmps
                (reverse
                  (fold-left
                    (lambda ($stack _) (push $stack (generate-temporary)))
                    (stack)
                    $param-types)))
              ($bindings
                (map
                  (lambda ($param-type $param-tmp) (typed #f $param-tmp $param-type))
                  $param-types
                  $param-tmps))
              ($context (fold-left context-push $context $bindings))
              ($typed-body (parse-typed $context #`$body))
              (typed
                #f ; todo make it compile-time constant if possible
                #`(lambda (#,@$param-tmps)
                  #,(typed-syntax $typed-body))
                (function-type $param-types (typed-type $typed-body)))))
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

  (define (parse-type $context $syntax)
    (lets
      ($typed (parse-typed $context $syntax))
      ($box-opt (typed-box-opt $typed))
      (cond
        ((not $box-opt) (syntax-error $syntax "not compile-time constant"))
        (else (typed-type $typed)))))
)
