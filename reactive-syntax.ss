(library (reactive-syntax)
  (export
    context context? context-lookup-fn
    empty-context lookup-context context-bind context-ref

    deps deps? deps-declarations deps-updaters
    empty-deps
    deps+

    reactive reactive? reactive-deps reactive-value
    pure-reactive
    reactive-bind
    reactive-list

    template template? template-params template-body

    syntax-reactive
    reactive-syntax
    reactive->datum
    reactive->vector-syntax
    reactive->vector
    reactive-counter
    reactive-osc

    syntax-transform
    syntax-list-transform

    pure sequence)
  (import (micascheme))

  (data (context bindings lookup-fn))
  (data (deps declarations updaters))
  (data (reactive deps value))
  (data (template params body))

  (define (empty-context)
    (lookup-context (lambda (_) #f)))

  (define (lookup-context $fn)
    (context (stack) $fn))

  (define (context-bind $context $id $reactive)
    (context
      (push (context-bindings $context) (cons $id $reactive))
      (context-lookup-fn $context)))

  (define (context-ref $context $id)
    (or
      (lets
        ($ass (assp (partial free-identifier=? $id) (context-bindings $context)))
        (and $ass (cdr $ass)))
      ((context-lookup-fn $context) $id)))

  (define (context-clean $context)
    (context (stack) (context-lookup-fn $context)))

  (define (empty-deps)
    (deps (stack) (stack)))

  (define (deps+ $a $b)
    (deps
      (push-all (deps-declarations $a) (deps-declarations $b))
      (push-all (deps-updaters $a) (deps-updaters $b))))

  (define (pure-reactive $value)
    (reactive (empty-deps) $value))

  (define (reactive-bind $reactive $fn)
    (lets
      ($fn-reactive ($fn (reactive-value $reactive)))
      (reactive
        (deps+
          (reactive-deps $reactive)
          (reactive-deps $fn-reactive))
        (reactive-value $fn-reactive))))

  (define (reactive-list $reactives)
    (cond
      ((null? $reactives)
        (pure-reactive (list)))
      (else
        (reactive-bind (car $reactives)
          (lambda ($car)
            (reactive-bind (reactive-list (cdr $reactives))
              (lambda ($cdr)
                (pure-reactive (cons $car $cdr)))))))))

  (define (syntax-list-transform $context $syntax-list)
    #`(begin
      #,@(map (partial syntax-transform $context) $syntax-list)))

  (define (syntax-transform $context $syntax)
    (syntax-case $syntax (define)
      ((define $id $body)
        (identifier? #`$id)
        #`(begin
          (define-aux-keyword $id)
          (define-property $id reactive
            #,(reactive-syntax
              (syntax-reactive $context
                #`$body)))))
      ((define ($id $param ...) $body)
        (for-all identifier? (syntax->list #`($id $param ...)))
        #`(begin
          (define-aux-keyword $id)
          (define-property $id reactive
            #,(reactive-syntax
              (syntax-reactive $context
                #`(lambda ($param ...) $body))))))
      ($other
        #`(writeln
          #,(reactive->vector-syntax
            (syntax-reactive $context #`$other)
            10)))))

  (define-aux-keyword pure)
  (define-aux-keyword sequence)

  (define (syntax-reactive $context $syntax)
    (syntax-case $syntax (sequence lets reactive apply pure lambda)
      ((pure $body)
        (pure-reactive #`$body))
      ((sequence $var $init $update) (identifier? #`$var)
        (lets
          ($tmp (generate-temporary #`$var))
          ($context (context-bind $context #`$var (pure-reactive $tmp)))
          (reactive-bind (syntax-reactive $context #`$init)
            (lambda ($init)
              (reactive-bind (syntax-reactive $context #`$update)
                (lambda ($update)
                  (reactive
                    (deps
                      (stack #`(define #,$tmp #,$init))
                      (stack #`(set! #,$tmp #,$update)))
                    $tmp)))))))
      ((lambda ($param ...) $body)
        (for-all identifier? (syntax->list #`($param ...)))
        (template (syntax->list #`($param ...)) #`$body))
      ((lets $body)
        (syntax-reactive $context #`$body))
      ((lets ($var $expr) $rest ... $body) (identifier? #`$var)
        (reactive-bind (syntax-reactive $context #`$expr)
          (lambda ($expr)
            (lets
              ($tmp (generate-temporary #`$var))
              ($context (context-bind $context #`$var (pure-reactive $tmp)))
              ($reactive
                (reactive
                  (deps
                    (stack #`(define #,$tmp #,$expr))
                    (stack #`(set! #,$tmp #,$expr)))
                  $tmp))
              (reactive-bind $reactive
                (lambda (_)
                  (syntax-reactive $context #`(lets $rest ... $body))))))))
      ((apply $fn $arg ...)
        (lets
          ($fn (syntax-reactive $context #`$fn))
          ($args
            (map
              (partial syntax-reactive $context)
              (syntax->list #`($arg ...))))
          (switch $fn
            ((reactive? $reactive-fn)
              (reactive-bind $reactive-fn
                (lambda ($fn)
                  (reactive-bind (reactive-list $args)
                    (lambda ($args)
                      (pure-reactive #`(#,$fn #,@$args)))))))
            ((template? $template)
              (lets
                ($params (template-params $template))
                ($context
                  (fold-left
                    context-bind
                    (context-clean $context)
                    $params $args))
                (syntax-reactive $context (template-body $template)))))))
      ($id (identifier? #`$id)
        (or
          (context-ref $context #`$id)
          (syntax-reactive $context #`(pure $id))))
      (($item ...)
        (syntax-reactive $context #`(apply $item ...)))
      ($item
        (syntax-reactive $context #`(pure $item)))))

  (define (reactive-syntax $reactive)
    (switch $reactive
      ((reactive? $reactive)
        (lets
          ($deps (reactive-deps $reactive))
          ($value (reactive-value $reactive))
          ($vector (generate-temporary #`vector))
          ($index (generate-temporary #`index))
          #`(reactive
            (deps
              (stack #,@(map (lambda ($) #`(syntax #,$)) (reverse (deps-declarations $deps))))
              (stack #,@(map (lambda ($) #`(syntax #,$)) (reverse (deps-updaters $deps)))))
            (syntax #,(reactive-value $reactive)))))
      ((template? $template)
        (lets
          #`(template
            (list #,@(map (lambda ($param) #`(syntax #,$param)) (template-params $template)))
            (syntax #,(template-body $template)))))))

  (define (reactive->datum $reactive)
    (lets
      ($deps (reactive-deps $reactive))
      ($value (reactive-value $reactive))
      ($vector (generate-temporary #`vector))
      ($index (generate-temporary #`index))
      `(reactive
        (declarations ,@(reverse (map syntax->datum (deps-declarations $deps))))
        (updaters ,@(reverse (map syntax->datum (deps-updaters $deps))))
        (value ,(syntax->datum (reactive-value $reactive))))))

  (define (reactive->vector-syntax $reactive $size)
    (lets
      ($deps (reactive-deps $reactive))
      ($value (reactive-value $reactive))
      ($vector (generate-temporary #`vector))
      ($index (generate-temporary #`index))
      #`(let ()
        (define #,$vector (make-vector #,$size))
        #,@(reverse (deps-declarations $deps))
        (do!
          ((#,$index 0 (+ #,$index 1)))
          ((= #,$index #,$size) #,$vector)
          (vector-set! #,$vector #,$index #,$value)
          #,@(reverse (deps-updaters $deps))))))

  (define (reactive->vector $reactive $size)
    (eval
      (syntax->datum (reactive->vector-syntax $reactive $size))
      (environment `(micascheme))))

  (define (reactive-counter)
    (lets
      ($counter (generate-temporary #`counter))
      (reactive
        (deps
          (stack #`(define #,$counter 0))
          (stack #`(set! #,$counter (+ #,$counter 1))))
        $counter)))

  (define (reactive-osc $delta)
    (reactive-bind $delta
      (lambda ($delta)
        (lets
          ($osc (generate-temporary #`osc))
          (reactive
            (deps
              (stack #`(define #,$osc 0.0))
              (stack #`(set! #,$osc (fract (+ #,$osc #,$delta)))))
            $osc)))))
)
