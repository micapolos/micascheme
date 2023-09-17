(library (reactive-syntax)
  (export
    context context? context-lookup-fn
    empty-context

    unit unit? unit-declarations unit-initializers unit-updaters
    empty-unit
    unit+

    reactive reactive? reactive-unit reactive-value
    pure-reactive
    reactive-bind

    syntax-reactive
    reactive-syntax
    reactive->datum
    reactive->vector-syntax
    reactive->vector
    reactive-counter
    reactive-osc

    syntax-transform
    syntax-list-transform

    pure)
  (import (micascheme))

  (data (context lookup-fn))
  (data (unit declarations initializers updaters))
  (data (reactive unit value))

  (define (empty-context)
    (context (lambda ($id) #f)))

  (define (empty-unit)
    (unit (stack) (stack) (stack)))

  (define (unit+ $a $b)
    (unit
      (push-all (unit-declarations $a) (unit-declarations $b))
      (push-all (unit-initializers $a) (unit-initializers $b))
      (push-all (unit-updaters $a) (unit-updaters $b))))

  (define (pure-reactive $value)
    (reactive (empty-unit) $value))

  (define (reactive-bind $reactive $fn)
    (lets
      ($fn-reactive ($fn (reactive-value $reactive)))
      (reactive
        (unit+
          (reactive-unit $reactive)
          (reactive-unit $fn-reactive))
        (reactive-value $fn-reactive))))

  (define (syntax-list-transform $context $syntax-list)
    #`(begin
      #,@(map (partial syntax-transform $context) $syntax-list)))

  (define (syntax-transform $context $syntax)
    (syntax-case $syntax (define)
      ((define $id $body) (identifier? #`$id)
        #`(begin
          (define-aux-keyword $id)
          (define-property $id reactive
            #,(reactive-syntax (syntax-reactive $context #`$body)))))
      ($other
        #`(writeln
          `(
            #,@(vector->list
              (reactive->vector
                (syntax-reactive $context #`$other)
                10))
            #,(datum->syntax #`+ `...))))))

  (define-aux-keyword pure)

  (define (syntax-reactive $context $syntax)
    (syntax-case $syntax (lets reactive var init update apply pure)
      ((pure $body)
        (pure-reactive #`$body))
      ((lets $body)
        (syntax-reactive $context #`$body))
      ((lets ($var $expr) $rest ... $body)
        (reactive-bind (syntax-reactive $context #`$expr)
          (lambda ($expr)
            (reactive-bind (syntax-reactive $context #`(lets $rest ... $body))
              (lambda ($body)
                (pure-reactive
                  #`(let (($var #,$expr))
                    #,$body)))))))
      ((apply $item ...)
        (reactive-bind
          (fold-left
            (lambda ($stack $syntax)
              (reactive-bind $stack
                (lambda ($stack)
                  (reactive-bind (syntax-reactive $context $syntax)
                    (lambda ($item)
                      (pure-reactive
                        (push $stack $item)))))))
            (pure-reactive (stack))
            (syntax->list #`($item ...)))
          (lambda ($stack)
            (pure-reactive
              #`(#,@(reverse $stack))))))
      ($id (identifier? #`$id)
        (or
          ((context-lookup-fn $context) #`$id)
          (syntax-reactive $context #`(pure $id))))
      (($item ...)
        (syntax-reactive $context #`(apply $item ...)))
      ($item
        (syntax-reactive $context #`(pure $item)))))


  (define (reactive-syntax $reactive)
    (lets
      ($unit (reactive-unit $reactive))
      ($value (reactive-value $reactive))
      ($vector (generate-temporary #`vector))
      ($index (generate-temporary #`index))
      #`(reactive
        (unit
          (stack #,@(reverse (unit-declarations $unit)))
          (stack #,@(reverse (unit-initializers $unit)))
          (stack #,@(reverse (unit-updaters $unit))))
        #,(reactive-value $reactive))))

  (define (reactive->datum $reactive)
    (lets
      ($unit (reactive-unit $reactive))
      ($value (reactive-value $reactive))
      ($vector (generate-temporary #`vector))
      ($index (generate-temporary #`index))
      `(reactive
        (declarations ,@(reverse (map syntax->datum (unit-declarations $unit))))
        (initializers ,@(reverse (map syntax->datum (unit-initializers $unit))))
        (updaters ,@(reverse (map syntax->datum (unit-updaters $unit))))
        (value ,(syntax->datum (reactive-value $reactive))))))

  (define (reactive->vector-syntax $reactive $size)
    (lets
      ($unit (reactive-unit $reactive))
      ($value (reactive-value $reactive))
      ($vector (generate-temporary #`vector))
      ($index (generate-temporary #`index))
      #`(let ()
        (define #,$vector (make-vector #,$size))
        #,@(reverse (unit-declarations $unit))
        #,@(reverse (unit-initializers $unit))
        (do!
          ((#,$index 0 (+ #,$index 1)))
          ((= #,$index #,$size) #,$vector)
          (vector-set! #,$vector #,$index #,$value)
          #,@(reverse (unit-updaters $unit))))))

  (define (reactive->vector $reactive $size)
    (eval
      (syntax->datum (reactive->vector-syntax $reactive $size))
      (environment `(micascheme))))

  (define (reactive-counter)
    (lets
      ($counter (generate-temporary #`counter))
      (reactive
        (unit
          (stack #`(define #,$counter))
          (stack #`(set! #,$counter 0))
          (stack #`(set! #,$counter (+ #,$counter 1))))
        $counter)))

  (define (reactive-osc $delta)
    (reactive-bind $delta
      (lambda ($delta)
        (lets
          ($osc (generate-temporary #`osc))
          (reactive
            (unit
              (stack #`(define #,$osc))
              (stack #`(set! #,$osc 0.0))
              (stack #`(set! #,$osc (fract (+ #,$osc #,$delta)))))
            $osc)))))
)
