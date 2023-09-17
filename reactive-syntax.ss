(library (reactive-syntax)
  (export
    unit unit? unit-declarations unit-initializers unit-updaters
    empty-unit
    unit+

    reactive reactive? reactive-unit reactive-value
    pure-reactive
    reactive-bind

    syntax-reactive
    reactive-vector-syntax
    reactive-vector
    counter)
  (import (micascheme))

  (data (unit declarations initializers updaters))
  (data (reactive unit value))

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

  (define (syntax-reactive $syntax)
    (syntax-case $syntax (lets)
      ((lets $body)
        (syntax-reactive #`$body))
      ((lets ($var $expr) $rest ... $body)
        (reactive-bind (syntax-reactive #`$expr)
          (lambda ($expr)
            (reactive-bind (syntax-reactive #`(let $rest ... $body))
              (lambda ($body)
                (pure-reactive
                  #`(let (($var #,$expr))
                    #,$body)))))))
      (($item ...)
        (reactive-bind
          (fold-left
            (lambda ($stack $syntax)
              (reactive-bind $stack
                (lambda ($stack)
                  (reactive-bind (syntax-reactive $syntax)
                    (lambda ($item)
                      (pure-reactive
                        (push $stack $item)))))))
            (pure-reactive (stack))
            (syntax->list #`($item ...)))
          (lambda ($stack)
            (pure-reactive
              #`(#,@(reverse $stack))))))
      ($other
        (pure-reactive #`$other))))

  (define (reactive-vector-syntax $reactive $size)
    (lets
      ($unit (reactive-unit $reactive))
      ($value (reactive-value $reactive))
      ($vector (generate-temporary #`vector))
      ($index (generate-temporary #`index))
      #`(let ()
        #,@(reverse (unit-declarations $unit))
        #,@(reverse (unit-initializers $unit))
        (define #,$vector (make-vector #,$size))
        (do!
          ((#,$index 0 (+ #,$index 1)))
          ((= #,$index #,$size) #,$vector)
          #,@(reverse (unit-updaters $unit))
          (vector-set! #,$vector #,$index #,$value)))))

  (define (reactive-vector $reactive $size)
    (eval
      (syntax->datum (reactive-vector-syntax $reactive $size))
      (environment `(micascheme))))

  (define (counter)
    (lets
      ($counter (generate-temporary #`counter))
      (reactive
        (unit
          (stack #`(define #,$counter -1))
          (stack)
          (stack #`(set! #,$counter (+ #,$counter 1))))
        $counter)))
)
