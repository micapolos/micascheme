(library (micalang idris)
  (export
    a-type a-type?
    an-index an-index?
    a-string a-string?
    arrow arrow? arrow-ins arrow-out
    typed typed? typed-type typed-ref
    inc
    parse evaluate)
  (import (micascheme))

  (data a-type)
  (data an-index)
  (data a-string)
  (data (arrow ins out))

  (data (typed type ref))

  (define evaluate-environment (environment '(micascheme) '(micalang idris)))

  (define inc (lambda (x) (+ x 1)))

  (define (index? $obj)
    (nonnegative-integer? $obj))

  (define (pi? $obj)
    (syntax-case? $obj (pi)
      ((pi in out) #t)))

  (define (env->var $env)
    (env-index->var $env 0))

  (define (env-index->var $env $index)
    (string->symbol
      (string-append "v" (number->string (+ (length $env) $index)))))

  (define (evaluate $env $term)
    (eval (typed-ref (parse $env $term)) evaluate-environment))

  (define (evaluate-typed $env $expected-type $term)
    (eval (parse-typed $env $expected-type $term) evaluate-environment))

  (define (parse-typed $env $type $term)
    (lets
      ($typed (parse $env $term))
      (if (equal? (typed-type $typed) $type)
        (typed-ref $typed)
        (syntax-error $term "invalid type"))))

  (define (parse $env $term)
    (syntax-case $term (type index string arrow inc + switch var lambda)
      (type
        (typed a-type 'a-type))
      (index
        (typed a-type 'an-index))
      (string
        (typed a-type 'a-string))
      ((arrow (in ...) out)
        (typed a-type
          `(arrow
            (list ,@(map (partial parse-typed $env a-type) #'(in ...)))
            ,(parse-typed $env a-type #'out))))
      (n
        (number? (datum n))
        (typed an-index (datum n)))
      (s
        (string? (datum s))
        (typed a-string (datum s)))
      (inc
        (typed (arrow (list an-index) an-index) 'inc))
      (+
        (typed (arrow (list an-index an-index) an-index) '+))
      ((switch idx branch ... default)
        (lets
          ($index (parse-typed $env an-index #'idx))
          ($typed-default (parse $env #'default))
          ($branches (map (partial parse-typed $env (typed-type $typed-default)) #'(branch ...)))
          (typed
            (typed-type $typed-default)
            `(index-switch ,$index ,@$branches ,(typed-ref $typed-default)))))
      ((var n)
        (switch (datum n)
          ((index? $index)
            (or
              (list-ref? $env $index)
              (syntax-error $term "undefined")))
          ((else $other)
            (syntax-error $term))))
      ((lambda (in ...) out)
        (lets
          ($in-types (map (partial evaluate-typed $env a-type) #'(in ...)))
          ($vars (map (partial env-index->var $env) (indices (length $in-types))))
          ($typed-vars (map typed $in-types $vars))
          ($typed-out (parse (append $typed-vars $env) #'out))
          (typed
            (arrow $in-types (typed-type $typed-out))
            `(lambda (,@$vars) ,(typed-ref $typed-out)))))
      ((fn arg ...)
        (lets
          ($typed-fn (parse $env #'fn))
          ($typed-args (map (partial parse $env) #'(arg ...)))
          (switch (typed-type $typed-fn)
            ((arrow? $arrow)
              (if
                (and
                  (= (length (arrow-ins $arrow)) (length $typed-args))
                  (for-all equal? (arrow-ins $arrow) (map typed-type $typed-args)))
                (typed
                  (arrow-out $arrow)
                  `(,(typed-ref $typed-fn) ,@(map typed-ref $typed-args)))
                (syntax-error $term "application type error")))
            ((else $other)
              (syntax-error #'fn "not arrow")))))
      (_ (syntax-error $term))))
)
