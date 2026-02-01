(library (micalang idris)
  (export
    a-type a-type?
    an-index an-index?
    a-string a-string?
    forall forall? forall-in forall-out
    typed typed? typed-type typed-ref
    parse evaluate
    inc)
  (import (micascheme))

  (data a-type)
  (data an-index)
  (data a-string)
  (data (forall in out))

  (data (typed type ref))

  (define evaluate-environment (environment '(micascheme) '(micalang idris)))

  (define (inc x) (+ x 1))

  (define (index? $obj)
    (nonnegative-integer? $obj))

  (define (pi? $obj)
    (syntax-case? $obj (pi)
      ((pi in out) #t)))

  (define (env->var $env)
    (string->symbol
      (string-append "v" (number->string (length $env)))))

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
    (syntax-case $term (type index string forall inc switch var lambda)
      (type
        (typed a-type a-type))
      (index
        (typed a-type an-index))
      (string
        (typed a-type a-string))
      ((forall in out)
        (typed a-type
          (forall
            (parse-typed $env a-type #'in)
            (parse-typed $env a-type #'out))))
      (n
        (number? (datum n))
        (typed an-index (datum n)))
      (s
        (string? (datum s))
        (typed a-string (datum s)))
      (inc
        (typed (forall an-index an-index) 'inc))
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
      ((lambda in out)
        (lets
          ($typed-var
            (typed
              (parse-typed $env a-type #'in)
              (env->var $env)))
          ($typed-out (parse (cons $typed-var $env) #'out))
          (typed
            (forall (typed-type $typed-var) (typed-type $typed-out))
            `(lambda (,(typed-ref $typed-var)) ,(typed-ref $typed-out)))))
      ((fn arg)
        (lets
          ($typed-fn (parse $env #'fn))
          ($typed-arg (parse $env #'arg))
          (switch (typed-type $typed-fn)
            ((forall? $forall)
              (if (equal? (forall-in $forall) (typed-type $typed-arg))
                (typed
                  (forall-out $forall)
                  `(,(typed-ref $typed-fn) ,(typed-ref $typed-arg)))
                (syntax-error #'arg "invalid type")))
            ((else $other)
              (syntax-error #'fn "not forall")))))
      (_ (syntax-error $term))))
)
