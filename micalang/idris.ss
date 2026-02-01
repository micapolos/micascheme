(library (micalang idris)
  (export
    a-type a-type?
    an-index an-index?
    a-string a-string?
    arrow arrow? arrow-in arrow-out
    typed typed? typed-type typed-ref
    parse evaluate
    inc)
  (import (micascheme))

  (data a-type)
  (data an-index)
  (data a-string)
  (data (arrow in out))

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

  (define (env-ref $env $symbol)
    (switch (assq $symbol $env)
      ((false? _) (syntax-error $symbol "undefined"))
      ((else $ass) (cdr $ass))))

  (define (parse $env $term)
    (syntax-case $term (type index string arrow inc switch var lambda)
      (type
        (typed a-type a-type))
      (index
        (typed a-type an-index))
      (string
        (typed a-type a-string))
      ((arrow in out)
        (typed a-type
          (arrow
            (parse-typed $env a-type #'in)
            (parse-typed $env a-type #'out))))
      (n
        (number? (datum n))
        (typed an-index (datum n)))
      (s
        (string? (datum s))
        (typed a-string (datum s)))
      (inc
        (typed (arrow an-index an-index) 'inc))
      ((switch idx branch ... default)
        (lets
          ($index (parse-typed $env an-index #'idx))
          ($typed-default (parse $env #'default))
          ($branches (map (partial parse-typed $env (typed-type $typed-default)) #'(branch ...)))
          (typed
            (typed-type $typed-default)
            `(index-switch ,$index ,@$branches ,(typed-ref $typed-default)))))
      ((lambda (id typ) out)
        (symbol? (datum id))
        (lets
          ($id (datum id))
          ($type (parse-typed $env a-type #'typ))
          ($typed-out (parse (cons (cons $id $type) $env) #'out))
          (typed
            (arrow $type (typed-type $typed-out))
            `(lambda (,$id) ,(typed-ref $typed-out)))))
      ((fn arg)
        (lets
          ($typed-fn (parse $env #'fn))
          ($typed-arg (parse $env #'arg))
          (switch (typed-type $typed-fn)
            ((arrow? $arrow)
              (if (equal? (arrow-in $arrow) (typed-type $typed-arg))
                (typed
                  (arrow-out $arrow)
                  `(,(typed-ref $typed-fn) ,(typed-ref $typed-arg)))
                (syntax-error #'arg "invalid type")))
            ((else $other)
              (syntax-error #'fn "not arrow")))))
      (s
        (symbol? (datum s))
        (typed (env-ref $env (datum s)) (datum s)))
      (_ (syntax-error $term))))
)
