(library (micalang idris)
  (export
    a-type a-type?
    an-index an-index?
    a-string a-string?
    arrow arrow? arrow-in arrow-out
    typed typed? typed-type typed-ref

    variable variable? variable-index
    abstraction abstraction? abstraction-body
    application application? application-lhs application-rhs

    parse evaluate
    compile-term
    inc add)
  (import (micascheme))

  (data a-type)
  (data an-index)
  (data a-string)
  (data (a-list element))
  (data (arrow in out))

  (data (typed type ref))

  (data (abstraction body))
  (data (application lhs rhs))
  (data (variable index))

  (define evaluate-environment (environment '(micascheme) '(micalang idris)))

  (define (inc x) (+ x 1))
  (define (add x) (lambda (y) (+ x y)))

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
    (syntax-case $term (native type index string arrow list inc add switch lambda)
      ((native typ value)
        (typed
          (parse-typed $env a-type (datum typ))
          (datum value)))
      (type
        (typed a-type a-type))
      (index
        (typed a-type an-index))
      (string
        (typed a-type a-string))
      ((arrow in out)
        (typed a-type
          (arrow
            (parse-typed $env a-type (datum in))
            (parse-typed $env a-type (datum out)))))
      (list
        (typed
          (arrow a-type a-type)
          (abstraction (a-list (variable 0)))))
      (n
        (number? (datum n))
        (typed an-index (datum n)))
      (s
        (string? (datum s))
        (typed a-string (datum s)))
      (inc
        (typed (arrow an-index an-index) 'inc))
      (add
        (typed (arrow an-index (arrow an-index an-index)) 'add))
      ((switch idx branch ... default)
        (lets
          ($index (parse-typed $env an-index (datum idx)))
          ($typed-default (parse $env (datum default)))
          ($branches (map (partial parse-typed $env (typed-type $typed-default)) (datum (branch ...))))
          (typed
            (typed-type $typed-default)
            `(index-switch ,$index ,@$branches ,(typed-ref $typed-default)))))
      ((lambda (id typ) out)
        (symbol? (datum id))
        (lets
          ($id (datum id))
          ($type (parse-typed $env a-type (datum typ)))
          ($typed-out (parse (cons (cons $id $type) $env) (datum out)))
          (typed
            (arrow $type (typed-type $typed-out))
            `(lambda (,$id) ,(typed-ref $typed-out)))))
      ((fn arg)
        (lets
          ($typed-fn (parse $env (datum fn)))
          ($typed-arg (parse $env (datum arg)))
          (switch (typed-type $typed-fn)
            ((arrow? $arrow)
              (if (equal? (arrow-in $arrow) (typed-type $typed-arg))
                (typed
                  (arrow-out $arrow)
                  (switch (typed-ref $typed-fn)
                    ((abstraction? $abstraction)
                      (substitute (abstraction-body $abstraction) (typed-ref $typed-arg) 0))
                    ((else $other)
                      `(,(typed-ref $typed-fn) ,(typed-ref $typed-arg)))))
                (syntax-error #'arg "invalid type")))
            ((else $other)
              (syntax-error #'fn "not arrow")))))
      (s
        (symbol? (datum s))
        (typed (env-ref $env (datum s)) (datum s)))
      (_ (syntax-error $term))))

  (define (substitute $lhs $rhs $depth)
    (switch-exhaustive $lhs
      ((variable? $variable)
        (if (= (variable-index $variable) $depth)
          $rhs
          $variable))
      ((abstraction? $abstraction)
        (abstraction
          (substitute (abstraction-body $abstraction) $rhs (inc $depth))))
      ((application? $application)
        (application
          (substitute (application-lhs $application) $rhs $depth)
          (substitute (application-rhs $application) $rhs $depth)))))

  (define (compile-term $env $term)
    (switch-exhaustive $term
      ((index? $index) $index)
      ((string? $string) $string)
      ((a-type? $a-type) 'a-type)
      ((an-index? $an-index) 'an-index)
      ((a-string? $a-string) 'a-string)
      ((a-list? $a-list)
        `(a-list ,(compile-term $env (a-list-element $a-list))))
      ((arrow? $arrow)
        `(arrow
          ,(compile-term $env (arrow-in $arrow))
          ,(compile-term $env (arrow-out $arrow))))
      ((variable? $variable)
        (list-ref $env (variable-index $variable)))
      ((abstraction? $abstraction)
        (lets
          ($var (env->var $env))
          `(lambda (,$var)
            ,(compile-term (cons $var $env) (abstraction-body $abstraction)))))
      ((application? $application)
        `(
          ,(compile-term $env (application-lhs $application))
          ,(compile-term $env (application-rhs $application))))))
)
