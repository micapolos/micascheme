(library (micalang idris)
  (export parse evaluate)
  (import (micascheme))

  (define parse-environment (environment '(micascheme)))

  (define (index? $obj)
    (nonnegative-integer? $obj))

  (define (pi? $obj)
    (syntax-case? $obj (pi)
      ((pi in out) #t)))

  (define (env->var $env)
    (string->symbol
      (string-append "v" (number->string (length $env)))))

  (define (evaluate $env $term)
    (eval (cadr (parse $env $term)) parse-environment))

  (define (parse-typed $env $expected-type $term)
    (lets
      ($parsed (parse $env $term))
      (if (equal? (car $parsed) $expected-type)
        (cadr $parsed)
        (syntax-error $term "invalid type"))))

  (define (parse $env $term)
    (syntax-case $term (type index string pi inc switch var lambda)
      (type '(type type))
      (index '(type index))
      (string '(type string))
      ((pi in out)
        `(type (pi ,#'in ,#'out)))
      (n
        (number? (datum n))
        `(index ,(datum n)))
      (s
        (string? (datum s))
        `(string ,(datum s)))
      ((inc n)
        `(index (+ ,(parse-typed $env 'index #'n) 1)))
      ((switch idx branch ... default)
        (lets
          ($index (parse-typed $env 'index #'idx))
          ($parsed-default (parse $env #'default))
          ($branches (map (partial parse-typed $env (car $parsed-default)) #'(branch ...)))
          `(
            ,(car $parsed-default)
            (index-switch ,$index ,@$branches ,(cadr $parsed-default)))))
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
          ($var (env->var $env))
          ($entry `(,#'in ,$var))
          ($typed-out (parse (cons $entry $env) #'out))
          `(
            (pi ,#'in ,(car $typed-out))
            (lambda (,$var) ,(cadr $typed-out)))))
      ((fn arg)
        (lets
          ($typed-fn (parse $env #'fn))
          ($typed-arg (parse $env #'arg))
          (switch (car $typed-fn)
            ((pi? $pi)
              (if (equal? (cadr $pi) (car $typed-arg))
                `(
                  ,(caddr $pi)
                  (,(cadr $typed-fn) ,(cadr $typed-arg)))
                (syntax-error #'arg "invalid type")))
            ((else $other)
              (syntax-error #'fn "not func")))))
      (_ (syntax-error $term))))
)
