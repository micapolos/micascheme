(import (scheme) (check) (syntax) (procedure) (boolean) (lets))

(check (equal? (identifiers? #'()) #t))
(check (equal? (identifiers? #'(foo bar)) #t))
(check (equal? (identifiers? #'(foo 123)) #f))

(define-aux-keyword foo)
(check (raises foo))

(check (syntax-null? #'()))
(check (not (syntax-null? #'(1))))

(define (predicate-syntax s)
  (datum->syntax #'+
    (string->symbol
      (string-append
        (symbol->string s)
        "?"))))

(check (free-identifier=? (syntax-selector #'+) #'+))
(check (free-identifier=? (syntax-selector #'(+ a b)) #'+))
(check (false? (syntax-selector #'"foo")))
(check (false? (syntax-selector #'("foo"))))

(check (free-identifier=? (syntax-pattern-id #'+) #'+))
(check (free-identifier=? (syntax-pattern-id #'(+ a b)) #'+))

(check (free-identifier=? (syntax-rule-id #'(+ body)) #'+))
(check (free-identifier=? (syntax-rule-id #'((+ a b) body)) #'+))

(check (false? (syntax-case-opt #'foo ())))

(check
  (equal?
    (expand
      `(inline-indexed ($i 4) $i)
      (environment '(scheme) '(syntax)))
    `(begin 0 1 2 3)))

; --- syntax=?

(check (syntax=? #`10 #`10))
(check (not (syntax=? #`10 #`20)))

(check (syntax=? #`foo #`foo))
(check (not (syntax=? #`foo #`bar)))

(check (syntax=? #`"foo" #`"foo"))
(check (not (syntax=? #`"foo" #`"bar")))

(check (syntax=? #`() #`()))
(check (syntax=? #`(+) #`(+)))
(check (syntax=? #`(+ -) #`(+ -)))
(check (syntax=? #`(+ - *) #`(+ - *)))
(check (syntax=? #`(+ - . *) #`(+ - . *)))

(check (not (syntax=? #`() #`(+))))
(check (not (syntax=? #`(+) #`(-))))
(check (not (syntax=? #`(+) #`(+ -))))
(check (not (syntax=? #`(+ -) #`(+))))
(check (not (syntax=? #`(+ . -) #`(+ . *))))

(check
  (syntax=?
    (syntax-replace #'+ #'- #'(+ 10 (+ 20 (* 30 40))))
    #'(- 10 (- 20 (* 30 40)))))

(check
  (syntax=?
    (syntax-subst
      #'(a (b c))
      #'(10 (20 30))
      #'(+ a b c))
    #'(+ 10 20 30)))

(check (not (syntax-contains? #'() #'x)))
(check (syntax-contains? #'x #'x))
(check (syntax-contains? #'(x y) #'x))
(check (syntax-contains? #'(y x) #'x))
(check (not (syntax-contains? #'(x y) #'z)))

(check
  (equal?
    (syntax->datum
      (syntax-properties-ref*
        #'((prop1 value1) (prop2 value2) (prop1 value3))
        #'prop1))
    '(value1 value3)))

(check
  (equal?
    (syntax->datum
      (syntax-properties-ref*
        #'((prop1 value1) (prop2 value2) (prop1 value3))
        #'prop2))
    '(value2)))

(check
  (equal?
    (syntax->datum
      (syntax-properties-ref*
        #'((prop1 value1) (prop2 value2) (prop1 value3))
        #'prop3))
    '()))

(check
  (equal?
    (syntax->datum
      (syntax-properties-ref?
        #'((prop1 value1) (prop2 value2))
        #'prop1))
    'value1))

(check
  (equal?
    (syntax->datum
      (syntax-properties-ref?
        #'((prop1 value1) (prop2 value2))
        #'prop2))
    'value2))

(check
  (equal?
    (syntax-properties-ref?
      #'((prop1 value1) (prop2 value2))
      #'prop3)
    #f))

(check
  (equal?
    (syntax->datum
      (syntax-properties-ref
        #'((prop1 value1) (prop2 value2))
        #'prop1))
    'value1))

(check
  (equal?
    (syntax->datum
      (syntax-properties-ref
        #'((prop1 value1) (prop2 value2))
        #'prop2))
    'value2))

(check
  (equal?
    (syntax->datum
      (syntax-properties-add
        #'((prop1 value1) (prop2 value2))
        #'prop3
        #'value3))
    '((prop3 value3) (prop1 value1) (prop2 value2))))

(check
  (equal?
    (syntax->datum
      (syntax-properties-update
        #'((prop1 value1) (prop2 value2))
        #'prop2
        (lambda ($old) (if $old #`(updated #,$old) #`new))))
    '((prop1 value1) (prop2 (updated value2)))))

(check
  (equal?
    (syntax->datum
      (syntax-properties-update
        #'((prop1 value1) (prop2 value2))
        #'prop3
        (lambda ($old) (if $old #`(updated #,$old) #`new))))
    '((prop1 value1) (prop2 value2) (prop3 new))))

(check
  (equal?
    (syntax->datum
      (syntax-properties-update
        #'((prop1 value1) (prop2 value2))
        #'prop2
        (lambda (_) #f)))
    '((prop1 value1))))

(check
  (equal?
    (syntax->datum
      (syntax-properties-update
        #'((prop1 value1) (prop2 value2))
        #'prop3
        (lambda (_) #f)))
    '((prop1 value1) (prop2 value2))))

