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

; === syntax-datum=? ===

(check (syntax-datum=? #'(a b) #'(a b)))
(check (not (syntax-datum=? #'(a b) #'(a c))))

; === null? cons car cdr ===

(check (syntax=? (null-syntax) #'()))

(check (syntax-null? #'()))
(check (not (syntax-null? #'a)))
(check (not (syntax-null? #'(a))))

(check (syntax=? (syntax-cons #'a #'b) #'(a . b)))
(check (syntax=? (syntax-car #'(a . b)) #'a))
(check (syntax=? (syntax-cdr #'(a . b)) #'b))

(check (syntax=? (syntax-ref? #'((a . b) (c . d)) #'a) #'b))
(check (syntax=? (syntax-ref? #'((a . b) (c . d)) #'c) #'d))
(check (not (syntax-ref? #'((a . b) (c . d)) #'e)))

(check (syntax=? (syntax-ref #'((a . b) (c . d)) #'a) #'b))
(check (syntax=? (syntax-ref #'((a . b) (c . d)) #'c) #'d))
(check (raises (syntax-ref #'((a . b) (c . d)) #'e)))

(check (syntax=? (syntax-ref* #'((a . b) (c . d)) #'a) #'(b)))
(check (syntax=? (syntax-ref* #'((a . b) (c . d)) #'c) #'(d)))
(check (syntax=? (syntax-ref* #'((a . b) (c . d)) #'e) #'()))
(check (raises (syntax-ref* #'((a . b) (c . d) (a . f)) #'(b f))))

(check (syntax=? (syntax-add #'() #'a #'b) #'((a . b))))
(check (syntax=? (syntax-add #'((a . b)) #'c #'d) #'((c . d) (a . b))))

(check (syntax=? (syntax-remove #'() #'a) #'()))
(check (syntax=? (syntax-remove #'((a . b)) #'a) #'()))
(check (syntax=? (syntax-remove #'((a . b) (c . d)) #'a) #'((c . d))))
(check (syntax=? (syntax-remove #'((a . b) (c . d) (a . f)) #'a) #'((c . d) (a . f))))

(check (syntax=? (syntax-set #'() #'a #'b) #'((a . b))))
(check (syntax=? (syntax-set #'((a . d)) #'a #'b) #'((a . b))))
(check (syntax=? (syntax-set #'((c . d)) #'a #'b) #'((c . d) (a . b))))
(check (syntax=? (syntax-set #'((c . d) (a . f)) #'a #'b) #'((c . d) (a . b))))
(check (syntax=? (syntax-set #'((a . b) (c . d) (a . f)) #'a #'g) #'((a . g) (c . d) (a . f))))

(define (update-fn $a) (if $a #`(updated #,$a) #f))
(check (syntax=? (syntax-update #'() #'a update-fn) #'()))
(check (syntax=? (syntax-update #'((a . b)) #'a update-fn) #'((a . (updated b)))))
(check (syntax=? (syntax-update #'((a . b) (c . d)) #'a update-fn) #'((a . (updated b)) (c . d))))
(check (syntax=? (syntax-update #'((a . b) (c . d) (a . g)) #'a update-fn) #'((a . (updated b)) (c . d) (a . g))))

(define (remove-fn $a) #f)
(check (syntax=? (syntax-update #'() #'a remove-fn) #'()))
(check (syntax=? (syntax-update #'((a . b)) #'a remove-fn) #'()))
(check (syntax=? (syntax-update #'((a . b) (c . d)) #'a remove-fn) #'((c . d))))
(check (syntax=? (syntax-update #'((a . b) (c . d) (a . g)) #'a remove-fn) #'((c . d) (a . g))))

(define (update-or-new-fn $a) (if $a #`(updated #,$a) #'new))
(check (syntax=? (syntax-update #'() #'a update-or-new-fn) #'((a . new))))
(check (syntax=? (syntax-update #'((a . b)) #'a update-or-new-fn) #'((a . (updated b)))))
(check (syntax=? (syntax-update #'((a . b) (c . d)) #'a update-or-new-fn) #'((a . (updated b)) (c . d))))
(check (syntax=? (syntax-update #'((a . b) (c . d) (a . g)) #'a update-or-new-fn) #'((a . (updated b)) (c . d) (a . g))))
(check (syntax=? (syntax-update #'((a . b)) #'e update-or-new-fn) #'((a . b) (e . new))))

(check (syntax-false? #'#f))
(check (not (syntax-false? #'#t)))
(check (not (syntax-false? #'())))
(check (not (syntax-false? #'a)))

(check (syntax=? (syntax-and #'#f #'#f) #'#f))
(check (syntax=? (syntax-and #'foo #'#f) #'#f))
(check (syntax=? (syntax-and #'#f #'bar) #'#f))
(check (syntax=? (syntax-and #'foo #'bar) #'bar))

(check (syntax=? (syntax-or #'#f #'#f) #'#f))
(check (syntax=? (syntax-or #'foo #'#f) #'foo))
(check (syntax=? (syntax-or #'#f #'bar) #'bar))
(check (syntax=? (syntax-or #'foo #'bar) #'foo))

(check-datum=? (syntaxes->syntax (list)) #`(begin))
(check-datum=? (syntaxes->syntax (list #'foo)) #`foo)
(check-datum=? (syntaxes->syntax (list #'foo #'bar)) #`(begin foo bar))
