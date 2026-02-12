(import
  (except (micalang base) syntax-eval)
  (micalang syntax-eval))

(define (syntax-eval $syntax)
  (default-lookup-syntax-eval
    (lambda ($default $lookup $syntax)
      (syntax-case $syntax (+)
        (n (number? (datum n)) #'n)
        (((+ a) b)
          (and (number? (datum a)) (number? (datum b)))
          (literal->syntax (+ (datum a) (datum b))))
        (other #'other)))
    (lambda ($id)
      (syntax-case $id (+)
        (+ #'+)
        (_ (syntax-error $id "undefined"))))
    $syntax))

(define-rule-syntax (check-evals in out)
  (check (equal? (syntax->datum (syntax-eval #'in)) 'out)))

(define-rule-syntax (check-eval-error in)
  (check (raises (syntax-eval #'in))))

(check-evals #t #t)
(check-evals #f #f)
(check-evals 123 123)

(check-evals ((lambda x x) #t) #t)

(check-evals
  ((lambda x ((+ x) 2)) 1)
  3)
