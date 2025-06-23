(import (micascheme) (typico expand))

(define (lookup $datum/annotation)
  (case (datum/annotation-stripped $datum/annotation)
    (($integer)
      (lambda ($lookup $datum/annotation)
        (case (datum/annotation-stripped $datum/annotation)
          (($integer)
            (typed integer-type '$local-integer))
          (else
            (default-expand-typed $lookup $datum/annotation)))))
    ((inc)
      (lambda ($lookup $datum/annotation)
        (case (datum/annotation-stripped $datum/annotation)
          ((inc)
            (typed
              (function-type (list integer-type) integer-type)
              'local-inc))
          (else
            (default-expand-typed $lookup $datum/annotation)))))
    (else
      (syntax-error $datum/annotation "unbound"))))

(check
  (equal?
    (expand-typed lookup #t)
    (typed boolean-type #t)))

(check
  (equal?
    (expand-typed lookup 123)
    (typed integer-type 123)))

(check
  (equal?
    (expand-typed lookup 123)
    (typed integer-type 123)))

(check
  (equal?
    (expand-typed lookup '$integer)
    (typed integer-type '$local-integer)))

(check
  (equal?
    (expand-typed lookup 'inc)
    (typed (function-type (list integer-type) integer-type) 'local-inc)))
