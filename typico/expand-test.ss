(import (micascheme) (typico expand))

(define (lookup $datum/annotation)
  (case (datum/annotation-stripped $datum/annotation)
    (($integer)
      (lambda ($lookup $datum/annotation)
        (case (datum/annotation-stripped $datum/annotation)
          (($integer)
            (typed integer-type '$resolved-integer))
          (else
            (default-expand-typed $lookup $datum/annotation)))))
    ((inc)
      (lambda ($lookup $datum/annotation)
        (case (datum/annotation-stripped $datum/annotation)
          ((inc)
            (typed
              (function-type (list integer-type) integer-type)
              'resolved-inc))
          (else
            (default-expand-typed $lookup $datum/annotation)))))
    ((-)
      (lambda ($lookup $datum/annotation)
        (case (datum/annotation-stripped $datum/annotation)
          ((-)
            (typed
              (function-type (list* integer-type integer-type) integer-type)
              'resolved-))
          (else
            (default-expand-typed $lookup $datum/annotation)))))
    ((macro)
      (lambda ($lookup $datum/annotation)
        (case (datum/annotation-stripped $datum/annotation)
          ((macro)
            (typed integer-type '(expanded macro)))
          (else
            (typed integer-type `(expanded ,(datum/annotation-stripped $datum/annotation)))))))
    (else
      (syntax-error $datum/annotation "unbound"))))

(check
  (equal?
    (expand-typed lookup (datum/annotation #t))
    (typed boolean-type #t)))

(check
  (equal?
    (expand-typed lookup (datum/annotation 123))
    (typed integer-type 123)))

(check
  (equal?
    (expand-typed lookup (datum/annotation 123))
    (typed integer-type 123)))

(check
  (equal?
    (expand-typed lookup (datum/annotation $integer))
    (typed integer-type '$resolved-integer)))

(check
  (raises
    (expand-typed lookup (datum/annotation ($integer)))))

(check
  (equal?
    (expand-typed lookup (datum/annotation inc))
    (typed (function-type (list integer-type) integer-type) 'resolved-inc)))

(check
  (equal?
    (expand-typed lookup (datum/annotation (inc $integer)))
    (typed integer-type '(resolved-inc $resolved-integer))))

(check
  (raises
    (expand-typed lookup (datum/annotation (inc)))))

(check
  (raises
    (expand-typed lookup (datum/annotation (inc $boolean)))))

(check
  (raises
    (expand-typed lookup (datum/annotation (inc $integer $integer)))))

(check
  (equal?
    (expand-typed lookup (datum/annotation -))
    (typed (function-type (list* integer-type integer-type) integer-type) 'resolved-)))

(check
  (raises
    (expand-typed lookup (datum/annotation (-)))))

(check
  (equal?
    (expand-typed lookup (datum/annotation (- $integer)))
    (typed integer-type '(resolved- $resolved-integer))))

(check
  (equal?
    (expand-typed lookup (datum/annotation (- $integer $integer)))
    (typed integer-type '(resolved- $resolved-integer $resolved-integer))))

(check
  (raises
    (expand-typed lookup (datum/annotation (- $boolean)))))

(check
  (raises
    (expand-typed lookup (datum/annotation (- $boolean $integer)))))

(check
  (equal?
    (expand-typed lookup (datum/annotation macro))
    (typed integer-type '(expanded macro))))

(check
  (equal?
    (expand-typed lookup (datum/annotation (macro 123)))
    (typed integer-type '(expanded (macro 123)))))
