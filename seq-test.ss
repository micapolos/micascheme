(import (base) (seq))

(define-syntax-rule (check-seq-equal $lhs $rhs)
  (check (equal? (seq->list $lhs) (seq->list $rhs))))

(check-seq-equal (seq) (seq))
(check-seq-equal (seq 1) (seq 1))
(check-seq-equal (seq 1 2) (seq 1 2))

(check-seq-equal
  (seq-map number->string (seq 1 2))
  (seq "1" "2"))

(check-seq-equal
  (seq-flat-map
    (lambda ($item) (seq $item (string-length $item)))
    (seq "a" "foo"))
  (seq "a" 1 "foo" 3))
