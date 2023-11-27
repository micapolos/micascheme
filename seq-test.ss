(import (list) (seq))

(define-syntax-rule (check-seq= $lhs $rhs)
  (check (equal? (seq->list $lhs) (seq->list $rhs))))

(check-seq= (seq) (seq))
(check-seq= (seq 1) (seq 1))
(check-seq= (seq 1 2) (seq 1 2))

(check-seq=
  (seq-append (seq 1 2) (seq 3 4))
  (seq 1 2 3 4))

(check-seq=
  (seq-map number->string (seq 1 2))
  (seq "1" "2"))

(check-seq=
  (seq-flat-map
    (lambda ($item) (seq $item (string-length $item)))
    (seq "a" "foo"))
  (seq "a" 1 "foo" 3))

(check-seq=
  (indexed-seq (seq "a" "b"))
  (seq (indexed "a" 0) (indexed "b" 1)))
