(library (lim)
  (export lim? lim=? make-lim? lim+?)
  (import (scheme) (data))

  (data (lim number slack))

  (define (lim=? $lim-a $lim-b)
    (and
      (= (lim-number $lim-a) (lim-number $lim-b))
      (= (lim-slack $lim-a) (lim-slack $lim-b))))

  (define (make-lim? $number $slack)
    (and
      (nonnegative? $slack)
      (lim $number $slack)))

  (define (lim+? $lim $number)
    (make-lim?
      (+ (lim-number $lim) $number)
      (- (lim-slack $lim) $number)))
)
