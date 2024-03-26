(import (check) (syntax-matcher) (micascheme))

(define-aux-keywords b c d e h ixh iyh l ixl iyl hl ix iy a)

(define-rule-syntax (match $expr ...)
  (lambda ($fn)
    ($fn $expr ...)))

(define-syntax-matcher r
  (lambda ($syntax)
    (syntax-case $syntax (+ b c d e h ixh iyh l ixl iyl hl ix iy a)
      (b         (match #f   #b000 #f))
      (c         (match #f   #b001 #f))
      (d         (match #f   #b010 #f))
      (e         (match #f   #b011 #f))
      (h         (match #f   #b100 #f))
      (ixh       (match #xdd #b100 #f))
      (iyh       (match #xfd #b100 #f))
      (l         (match #f   #b101 #f))
      (ixl       (match #xdd #b101 #f))
      (iyl       (match #xfd #b101 #f))
      ((hl)      (match #f   #b110 #f))
      ((+ ix $d) (and (fixnum? (datum $d)) (match #xdd #b110 (datum $d))))
      ((+ iy $d) (and (fixnum? (datum $d)) (match #xfd #b110 (datum $d))))
      (a         (match #f   #b111 #f))
      (_         #f))))

(define-rule-syntax (check-matches? $syntax $result)
  (check
    (equal?
      (lets
        ($match (app (syntax-matcher r) (syntax $syntax)))
        (if $match
          ($match
            (lambda ($prefix $u3 $d)
              (list $prefix $u3 $d)))
          (syntax-error (syntax $syntax))))
      $result)))

(check-matches? b (list #f #b000 #f))
(check-matches? (hl) (list #f #b110 #f))
(check-matches? (+ ix 12) (list #xdd #b110 12))
; (check-matches? (+ ix "foo") 123)
