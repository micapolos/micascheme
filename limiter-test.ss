(import (scheme) (check) (limited) (limiter) (boolean))

(check
  (limited=? string=?
    (limiter-apply
      (limiter-lets
        ($foo (limiter-using "foo" 3))
        ($exclamation (limiter-using "!" 1))
        (limiter (string-append $foo $exclamation)))
      100)
    (make-limited? "foo!" 96)))

(check
  (limited=? string=?
    (limiter-apply
      (limiter-lets
        ($foo (limiter-using "foo" 3))
        ($exclamation (limiter-using "!" 1))
        (limiter (string-append $foo $exclamation)))
      4)
    (make-limited? "foo!" 0)))

(check
  (false?
    (limiter-apply
      (limiter-lets
        ($foo (limiter-using "foo" 3))
        ($exclamation (limiter-using "!" 1))
        (limiter (string-append $foo $exclamation)))
      3)))

; === limiter-try

(check
  (limited=? equal?
    (limiter-apply (limiter-try) 10)
    (make-limited? #f 10)))

(check
  (limited=? equal?
    (limiter-apply
      (limiter-try (limiter-using "foo" 3))
      3)
    (make-limited? "foo" 0)))

(check
  (limited=? equal?
    (limiter-apply
      (limiter-try (limiter-using "foo" 3))
      2)
    (make-limited? #f 2)))

(check
  (limited=? equal?
    (limiter-apply
      (limiter-try
        (limiter-using "foobar" 6)
        (limiter-using "foo" 3))
      6)
    (make-limited? "foobar" 0)))

(check
  (limited=? equal?
    (limiter-apply
      (limiter-try
        (limiter-using "foobar" 6)
        (limiter-using "foo" 3))
      3)
    (make-limited? "foo" 0)))

(check
  (limited=? equal?
    (limiter-apply
      (limiter-try
        (limiter-using "foobar" 6)
        (limiter-using "foo" 3))
      2)
    (make-limited? #f 2)))





