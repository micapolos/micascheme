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
