(import (scheme) (check) (emu dispatch))

(let ()
  (define $suffix "!")

  (define-dispatch name
    (string-append "zero" $suffix)
    (string-append "one" $suffix)
    (string-append "two" $suffix))

  (check (equal? (name 0) "zero!"))
  (check (equal? (name 1) "one!"))
  (check (equal? (name 2) "two!"))

  (set! $suffix "?")
  (check (equal? (name 0) "zero?"))
  (check (equal? (name 1) "one?"))
  (check (equal? (name 2) "two?")))
