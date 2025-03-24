(import (micascheme) (typed scope) (typed compiled) (typed combo))

(check
  (equal?
    (compiled-combo
      (environment '(scheme))
      (compiled
        (scope
          (foo (combo "foo" '(string #\f #\o #\o)))
          (bar (combo "bar" '(string #\b #\a #\r))))
        '(string-append foo bar)))
    (combo
      "foobar"
      '(lets
        (bar (string #\b #\a #\r))
        (foo (string #\f #\o #\o))
        (string-append foo bar)))))

(check (equal? (combo-compiled (combo #t 'bool)) (compiled (scope) 'bool)))
(check (equal? (combo-compiled (combo #\a 'char)) (compiled (scope) 'char)))
(check (equal? (combo-compiled (combo 123 'num)) (compiled (scope) 'num)))
(check (equal? (combo-compiled (combo "foo" 'str)) (compiled (scope) 'str)))
(check (equal? (combo-compiled (combo 'foo 'symb)) (compiled (scope) 'symb)))

(check
  (equal?
    (combo-compiled (combo (cons "foo" "bar") '(cons "foo" "bar")))
    (compiled (scope) '(cons "foo" "bar"))))

(check
  (equal?
    (combine-compiled-list
      (list
        (compiled
          (scope
            (foo-1 (combo "foo-1" 'foo-1))
            (foo-2 (combo "foo-2" 'foo-2)))
          '(string-append foo-1 foo-2))
        (compiled
          (scope
            (bar-1 (combo "bar-1" 'bar-1))
            (bar-2 (combo "bar-2" 'bar-2)))
          '(string-append bar-1 bar-2)))
      (lambda ($datums)
        `(string-append ,@$datums)))
    (compiled
      (scope
        (foo-1 (combo "foo-1" 'foo-1))
        (foo-2 (combo "foo-2" 'foo-2))
        (bar-1 (combo "bar-1" 'bar-1))
        (bar-2 (combo "bar-2" 'bar-2)))
      '(string-append
        (string-append foo-1 foo-2)
        (string-append bar-1 bar-2)))))
