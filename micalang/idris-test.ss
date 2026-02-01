(import (micascheme) (micalang idris))

(check
  (equal?
    (parse '() 'type)
    '(type type)))

(check
  (equal?
    (parse '() 'index)
    '(type index)))

(check
  (equal?
    (parse '() 'string)
    '(type string)))

(check
  (equal?
    (parse '() '(pi index string))
    '(type (pi index string))))

(check
  (equal?
    (parse '() 0)
    '(index 0)))

(check
  (equal?
    (parse '() "foo")
    '(string "foo")))

(check
  (equal?
    (parse '() '(inc 0))
    '(index (+ 0 1))))

(check
  (equal?
    (parse '() '(switch (inc 0) "foo" "bar"))
    '(string (index-switch (+ 0 1) "foo" "bar"))))

(check
  (equal?
    (parse '((index 10)) '(var 0))
    '(index 10)))

(check
  (equal?
    (parse '((index 10) (string "foo")) '(var 0))
    '(index 10)))

(check
  (equal?
    (parse '((index 10) (string "foo")) '(var 1))
    '(string "foo")))

(check
  (equal?
    (parse '() '(lambda index (inc (var 0))))
    `((pi index index) (lambda (v0) (+ v0 1)))))

(check
  (equal?
    (parse '() '((lambda index (inc (var 0))) 10))
    `(index ((lambda (v0) (+ v0 1)) 10))))
