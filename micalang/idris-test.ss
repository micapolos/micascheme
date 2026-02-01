(import (micascheme) (micalang idris))

(check
  (equal?
    (parse '() 'type)
    (typed a-type 'a-type)))

(check
  (equal?
    (parse '() 'index)
    (typed a-type 'an-index)))

(check
  (equal?
    (parse '() 'string)
    (typed a-type 'a-string)))

(check
  (equal?
    (parse '() '(arrow (index string) string))
    (typed a-type '(arrow (list an-index a-string) a-string))))

(check
  (equal?
    (parse '() 0)
    (typed an-index 0)))

(check
  (equal?
    (parse '() "foo")
    (typed a-string "foo")))

(check
  (equal?
    (parse '() '(inc 0))
    (typed an-index '(inc 0))))

(check
  (equal?
    (parse '() '(+ 1 2))
    (typed an-index '(+ 1 2))))

(check
  (equal?
    (parse '() '(switch (inc 0) "foo" "bar"))
    (typed a-string '(index-switch (inc 0) "foo" "bar"))))

(check
  (equal?
    (parse (list (typed an-index 'x)) '(var 0))
    (typed an-index 'x)))

(check
  (equal?
    (parse (list (typed an-index 'x) (typed a-string 'y)) '(var 0))
    (typed an-index 'x)))

(check
  (equal?
    (parse (list (typed an-index 'x) (typed a-string 'y)) '(var 1))
    (typed a-string 'y)))

(check
  (equal?
    (parse '() '(lambda (index) (inc (var 0))))
    (typed (arrow (list an-index) an-index) '(lambda (v0) (inc v0)))))

(check
  (equal?
    (parse '() '((lambda (index) (inc (var 0))) 10))
    (typed an-index '((lambda (v0) (inc v0)) 10))))
