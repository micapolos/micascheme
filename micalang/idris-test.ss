(import (micascheme) (micalang idris))

; === normalize

(check
  (equal?
    (normalize (list) (native "foo") 1)
    (native "foo")))

(check
  (equal?
    (normalize (list (native "foo") hole (native "bar")) (variable 0) 1)
    (native "foo")))

(check
  (equal?
    (normalize (list (native "foo") hole (native "bar")) (variable 1) 1)
    (variable 0)))

(check
  (equal?
    (normalize (list (native "foo") hole (native "bar")) (variable 2) 1)
    (native "bar")))

(check
  (equal?
    (normalize (list) (application (abstraction (variable 0)) (native "foo")) 0)
    (native "foo")))

(check
  (equal?
    (normalize (list) (abstraction (application (abstraction (variable 0)) (native "foo"))) 0)
    (abstraction (native "foo"))))

(check
  (equal?
    (normalize (list) (abstraction (application (abstraction (variable 0)) (variable 0))) 0)
    (abstraction (variable 0))))

; === parse

(check
  (equal?
    (parse '() '(native string (string-append "foo" "bar")))
    (typed a-string '(string-append "foo" "bar"))))

(check
  (equal?
    (parse '() 'type)
    (typed a-type a-type)))

(check
  (equal?
    (parse '() 'index)
    (typed a-type an-index)))

(check
  (equal?
    (parse '() 'string)
    (typed a-type a-string)))

(check
  (equal?
    (parse '() '(arrow index string))
    (typed a-type (arrow an-index a-string))))

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
    (parse '() 'inc)
    (typed (arrow an-index an-index) 'inc)))

(check
  (equal?
    (parse '() 'inc)
    (typed (arrow an-index an-index) 'inc)))

(check
  (equal?
    (parse '() '(inc 0))
    (typed an-index '(inc 0))))

(check
  (equal?
    (parse '() 'add)
    (typed (arrow an-index (arrow an-index an-index)) 'add)))

(check
  (equal?
    (parse '() '((add 2) 3))
    (typed an-index '((add 2) 3))))

(check
  (equal?
    (parse '() '(switch (inc 0) "foo" "bar"))
    (typed a-string '(index-switch (inc 0) "foo" "bar"))))

(check
  (equal?
    (parse (list (cons 'x an-index)) 'x)
    (typed an-index 'x)))

(check
  (equal?
    (parse (list (cons 'x an-index) (cons 'y a-string)) 'x)
    (typed an-index 'x)))

(check
  (equal?
    (parse (list (cons 'x an-index) (cons 'y a-string)) 'y)
    (typed a-string 'y)))

(check
  (equal?
    (parse '() '(lambda (x index) (inc x)))
    (typed (arrow an-index an-index) '(lambda (x) (inc x)))))

(check
  (equal?
    (parse '() '((lambda (x index) (inc x)) 10))
    (typed an-index '((lambda (x) (inc x)) 10))))
