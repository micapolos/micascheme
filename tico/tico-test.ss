(import
  (micascheme)
  (tico tico)
  (tico type))

; --- native-item

(check
  (equal?
    (native-item '(+ 1 2))
    (typed
      (native-type)
      (phased
        '(+ 1 2)
        (constant 3)))))

(check
  (raises?
    (lambda ()
      (native-item 'foo))))

; --- literal-item

(check
  (equal?
    (literal-item #f)
    (typed (boolean-type) (phased #f (constant #f)))))

(check
  (equal?
    (literal-item 128)
    (typed (number-type) (phased 128 (constant 128)))))

(check
  (equal?
    (literal-item #\a)
    (typed (char-type) (phased #\a (constant #\a)))))

(check
  (equal?
    (literal-item "foo")
    (typed (string-type) (phased "foo" (constant "foo")))))

(check (raises? (lambda () (literal-item `()))))

; --- struct-item

(check
  (equal?
    (struct-item 'foo (list))
    (typed
      (struct-type 'foo (list))
      #f)))

(check
  (equal?
    (struct-item 'foo (list (literal-item "foo")))
    (typed
      (struct-type 'foo (list (string-type)))
      (phased
        "foo"
        (constant "foo")))))

(check
  (equal?
    (struct-item 'foo (list (literal-item "foo") (literal-item 10)))
    (typed
      (struct-type 'foo (list (string-type) (number-type)))
      (phased
        `(cons "foo" 10)
        (constant (cons "foo" 10))))))

(check
  (equal?
    (struct-item 'foo (list (literal-item "foo") (literal-item 10) (native-item '+)))
    (typed
      (struct-type 'foo (list (string-type) (number-type) (native-type)))
      (phased
        `(vector "foo" 10 +)
        (constant (vector "foo" 10 +))))))

; --- item-compile

(check
  (equal?
    (item-compile
      (struct-item 'foo (list (literal-item 10) (literal-item "bar"))))
    (typed
      (struct-type 'foo (list (number-type) (string-type)))
      (phased
        `(cons 10 "bar")
        (constant (cons 10 "bar"))))))

; --- native

(check
  (equal?
    (tico-items
      (native
        "#f"
        "128"
        "\"foo\""
        "#\\a"
        "+"
        "(+ 1 2)"))
    (stack
      (native-item #f)
      (native-item 128)
      (native-item "foo")
      (native-item #\a)
      (native-item '+)
      (native-item '(+ 1 2)))))

; --- compile

(check
  (equal?
    (tico-items
      (compile
        #f
        128
        "foo"
        #\a
        x
        (x 10)
        (x 10 20)
        (x 10 20 30)
        (native "(string-append \"foo\" \"bar\")")))
    (stack
      (item-compile (literal-item #f))
      (item-compile (literal-item 128))
      (item-compile (literal-item "foo"))
      (item-compile (literal-item #\a))
      (item-compile (struct-item 'x (list)))
      (item-compile (struct-item 'x (list (literal-item 10))))
      (item-compile (struct-item 'x (list (literal-item 10) (literal-item 20))))
      (item-compile (struct-item 'x (list (literal-item 10) (literal-item 20) (literal-item 30))))
      (item-compile (native-item "foobar")))))

(check
  (raises?
    (lambda ()
      (tico-item (compile (native "string-append"))))))

; --- native apply

(check
  (equal?
    (tico-item (native "(lambda () 128)") apply)
    (typed
      (native-type)
      (phased
        '((lambda () 128))
        (constant 128)))))

(check
  (equal?
    (tico-item
      (native "-")
      (apply
        (native "3")
        (native "2")))
    (typed
      (native-type)
      (phased
        '(- 3 2)
        (constant 1)))))

(check
  (raises?
    (lambda ()
      (tico-item apply))))

(check
  (raises?
    (lambda ()
      (tico-item
        (native "foo")
        (native "bar")
        apply))))

; --- literals

(check
  (equal?
    (tico-item #f)
    (literal-item #f)))

(check
  (equal?
    (tico-item 128)
    (literal-item 128)))

(check
  (equal?
    (tico-item #\a)
    (literal-item #\a)))

(check
  (equal?
    (tico-item "foo")
    (literal-item "foo")))

; --- take

(check
  (equal?
    (tico-items take)
    (stack)))

(check
  (equal?
    (tico-items
      10 20
      (take 30 40))
    (stack
      (literal-item 10)
      (literal-item 20)
      (literal-item 30)
      (literal-item 40))))

; --- struct

(check
  (equal?
    (tico-items (struct))
    (stack)))

(check
  (equal?
    (tico-item (struct x))
    (struct-item 'x (list))))

(check
  (equal?
    (tico-item (struct (x 10)))
    (struct-item 'x (list (literal-item 10)))))

(check
  (equal?
    (tico-item (struct (x 10 "foo")))
    (struct-item 'x (list (literal-item 10) (literal-item "foo")))))

(check
  (equal?
    (tico-items (struct (x 10) (y "foo")))
    (stack
      (struct-item 'x (list (literal-item 10)))
      (struct-item 'y (list (literal-item "foo"))))))

; --- ordering

(check
  (equal?
    (tico-items #f 1 "foo")
    (stack
      (literal-item #f)
      (literal-item 1)
      (literal-item "foo"))))
