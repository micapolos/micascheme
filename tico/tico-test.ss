(import
  (micascheme)
  (tico tico)
  (tico type))

; --- native->item

(check
  (equal?
    (native->item '(+ 1 2))
    (typed
      (native-type)
      (phased
        '(+ 1 2)
        (constant 3)))))

(check
  (raises?
    (lambda ()
      (native->item 'foo))))

; --- literal->item

(check
  (equal?
    (literal->item #f)
    (typed (boolean-type) (phased #f (constant #f)))))

(check
  (equal?
    (literal->item 128)
    (typed (number-type) (phased 128 (constant 128)))))

(check
  (equal?
    (literal->item #\a)
    (typed (char-type) (phased #\a (constant #\a)))))

(check
  (equal?
    (literal->item "foo")
    (typed (string-type) (phased "foo" (constant "foo")))))

(check (raises? (lambda () (literal->item `()))))

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
      (native->item #f)
      (native->item 128)
      (native->item "foo")
      (native->item #\a)
      (native->item '+)
      (native->item '(+ 1 2)))))

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
    (literal->item #f)))

(check
  (equal?
    (tico-item 128)
    (literal->item 128)))

(check
  (equal?
    (tico-item #\a)
    (literal->item #\a)))

(check
  (equal?
    (tico-item "foo")
    (literal->item "foo")))

; --- struct

(check
  (equal?
    (tico-items (struct))
    (stack)))

(check
  (equal?
    (tico-item (struct x))
    (typed
      (struct-type 'x (list))
      #f)))

(check
  (equal?
    (tico-item (struct (x 10)))
    (typed
      (struct-type 'x (list (number-type)))
      (phased 10 (constant 10)))))

(check
  (equal?
    (tico-item (struct (x 10 "foo")))
    (typed
      (struct-type 'x (list (number-type) (string-type)))
      (phased `(cons 10 "foo") (constant (cons 10 "foo"))))))

(check
  (equal?
    (tico-item (struct (x 10 "foo" #f)))
    (typed
      (struct-type 'x (list (number-type) (string-type) (boolean-type)))
      (phased
        `(vector 10 "foo" #f)
        (constant (vector 10 "foo" #f))))))

(check
  (equal?
    (tico-items (struct (x 10) (y "foo")))
    (stack
      (typed
        (struct-type 'x (list (number-type)))
        (phased 10 (constant 10)))
      (typed
        (struct-type 'y (list (string-type)))
        (phased "foo" (constant "foo"))))))

; --- ordering

(check
  (equal?
    (tico-items #f 1 "foo")
    (stack
      (literal->item #f)
      (literal->item 1)
      (literal->item "foo"))))
