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
        (+ 1 2)
        (string-append "foo" "bar")))
    (stack
      (native->item '(+ 1 2))
      (native->item '(string-append "foo" "bar")))))

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

; --- ordering

(check
  (equal?
    (tico-items #f 1 "foo")
    (stack
      (literal->item #f)
      (literal->item 1)
      (literal->item "foo"))))
