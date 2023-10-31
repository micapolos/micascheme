(import
  (micascheme)
  (tico tico)
  (tico type))

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

; --- quote

(check
  (equal?
    (tico-item (quote 128))
    (typed
      (value-type 128)
      #f)))

(check
  (equal?
    (tico-item (quote (x 10 20)))
    (typed
      (value-type (struct-type 'x (list 10 20)))
      #f)))

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
