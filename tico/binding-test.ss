(import
  (micascheme)
  (tico binding)
  (tico typing)
  (tico type)
  (tico compilation)
  (tico variable)
  (tico layout)
  (tico layment))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (generate-parameter-binding (number-type)))
    (with-generate-temporary-seed $tmp
      (binding
        (generate-parameter-typing (number-type))))))

(check
  (equal?
    (binding-switch (binding (literal->typing "foo"))
      ((typing? $typing) (cons 'typing $typing))
      ((else $other) (cons 'other $other)))
    (cons 'typing (literal->typing "foo"))))

(check
  (equal?
    (binding-switch (binding (literal->typing "foo"))
      ((typing? $typing) (cons 'typing $typing))
      ((else $other) (cons 'other $other)))
    (cons 'typing (literal->typing "foo"))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (bindings-match
        (stack
          (generate-parameter-binding (number-type))
          (generate-parameter-binding (string-type)))
        (number-type)))
    (typing (number-type)
      (layment (type->layout (number-type))
        (compilation '$tmp-1
          (variable 1 (stack)))))))

(check
  (equal?
    (with-tmps
      (bindings-match
        (reverse
          (list
            (binding (typing-parameter (literal->typing 128)))
            (generate-parameter-binding (string-type))))
        (number-type)))
    (with-tmps
      (typing-variable (typing-parameter (literal->typing 128)) 1))))
