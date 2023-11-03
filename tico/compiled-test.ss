(import
  (micascheme)
  (tico compiled))

(check
  (equal?
    (type-literal->compiled (number-type) 128)
    (compiled (globals)
      (typed (number-type)
        (packet
          (comptime 128)
          (runtime (constant 128)))))))

(check
  (equal?
    (boolean->compiled #f)
    (type-literal->compiled (boolean-type) #f)))

(check
  (equal?
    (number->compiled 128)
    (type-literal->compiled (number-type) 128)))

(check
  (equal?
    (string->compiled "foo")
    (type-literal->compiled (string-type) "foo")))

(check
  (equal?
    (literal->compiled #f)
    (boolean->compiled #f)))

(check
  (equal?
    (literal->compiled 128)
    (number->compiled 128)))

(check
  (equal?
    (literal->compiled "foo")
    (string->compiled "foo")))
