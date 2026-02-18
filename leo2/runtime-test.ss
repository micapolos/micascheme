(import
  (prefix (leo2 base) %)
  (leo2 runtime))

(check=?
  (variable %list)
  %list)

(check=?
  (type 0)
  'erased)

(check=?
  (native 10)
  10)

(check=?
  (native-apply
    (native %string-append)
    (native "foo")
    (native "bar"))
  "foobar")

(check=?
  (apply
    (apply
      (lambda x
        (lambda y
          (native-apply
            (native %string-append)
            x
            y)))
      (native "foo"))
    (native "bar"))
  "foobar")

(check=?
  (a-lambda (_ (variable a-boolean)) (variable a-string))
  'erased)

(check=?
  (a-lambda (x a-type) (apply x x))
  'erased)

(check=?
  (apply
    (recursive fn
      (lambda x
        (if (native-apply (native %zero?) x)
          (native "Done")
          (apply fn (native-apply (native %-) x (native 1))))))
    10)
  "Done")

(check=?
  (if (native #t) (native "foo") (native "bar"))
  "foo")

(check=?
  (if (native #f) (native "foo") (native "bar"))
  "bar")
