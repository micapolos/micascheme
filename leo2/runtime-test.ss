(import
  (prefix (leo2 base) %)
  (leo2 runtime))

(check=?
  (variable #f %list)
  %list)

(check=?
  (type 0)
  #f)

(check=?
  (native #f 10)
  10)

(check=?
  (native-apply #f
    (native #f %string-append)
    (native #f "foo")
    (native #f "bar"))
  "foobar")

(check=?
  (apply
    (apply
      (lambda (x #f)
        (lambda (y #f)
          (native-apply #f
            (native #f %string-append)
            x
            y)))
      (native #f "foo"))
    (native #f "bar"))
  "foobar")

(check=?
  (a-lambda (_ (variable a-boolean)) (variable a-string))
  #f)

(check=?
  (a-lambda (x a-type) (apply x x))
  #f)

(check=?
  (apply
    (recursion fn #f
      (lambda (x #f)
        (if (native-apply #f (native #f %zero?) x)
          (native #f "Done")
          (apply fn (native-apply #f (native #f %-) x (native #f 1))))))
    (native #f 10))
  "Done")

(check=?
  (if (native #f #t) (native #f "foo") (native #f "bar"))
  "foo")

(check=?
  (if (native #f #f) (native #f "foo") (native #f "bar"))
  "bar")
