(import
  (prefix (leo2 base) %)
  (prefix (leo2 term) %)
  (only (leo2 base) quote)
  (leo2 comptime))

(check=?
  (variable x)
  (%variable 'x))

(check=?
  a-type
  (%type 0))

(check=?
  (native 10)
  (%native 10))

(check=?
  (native-apply
    (native %string-append)
    (native "foo")
    (variable x))
  (%native-application
    (%native %string-append)
    (%list
      (%native "foo")
      (%variable 'x))))

(check=?
  (lambda x x)
  (%abstraction (%lambda (x) x)))

(check=?
  (lambda x (lambda y (apply x y)))
  (%abstraction
    (%lambda (x)
      (%abstraction
        (%lambda (y)
          (%application x y))))))

(check=?
  (a-lambda (variable a-boolean) (variable a-string))
  (%abstraction-type (%variable 'a-boolean)
    (%lambda (_)
      (%variable 'a-string))))

(check=?
  (a-lambda (x : a-type) (apply x x))
  (%abstraction-type (%type 0)
    (%lambda (x)
      (%application x x))))

(check=?
  (recursive fn (lambda x (apply fn x)))
  (%recursive
    (%lambda (fn)
      (%abstraction
        (%lambda (x)
          (%application fn x))))))

(check=?
  (if (variable x) (native "foo") (native "bar"))
  (%branch
    (%variable 'x)
    (%native "foo")
    (%native "bar")))
