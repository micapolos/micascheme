(import
  (leo2 base)
  (leo2 stdlib)
  (leo2 equal)
  (leo2 type-of))

(check-term=?
  (type-of
    (abstraction-term string-type
      (lambda (x)
        (abstraction-term string-type
          (lambda (y)
            (native-application-term string-type string-append x y))))))
  (abstraction-type-term string-type
    (lambda (_)
      (abstraction-type-term string-type
        (lambda (_)
          string-type)))))

(check-term=?
  (type-of
    (application-term
      (application-term
        (abstraction-term string-type
          (lambda (x)
            (abstraction-term string-type
              (lambda (y)
                (native-application-term string-type string-append x y)))))
        (string-term "foo"))
      (string-term "bar")))
  string-type)
