(import
  (leo2 base)
  (leo2 term)
  (leo2 reify))

(check-reify
  (lambda (x) (lambda (y) (application x y)))
  (lambda (v0) (lambda (v1) (v0 v1))))

(check-reify
  (recursion (lambda (x) (lambda (y) (application x y))))
  (letrec ((v0 (lambda (v1) (v0 v1)))) v0))

(check-reify
  (branch (variable 0) (variable 1) (variable 2))
  (if v0 v1 v2))

(check-reify (native #f) #f)
(check-reify (native #t) #t)
(check-reify (native 123) 123)
(check-reify (native #\a) #\a)
(check-reify (native "foo") "foo")

(check-reify
  (labeled (native 'string-append) (native string-append))
  string-append)

(check-reify
  (labeled
    (native 'string-append)
    (native-application string-append
      (list (native "foo") (native "bar"))))
  (string-append "foo" "bar"))
