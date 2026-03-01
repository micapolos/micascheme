(import
  (leo2 base)
  (leo2 term)
  (leo2 reify))

(check-reify
  (lambda (x) (lambda (y) (application x y)))
  (lambda ($0) (lambda ($1) ($0 $1))))

(check-reify
  (recursion (lambda (fn) (lambda (x) (application fn x))))
  (letrec (($0 (lambda ($1) ($0 $1)))) $0))

(check-reify
  (branch (variable 0) (variable 1) (variable 2))
  (if $0 $1 $2))

; self-labeled literals
(check-reify (native #f) #f)
(check-reify (native #t) #t)
(check-reify (native 123) 123)
(check-reify (native #\a) #\a)
(check-reify (native "foo") "foo")

; labeled native lambdas
(check-reify
  (labeled (native 'string-append) (native string-append))
  string-append)

(check-reify
  (labeled
    (native 'string-append)
    (native-application string-append
      (list (native "foo") (native "bar"))))
  (string-append "foo" "bar"))
