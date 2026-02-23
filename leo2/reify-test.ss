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

(check-reify
  (labeled
    (quoted '(native 10))
    (native 10))
  10)

(check-reify
  (labeled
    (quoted '(native string-append))
    (native-application string-append
      (list
        (labeled
          (quoted '(native "foo"))
          (native "foo"))
        (labeled
          (quoted '(native "bar"))
          (native "bar")))))
  (string-append "foo" "bar"))
