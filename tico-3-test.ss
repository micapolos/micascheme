(import (micascheme) (tico-3))

(define env (environment `(micascheme)))

(check
  (equal?
    (scope-value (scope env (stack)) `+)
    (constant +)))

(check
  (raises?
    (lambda ()
      (scope-value
        (scope env (stack)) `foo))))

(check
  (equal?
    (scope-value
      (scope env
        (stack
          (cons `+ (constant "foo"))))
      `+)
    (constant "foo")))

(check
  (equal?
    (scope-value
      (scope env
        (stack
          (cons `v0 (hole))
          (cons `+ (hole))
          (cons `v1 (hole))
          (cons `v2 (hole))))
      `+)
    (variable 2)))

; --- compiler

(check
  (equal?
    (app
      (compiler `(+ a b))
      (stack (cons `a "a") (cons `b "b")))
    (compiled
      (stack (cons `a "a") (cons `b "b"))
      `(+ a b))))

(check
  (equal?
    (app
      (compiler+binding
        (compiler `(+ a b))
        (cons `c "c"))
      (stack (cons `a "a") (cons `b "b")))
    (compiled
      (stack (cons `a "a") (cons `b "b") (cons `c "c"))
      `(+ a b))))

(check
  (equal?
    (compiler-compiled
      (compilers-flatten
        (list
          (compiler+binding
            (compiler 10)
            (cons `a "a"))
          (compiler+binding
            (compiler 20)
            (cons `b "b")))))
    (compiled
      (stack (cons `a "a") (cons `b "b"))
      `(10 20))))

; --- datum->thunk

(check
  (equal?
    (syntax->thunk #`#f)
    (thunk (constant #f) (stack) #f)))

(check
  (equal?
    (syntax->thunk #`123)
    (thunk (constant 123) (stack) 123)))

(check
  (equal?
    (syntax->thunk #`"foo")
    (thunk (constant "foo") (stack) "foo")))

(check
  (equal?
    (syntax->thunk #`string-append)
    (thunk (constant string-append) (stack) `string-append)))

(check
  (equal?
    (syntax->thunk #`(string-append "foo" "bar"))
    (thunk
      (constant "foobar")
      (stack)
      `(string-append "foo" "bar"))))
