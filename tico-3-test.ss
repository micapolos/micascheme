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

; --- thunk-compiler->symbolize

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-compiled
        (thunk-compiler->symbolize
          (compiler (thunk (variable 3) `(some-string))))))
    (compiled
      (stack (cons `$tmp-0 `(some-string)))
      (thunk (variable 3) `$tmp-0))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-compiled
        (thunk-compiler->symbolize
          (compiler (thunk (variable 3) `$string)))))
    (compiled (stack)
      (thunk (variable 3) `$string))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-compiled
        (thunk-compiler->symbolize
          (compiler (thunk (variable 3) "foo")))))
    (compiled (stack)
      (thunk (variable 3) "foo"))))

; --- datum->thunk

(check
  (equal?
    (syntax->thunk #`#f)
    (thunk #f #f)))

(check
  (equal?
    (syntax->thunk #`123)
    (thunk 123 123)))

(check
  (equal?
    (syntax->thunk #`"foo")
    (thunk "foo" "foo")))

(check
  (equal?
    (syntax->thunk #`string-append)
    (thunk string-append `string-append)))

(check
  (equal?
    (syntax->thunk #`(string-append "foo" "bar"))
    (thunk
      "foobar"
      `(string-append "foo" "bar"))))

(let (($thunk (syntax->thunk #`(lambda ($x $y) "foo"))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) "foo")))
  (check (equal? (app (thunk-value $thunk) 1 2) "foo")))

(let (($thunk (syntax->thunk #`(lambda ($x $y) $x))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) $x)))
  (check (equal? (app (thunk-value $thunk) "foo" "bar") "foo")))

(let (($thunk (syntax->thunk #`(lambda ($x $y) (string-append $x $y)))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) (string-append $x $y))))
  (check (equal? (app (thunk-value $thunk) "foo" "bar") "foobar")))

; TODO: symbolize!!!
(let (($thunk (syntax->thunk #`(lambda ($x $y) (string-append "foo" "bar")))))
  (check (equal? (thunk-datum $thunk) `(lambda ($x $y) (string-append "foo" "bar"))))
  (check (equal? (app (thunk-value $thunk) "goo" "gar") "foobar")))
