(import (micascheme) (leo transform))

(check
  (equal?
    (syntax->datum (transform-name #'foo))
    (syntax->datum '(foo))))

(check
  (equal?
    (syntax->datum (transform-name #'(foo bar)))
    (syntax->datum '(foo bar))))

(check
  (equal?
    (syntax->datum (transform-name #'(foo (bar goo))))
    (syntax->datum '(foo bar goo))))

(check
  (equal?
    (syntax->datum (transform-spec #'foo))
    (syntax->datum '(foo))))

(check
  (equal?
    (syntax->datum (transform-spec #'(foo bar)))
    (syntax->datum '(foo bar))))

(check
  (equal?
    (syntax->datum (transform-spec #'(foo (bar goo))))
    (syntax->datum '(foo bar goo))))

(check
  (equal?
    (syntax->datum (transform-spec #'(rename (foo (bar goo)) (a %a) (b %b))))
    (syntax->datum '(rename (foo bar goo) (a %a) (b %b)))))

(check
  (equal?
    (syntax->datum (transform-spec #'(only (foo (bar goo)) a b)))
    (syntax->datum '(only (foo bar goo) a b))))

(check
  (equal?
    (syntax->datum (transform-spec #'(except (foo (bar goo)) a b)))
    (syntax->datum '(except (foo bar goo) a b))))

(check
  (equal?
    (syntax->datum (transform-export #'(export foo bar)))
    (syntax->datum '(export foo bar))))

(check
  (equal?
    (syntax->datum
      (transform-import
        #'(import foo (foo bar) (foo (bar goo)))))
    (syntax->datum
      '(import (foo) (foo bar) (foo bar goo)))))

(check
  (equal?
    (syntax->datum
      (transform-library
        #'(library (foo (bar goo))
          (export foo bar)
          (import (zoo (zar zoo)))
          a b c)))
    (syntax->datum
      '(library
        (foo bar goo)
        (export foo bar)
        (import (zoo zar zoo))
        a b c))))

(check
  (equal?
    (syntax->datum
      (transform-define
        #'(define (x 10))))
    (syntax->datum
      '(define x 10))))

(check
  (equal?
    (syntax->datum
      (transform-define
        #'(define (comma-separated x y)
          (string-append x y))))
    (syntax->datum
      '(define (comma-separated x y)
        (string-append x y)))))

(check
  (equal?
    (syntax->datum
      (transform-lambda
        #'(lambda x y (+ x y))))
    (syntax->datum
      '(lambda (x y) (+ x y)))))

(check
  (equal?
    (syntax->datum (transform-with #'(with newline)))
    (syntax->datum '(newline))))

