(import (micascheme) (micalang expand))

(check
  (equal?
    (expand-typed '() "foo")
    '(string "foo")))

(check
  (equal?
    (expand-typed '() 123)
    '(number 123)))

(check
  (equal?
    (expand-typed '() 'foo)
    '('foo #f)))

(check
  (equal?
    (expand-typed '((number n) (string s)) '(the number))
    '(number n)))

(check
  (equal?
    (expand-typed '((number n) (string s)) '(the string))
    '(string s)))

(check
  (raises
    (expand-typed '((number n) (string s)) '(the dupa))))

(check
  (equal?
    (parameterize ((var-count 0))
      (expand-typed '() '(lambda (number string) (the number))))
    '(
      (lambda (number string) number)
      (lambda (v0 v1) v0))))

(check
  (equal?
    (parameterize ((var-count 0))
      (expand-typed '() '(lambda (number string) (the string))))
    '(
      (lambda (number string) string)
      (lambda (v0 v1) v1))))
