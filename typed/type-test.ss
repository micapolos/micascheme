(import (micascheme) (typed type))

(check (equal? string-t string-t))
(check (equal? fx-t fx-t))
(check (not (equal? string-t fx-t)))

(check
  (equal?
    (lambda-t () fx-t)
    (lambda-t () fx-t)))
(check
  (equal?
    (lambda-t (string-t fx-t) fx-t)
    (lambda-t (string-t fx-t) fx-t)))

(check
  (not
    (equal?
      (lambda-t () fx-t)
      (lambda-t () string-t))))

(check
  (not
    (equal?
      (lambda-t (fx-t string-t) fx-t)
      (lambda-t (string-t fx-t) fx-t))))
