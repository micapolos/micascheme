(import (micascheme) (minic runtime))

(define env (environment '(scheme) '(minic runtime)))

(check
  (equal?
    (expand '(i+/wrap 16 'a 'b) env)
    '(($primitive 3 fxand) #xffff
      (($primitive 3 fx+/wraparound) 'a 'b))))

(check
  (equal?
    (expand '(i-/wrap 16 'a 'b) env)
    '(($primitive 3 fxand) #xffff
      (($primitive 3 fx-/wraparound) 'a 'b))))

(check
  (equal?
    (expand '(i+1/wrap 16 'a) env)
    (expand '(i+/wrap 16 'a 1) env)))

(check
  (equal?
    (expand '(i-1/wrap 16 'a) env)
    (expand '(i-/wrap 16 'a 1) env)))

(check (equal? (i+/wrap 8 #x80 #x90) #x10))
(check (equal? (i-/wrap 8 #x80 #x90) #xf0))
(check (equal? (i+1/wrap 8 #xff) #x00))
(check (equal? (i-1/wrap 8 #x00) #xff))
