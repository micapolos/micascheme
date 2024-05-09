(import (micascheme) (minic runtime))

(define env (environment '(scheme) '(minic runtime)))

(check
  (equal?
    (expand '(int 7 const 'a) env)
    ''a))

(check
  (equal?
    (expand '(int 7 clamp 'a) env)
    '(($primitive 3 fxand) #x7f 'a)))

(check
  (equal?
    (expand '(int 7 add 'a 'b) env)
    '(($primitive 3 fxand) #x7f (($primitive 3 fx+/wraparound) 'a 'b))))

(check
  (equal?
    (expand '(int 7 sub 'a 'b) env)
    '(($primitive 3 fxand) #x7f (($primitive 3 fx-/wraparound) 'a 'b))))

(check
  (equal?
    (expand '(int 7 inc 'a) env)
    '(($primitive 3 fxand) #x7f (($primitive 3 fx+/wraparound) 'a 1))))

(check
  (equal?
    (expand '(int 7 dec 'a) env)
    '(($primitive 3 fxand) #x7f (($primitive 3 fx-/wraparound) 'a 1))))

(check (equal? (int 7 const #x43) #x43))
(check (equal? (int 7 clamp #x143) #x43))
(check (equal? (int 7 add #x40 #x50) #x10))
(check (equal? (int 7 sub #x40 #x50) #x70))
(check (equal? (int 7 inc #x7f) #x00))
(check (equal? (int 7 dec #x00) #x7f))
