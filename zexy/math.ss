(library (zexy math)
  (export
    band bxor bor shl shr msb lsb inc-nm nm+
    n$ nm$)
  (import (micascheme))

  (define-syntax bxor (identifier-syntax bitwise-xor))
  (define-syntax bor (identifier-syntax bitwise-ior))
  (define-syntax band (identifier-syntax bitwise-and))
  (define-syntax shl (identifier-syntax bitwise-arithmetic-shift-left))
  (define-syntax shr (identifier-syntax bitwise-arithmetic-shift-right))
  (define (msb $nm) (band (shr $nm 8) #xff))
  (define (lsb $nm) (band $nm #xff))
  (define (inc-nm $nm) (band (add1 $nm) #xffff))
  (define (nm+ $nm $nm2) (band (+ $nm $nm2) #xffff))

  (define (n$ n)
    (string
      #\$
      (fx-hex-char (fxsrl n 4))
      (fx-hex-char n)))

  (define (nm$ nm)
    (string
      #\$
      (fx-hex-char (fxsrl nm 12))
      (fx-hex-char (fxsrl nm 8))
      (fx-hex-char (fxsrl nm 4))
      (fx-hex-char nm)))

  (define (fx-hex-char fx)
    (case1 (fxand fx #xf)
      (#x0 #\0)
      (#x1 #\1)
      (#x2 #\2)
      (#x3 #\3)
      (#x4 #\4)
      (#x5 #\5)
      (#x6 #\6)
      (#x7 #\7)
      (#x8 #\8)
      (#x9 #\9)
      (#xa #\A)
      (#xb #\B)
      (#xc #\C)
      (#xd #\D)
      (#xe #\E)
      (#xf #\F)))
)
