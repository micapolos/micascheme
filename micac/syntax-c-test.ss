(import (micascheme) (micac syntax) (micac syntax-c))

(define-aux-keywords micac lines)

(define-syntax check-c-string
  (syntax-rules (micac lines)
    ((_ (micac instr ...) string)
      (check (equal? (syntax-c #'(instr ...)) string)))))

(check-c-string
  (micac)
  (lines-string))

(check-c-string
  (micac (const int x 10))
  (lines-string "const int x = 10;"))

(check-c-string
  (micac (var int x))
  (lines-string "int x;"))

(check-c-string
  (micac (var int some-variable))
  (lines-string "int some_variable;"))

(check-c-string
  (micac (var (* uint8_t) x))
  (lines-string "uint8_t* x;"))

(check-c-string
  (micac (var uint8_t (* x)))
  (lines-string "uint8_t *x;"))

(check-c-string
  (micac (var uint8_t (* (* x))))
  (lines-string "uint8_t **x;"))

(check-c-string
  (micac (var uint8_t (* x 24)))
  (lines-string "uint8_t x[24];"))

(check-c-string
  (micac (var uint8_t (* (* x 24) 32)))
  (lines-string "uint8_t x[24][32];"))

(check-c-string
  (micac (var int x y))
  (lines-string "int x = y;"))

(check-c-string
  (micac (begin))
  (lines-string "{" "}"))

(check-c-string
  (micac (set x 10))
  (lines-string "x = 10;"))

(check-c-string
  (micac (set x + 10))
  (lines-string "x += 10;"))

(check-c-string
  (micac (set x - 10))
  (lines-string "x -= 10;"))

(check-c-string
  (micac (set x * 10))
  (lines-string "x *= 10;"))

(check-c-string
  (micac (set x div 10))
  (lines-string "x /= 10;"))

(check-c-string
  (micac (set x and 10))
  (lines-string "x &&= 10;"))

(check-c-string
  (micac (set x or 10))
  (lines-string "x ||= 10;"))

(check-c-string
  (micac (set x bitwise-and 10))
  (lines-string "x &= 10;"))

(check-c-string
  (micac (set x bitwise-ior 10))
  (lines-string "x |= 10;"))

(check-c-string
  (micac (set x bitwise-xor 10))
  (lines-string "x ^= 10;"))

(check-c-string
  (micac (set x bitwise-arithmetic-shift-left 10))
  (lines-string "x <<= 10;"))

(check-c-string
  (micac (set x bitwise-arithmetic-shift-right 10))
  (lines-string "x >>= 10;"))

(check-c-string
  (micac
    (set x (- a))
    (set x (bitwise-not a))
    (set x (not a)))
  (lines-string
    "x = -a;"
    "x = ~a;"
    "x = !a;"))

(check-c-string
  (micac (set pixels (cast (* uint8_t) ptr)))
  (lines-string "pixels = (uint8_t*)ptr;"))

(check-c-string
  (micac
    (set x (+ a b))
    (set x (- a b))
    (set x (* a b))
    (set x (div a b))
    (set x (and a b))
    (set x (or a b))
    (set x (not a))
    (set x (bitwise-and a b))
    (set x (bitwise-ior a b))
    (set x (bitwise-xor a b))
    (set x (bitwise-arithmetic-shift-left a b))
    (set x (bitwise-arithmetic-shift-right a b)))
  (lines-string
    "x = a + b;"
    "x = a - b;"
    "x = a * b;"
    "x = a / b;"
    "x = a && b;"
    "x = a || b;"
    "x = !a;"
    "x = a & b;"
    "x = a | b;"
    "x = a ^ b;"
    "x = a << b;"
    "x = a >> b;"))

(check-c-string
  (micac
    (set x (= a b))
    (set x (not (= a b)))
    (set x (> a b))
    (set x (>= a b))
    (set x (< a b))
    (set x (<= a b)))
  (lines-string
    "x = a == b;"
    "x = a != b;"
    "x = a > b;"
    "x = a >= b;"
    "x = a < b;"
    "x = a <= b;"))

(check-c-string
  (micac (set x (+ (+ a b) c)))
  (lines-string "x = a + b + c;"))

(check-c-string
  (micac (set x (+ a (+ b c))))
  (lines-string "x = a + (b + c);"))

(check-c-string
  (micac (set x (* (+ a b) c)))
  (lines-string "x = (a + b) * c;"))

(check-c-string
  (micac (set x (+ a (* b c))))
  (lines-string "x = a + b * c;"))

(check-c-string (micac (set x (ref y))) (lines-string "x = y;"))
(check-c-string (micac (set x (ref y z))) (lines-string "x = y.z;"))
(check-c-string (micac (set x (ref y (10)))) (lines-string "x = y[10];"))
(check-c-string (micac (set x (ref y ((+ a b))))) (lines-string "x = y[a + b];"))
(check-c-string (micac (set x (ref y *))) (lines-string "x = *y;"))
(check-c-string (micac (set x (ref y a b (10) *))) (lines-string "x = *y.a.b[10];"))

(check-c-string (micac (set x (&ref y))) (lines-string "x = &y;"))
(check-c-string (micac (set x (&ref y z))) (lines-string "x = &y.z;"))
(check-c-string (micac (set x (&ref y (10)))) (lines-string "x = &y[10];"))
(check-c-string (micac (set x (&ref y ((+ a b))))) (lines-string "x = &y[a + b];"))
(check-c-string (micac (set x (&ref y *))) (lines-string "x = &*y;"))
(check-c-string (micac (set x (&ref y a b (10) *))) (lines-string "x = &*y.a.b[10];"))

(check-c-string (micac (set (x) z)) (lines-string "x = z;"))
(check-c-string (micac (set (x y) z)) (lines-string "x.y = z;"))
(check-c-string (micac (set (x (10)) z)) (lines-string "x[10] = z;"))
(check-c-string (micac (set (x ((+ a b))) z)) (lines-string "x[a + b] = z;"))
(check-c-string (micac (set (x *) z)) (lines-string "*x = z;"))
(check-c-string (micac (set (x y a b (10) *) z)) (lines-string "*x.y.a.b[10] = z;"))

(check-c-string
  (micac (set a (if x y z)))
  (lines-string "a = x ? y : z;"))

(check-c-string
  (micac
    (if x
      (then
        (set x 10)
        (set x 20))
      (else
        (set x 30)
        (set x 40))))
  (lines-string
    "if (x) {"
    "  x = 10;"
    "  x = 20;"
    "} else {"
    "  x = 30;"
    "  x = 40;"
    "}"))

(check-c-string
  (micac
    (while x
      (set x 10)
      (set x 20)))
  (lines-string
    "while (x) {"
    "  x = 10;"
    "  x = 20;"
    "}"))

(check-c-string
  (micac
    (when x
      (set x 10)
      (set x 20)))
  (lines-string
    "if (x) {"
    "  x = 10;"
    "  x = 20;"
    "}"))

(check-c-string
  (micac
    (cond
      (x
        (set x 10)
        (set y 20))))
  (lines-string
    "if (x) {"
    "  x = 10;"
    "  y = 20;"
    "}"))

(check-c-string
  (micac
    (cond
      (x
        (set x 10)
        (set y 20))
      (y
        (set x 30)
        (set y 40))
      (z
        (set x 50)
        (set y 60))))
  (lines-string
    "if (x) {"
    "  x = 10;"
    "  y = 20;"
    "} else if (y) {"
    "  x = 30;"
    "  y = 40;"
    "} else if (z) {"
    "  x = 50;"
    "  y = 60;"
    "}"))

(check-c-string
  (micac
    (cond
      (x
        (set x 10)
        (set y 20))
      (y
        (set x 30)
        (set y 40))
      (else
        (set x 50)
        (set y 60))))
  (lines-string
    "if (x) {"
    "  x = 10;"
    "  y = 20;"
    "} else if (y) {"
    "  x = 30;"
    "  y = 40;"
    "} else {"
    "  x = 50;"
    "  y = 60;"
    "}"))

(check-c-string
  (micac (set x + (bitwise-and (- (+ x 10) 20) #xff)))
  (lines-string "x += x + 10 - 20 & 255;"))

(check-c-string
  (micac (printf "%i\\n" 10))
  (lines-string "printf(\"%i\\n\", 10);"))

(check-c-string
  (micac (SDL_CopyTexture x (SDL_Rect 0 0 20 30)))
  (lines-string "SDL_CopyTexture(x, SDL_Rect(0, 0, 20, 30));"))
