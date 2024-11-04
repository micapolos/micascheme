(import (micascheme) (micac syntax) (micac syntax-c) (check))

(define-aux-keyword macro)

(define $lookup
  (lambda ($id $key)
    (and
      (free-identifier=? $id #'macro)
      (free-identifier=? $key #'micac-key)
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ arg ...)
            #`(macroed arg ...)))))))

(check
  (equal?
    (syntax-c $lookup)
    (lines-string)))

(check
  (equal?
    (syntax-c $lookup #`(const int x 10))
    (lines-string "const int x = 10;")))

(check
  (equal?
    (syntax-c $lookup #`(var int x))
    (lines-string "int x;")))

(check
  (equal?
    (syntax-c $lookup #`(var int some-variable))
    (lines-string "int some_variable;")))

(check
  (equal?
    (syntax-c $lookup #`(var (* uint8_t) x))
    (lines-string "uint8_t* x;")))

(check
  (equal?
    (syntax-c $lookup #`(var uint8_t (* x)))
    (lines-string "uint8_t *x;")))

(check
  (equal?
    (syntax-c $lookup #`(var uint8_t (* (* x))))
    (lines-string "uint8_t **x;")))

(check
  (equal?
    (syntax-c $lookup #`(var uint8_t (* x 24)))
    (lines-string "uint8_t x[24];")))

(check
  (equal?
    (syntax-c $lookup #`(var uint8_t (* (* x 24) 32)))
    (lines-string "uint8_t x[24][32];")))

(check
  (equal?
    (syntax-c $lookup #`(var int x y))
    (lines-string "int x = y;")))

(check
  (equal?
    (syntax-c $lookup #`(begin))
    (lines-string
      "{"
      "}")))

(check (equal? (syntax-c $lookup #`(set x 10)) (lines-string "x = 10;")))
(check (equal? (syntax-c $lookup #`(set+ x 10)) (lines-string "x += 10;")))
(check (equal? (syntax-c $lookup #`(set- x 10)) (lines-string "x -= 10;")))
(check (equal? (syntax-c $lookup #`(set* x 10)) (lines-string "x *= 10;")))
(check (equal? (syntax-c $lookup #`(set/ x 10)) (lines-string "x /= 10;")))
(check (equal? (syntax-c $lookup #`(set-and x 10)) (lines-string "x &&= 10;")))
(check (equal? (syntax-c $lookup #`(set-or x 10)) (lines-string "x ||= 10;")))
(check (equal? (syntax-c $lookup #`(set-bitwise-and x 10)) (lines-string "x &= 10;")))
(check (equal? (syntax-c $lookup #`(set-bitwise-ior x 10)) (lines-string "x |= 10;")))
(check (equal? (syntax-c $lookup #`(set-bitwise-xor x 10)) (lines-string "x ^= 10;")))
(check (equal? (syntax-c $lookup #`(set-bitwise-arithmetic-shift-left x 10)) (lines-string "x <<= 10;")))
(check (equal? (syntax-c $lookup #`(set-bitwise-arithmetic-shift-right x 10)) (lines-string "x >>= 10;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (- a))
      #`(set x (inv a))
      #`(set x (not a)))
    (lines-string
      "x = -a;"
      "x = ~a;"
      "x = !a;")))

(check
  (equal?
    (syntax-c $lookup #`(set pixels (cast (* uint8_t) ptr)))
    (lines-string "pixels = (uint8_t*)ptr;")))


(check
  (equal?
    (syntax-c $lookup
      #`(set x (+ a b))
      #`(set x (- a b))
      #`(set x (bitwise-and a b))
      #`(set x (bitwise-ior a b))
      #`(set x (bitwise-xor a b))
      #`(set x (bitwise-arithmetic-shift-left a b))
      #`(set x (bitwise-arithmetic-shift-right a b)))
    (lines-string
      "x = a + b;"
      "x = a - b;"
      "x = a & b;"
      "x = a | b;"
      "x = a ^ b;"
      "x = a << b;"
      "x = a >> b;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (+))
      #`(set x (+ a))
      #`(set x (+ a b))
      #`(set x (+ a b c)))
    (lines-string
      "x = 0;"
      "x = a;"
      "x = a + b;"
      "x = a + b + c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (- a))
      #`(set x (- a b))
      #`(set x (- a b c)))
    (lines-string
      "x = -a;"
      "x = a - b;"
      "x = a - b - c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (*))
      #`(set x (* a))
      #`(set x (* a b))
      #`(set x (* a b c)))
    (lines-string
      "x = 1;"
      "x = a;"
      "x = a * b;"
      "x = a * b * c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (/ a))
      #`(set x (/ a b))
      #`(set x (/ a b c)))
    (lines-string
      "x = 1 / a;"
      "x = a / b;"
      "x = a / b / c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (and))
      #`(set x (and a))
      #`(set x (and a b))
      #`(set x (and a b c)))
    (lines-string
      "x = true;"
      "x = a;"
      "x = a && b;"
      "x = a && b && c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (or))
      #`(set x (or a))
      #`(set x (or a b))
      #`(set x (or a b c)))
    (lines-string
      "x = false;"
      "x = a;"
      "x = a || b;"
      "x = a || b || c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (bitwise-and))
      #`(set x (bitwise-and a))
      #`(set x (bitwise-and a b))
      #`(set x (bitwise-and a b c)))
    (lines-string
      "x = -1;"
      "x = a;"
      "x = a & b;"
      "x = a & b & c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (bitwise-ior))
      #`(set x (bitwise-ior a))
      #`(set x (bitwise-ior a b))
      #`(set x (bitwise-ior a b c)))
    (lines-string
      "x = 0;"
      "x = a;"
      "x = a | b;"
      "x = a | b | c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (bitwise-xor))
      #`(set x (bitwise-xor a))
      #`(set x (bitwise-xor a b))
      #`(set x (bitwise-xor a b c)))
    (lines-string
      "x = 0;"
      "x = a;"
      "x = a ^ b;"
      "x = a ^ b ^ c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(set x (= a b))
      #`(set x (not (= a b)))
      #`(set x (> a b))
      #`(set x (>= a b))
      #`(set x (< a b))
      #`(set x (<= a b)))
    (lines-string
      "x = a == b;"
      "x = a != b;"
      "x = a > b;"
      "x = a >= b;"
      "x = a < b;"
      "x = a <= b;")))

(check
  (equal?
    (syntax-c $lookup #`(set x (+ (+ a b) c)))
    (lines-string "x = a + b + c;")))

(check
  (equal?
    (syntax-c $lookup #`(set x (+ a (+ b c))))
    (lines-string "x = a + (b + c);")))

(check
  (equal?
    (syntax-c $lookup #`(set x (* (+ a b) c)))
    (lines-string "x = (a + b) * c;")))

(check
  (equal?
    (syntax-c $lookup #`(set x (+ a (* b c))))
    (lines-string "x = a + b * c;")))

(check
  (equal?
    (syntax-c $lookup
      #`(if x (set x 10)))
    (lines-string
      "if (x) {"
      "  x = 10;"
      "}")))

(check (equal? (syntax-c $lookup #`(set x (ref y))) (lines-string "x = y;")))
(check (equal? (syntax-c $lookup #`(set x (ref y z))) (lines-string "x = y.z;")))
(check (equal? (syntax-c $lookup #`(set x (ref y 10))) (lines-string "x = y[10];")))
(check (equal? (syntax-c $lookup #`(set x (ref y (+ a b)))) (lines-string "x = y[a + b];")))
(check (equal? (syntax-c $lookup #`(set x (ref y *))) (lines-string "x = *y;")))
(check (equal? (syntax-c $lookup #`(set x (ref y a b 10 *))) (lines-string "x = *y.a.b[10];")))

(check (equal? (syntax-c $lookup #`(set x (&ref y))) (lines-string "x = &y;")))
(check (equal? (syntax-c $lookup #`(set x (&ref y z))) (lines-string "x = &y.z;")))
(check (equal? (syntax-c $lookup #`(set x (&ref y 10))) (lines-string "x = &y[10];")))
(check (equal? (syntax-c $lookup #`(set x (&ref y (+ a b)))) (lines-string "x = &y[a + b];")))
(check (equal? (syntax-c $lookup #`(set x (&ref y *))) (lines-string "x = &*y;")))
(check (equal? (syntax-c $lookup #`(set x (&ref y a b 10 *))) (lines-string "x = &*y.a.b[10];")))

(check (equal? (syntax-c $lookup #`(set (x) z)) (lines-string "x = z;")))
(check (equal? (syntax-c $lookup #`(set (x y) z)) (lines-string "x.y = z;")))
(check (equal? (syntax-c $lookup #`(set (x 10) z)) (lines-string "x[10] = z;")))
(check (equal? (syntax-c $lookup #`(set (x (+ a b)) z)) (lines-string "x[a + b] = z;")))
(check (equal? (syntax-c $lookup #`(set (x *) z)) (lines-string "*x = z;")))
(check (equal? (syntax-c $lookup #`(set (x y a b 10 *) z)) (lines-string "*x.y.a.b[10] = z;")))

(check
  (equal?
    (syntax-c $lookup
      #`(if x
        (set x 10)
        (set x 20)))
    (lines-string
      "if (x) {"
      "  x = 10;"
      "}"
      "else {"
      "  x = 20;"
      "}")))

(check
  (equal?
    (syntax-c $lookup
      #`(while x
          (set+ x 1)
          (set+ x 2)))
    (lines-string
      "while (x) {"
      "  x += 1;"
      "  x += 2;"
      "}")))

(check
  (equal?
    (syntax-c $lookup
      #`(set+ x (bitwise-and (- (+ x 10) 20) #xff)))
    (lines-string
      "x += x + 10 - 20 & 255;")))

(check
  (equal?
    (syntax-c $lookup
      #`(defer (SDL_Quit))
      #`(SDL_Init))
    (lines-string
      "SDL_Init();"
      "SDL_Quit();")))

(check
  (equal?
    (syntax-c $lookup
      #`(break-if expr (init))
      #`(defer (destroy))
      #`(update))
    (lines-string
      "if (expr) {"
      "  init();"
      "}"
      "else {"
      "  update();"
      "  destroy();"
      "}")))

(check
  (equal?
    (syntax-c $lookup #`(printf "%i\\n" 10))
    (lines-string "printf(\"%i\\n\", 10);")))

(check
  (equal?
    (syntax-c $lookup #`(SDL_CopyTexture texture (SDL_Rect 0 0 20 30)))
    (lines-string "SDL_CopyTexture(texture, SDL_Rect(0, 0, 20, 30));")))

(check
  (equal?
    (syntax-c $lookup #`(macro 10 (macro 20 30)))
    (lines-string "macroed(10, macroed(20, 30));")))
