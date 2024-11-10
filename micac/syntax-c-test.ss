(import (micascheme) (micac syntax) (micac syntax-c) (micac env) (check) (micac scope) (micac variable))

(define-aux-keyword foo)

(define $env
  (env
    (lambda ($id)
      (and
        (free-identifier=? $id #'foo)
        (lambda ($syntax)
          (syntax-case $syntax ()
            ((_ arg ...)
              #`(fooed arg ...))
            (_
              (syntax-error $syntax "wtf"))))))
    (scope-with
      (cons #`a (variable #`a))
      (cons #`b (variable #`b))
      (cons #`c (variable #`c))
      (cons #`ptr (variable #`ptr))
      (cons #`x (variable #`x))
      (cons #`y (variable #`y))
      (cons #`z (variable #`z))
      (cons #`alloc (variable #`alloc))
      (cons #`update (variable #`update))
      (cons #`free (variable #`free))
      (cons #`printf (variable #`printf))
      (cons #`SDL_CopyTexture (variable #`SDL_CopyTexture))
      (cons #`SDL_Rect (variable #`SDL_Rect))
      (cons #`fooed (variable #`fooed)))))

(check
  (equal?
    (syntax-c $env)
    (lines-string)))

(check
  (equal?
    (syntax-c $env #`(const int x 10))
    (lines-string "const int x = 10;")))

(check
  (equal?
    (syntax-c $env #`(var int x))
    (lines-string "int x;")))

(check
  (equal?
    (syntax-c $env #`(var int some-variable))
    (lines-string "int some_variable;")))

(check
  (equal?
    (syntax-c $env #`(var (* uint8_t) x))
    (lines-string "uint8_t* x;")))

(check
  (equal?
    (syntax-c $env #`(var uint8_t (* x)))
    (lines-string "uint8_t *x;")))

(check
  (equal?
    (syntax-c $env #`(var uint8_t (* (* x))))
    (lines-string "uint8_t **x;")))

(check
  (equal?
    (syntax-c $env #`(var uint8_t (* x 24)))
    (lines-string "uint8_t x[24];")))

(check
  (equal?
    (syntax-c $env #`(var uint8_t (* (* x 24) 32)))
    (lines-string "uint8_t x[24][32];")))

(check
  (equal?
    (syntax-c $env #`(var int x y))
    (lines-string "int x = y;")))

(check
  (equal?
    (syntax-c $env #`(begin))
    (lines-string
      "{"
      "}")))

(check (equal? (syntax-c $env #`(set x 10)) (lines-string "x = 10;")))
(check (equal? (syntax-c $env #`(set+ x 10)) (lines-string "x += 10;")))
(check (equal? (syntax-c $env #`(set- x 10)) (lines-string "x -= 10;")))
(check (equal? (syntax-c $env #`(set* x 10)) (lines-string "x *= 10;")))
(check (equal? (syntax-c $env #`(set/ x 10)) (lines-string "x /= 10;")))
(check (equal? (syntax-c $env #`(set-and x 10)) (lines-string "x &&= 10;")))
(check (equal? (syntax-c $env #`(set-or x 10)) (lines-string "x ||= 10;")))
(check (equal? (syntax-c $env #`(set-bitwise-and x 10)) (lines-string "x &= 10;")))
(check (equal? (syntax-c $env #`(set-bitwise-ior x 10)) (lines-string "x |= 10;")))
(check (equal? (syntax-c $env #`(set-bitwise-xor x 10)) (lines-string "x ^= 10;")))
(check (equal? (syntax-c $env #`(set-bitwise-arithmetic-shift-left x 10)) (lines-string "x <<= 10;")))
(check (equal? (syntax-c $env #`(set-bitwise-arithmetic-shift-right x 10)) (lines-string "x >>= 10;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (- a))
      #`(set x (inv a))
      #`(set x (not a)))
    (lines-string
      "x = -a;"
      "x = ~a;"
      "x = !a;")))

(check
  (equal?
    (syntax-c $env #`(set pixels (cast (* uint8_t) ptr)))
    (lines-string "pixels = (uint8_t*)ptr;")))

(check
  (equal?
    (syntax-c $env
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
    (syntax-c $env
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
    (syntax-c $env
      #`(set x (- a))
      #`(set x (- a b))
      #`(set x (- a b c)))
    (lines-string
      "x = -a;"
      "x = a - b;"
      "x = a - b - c;")))

(check
  (equal?
    (syntax-c $env
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
    (syntax-c $env
      #`(set x (/ a))
      #`(set x (/ a b))
      #`(set x (/ a b c)))
    (lines-string
      "x = 1 / a;"
      "x = a / b;"
      "x = a / b / c;")))

(check
  (equal?
    (syntax-c $env
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
    (syntax-c $env
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
    (syntax-c $env
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
    (syntax-c $env
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
    (syntax-c $env
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
    (syntax-c $env
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
    (syntax-c $env #`(set x (+ (+ a b) c)))
    (lines-string "x = a + b + c;")))

(check
  (equal?
    (syntax-c $env #`(set x (+ a (+ b c))))
    (lines-string "x = a + (b + c);")))

(check
  (equal?
    (syntax-c $env #`(set x (* (+ a b) c)))
    (lines-string "x = (a + b) * c;")))

(check
  (equal?
    (syntax-c $env #`(set x (+ a (* b c))))
    (lines-string "x = a + b * c;")))

(check (equal? (syntax-c $env #`(set x (ref y))) (lines-string "x = y;")))
(check (equal? (syntax-c $env #`(set x (ref y z))) (lines-string "x = y.z;")))
(check (equal? (syntax-c $env #`(set x (ref y (10)))) (lines-string "x = y[10];")))
(check (equal? (syntax-c $env #`(set x (ref y ((+ a b))))) (lines-string "x = y[a + b];")))
(check (equal? (syntax-c $env #`(set x (ref y *))) (lines-string "x = *y;")))
(check (equal? (syntax-c $env #`(set x (ref y a b (10) *))) (lines-string "x = *y.a.b[10];")))

(check (equal? (syntax-c $env #`(set x (&ref y))) (lines-string "x = &y;")))
(check (equal? (syntax-c $env #`(set x (&ref y z))) (lines-string "x = &y.z;")))
(check (equal? (syntax-c $env #`(set x (&ref y (10)))) (lines-string "x = &y[10];")))
(check (equal? (syntax-c $env #`(set x (&ref y ((+ a b))))) (lines-string "x = &y[a + b];")))
(check (equal? (syntax-c $env #`(set x (&ref y *))) (lines-string "x = &*y;")))
(check (equal? (syntax-c $env #`(set x (&ref y a b (10) *))) (lines-string "x = &*y.a.b[10];")))

(check (equal? (syntax-c $env #`(set (x) z)) (lines-string "x = z;")))
(check (equal? (syntax-c $env #`(set (x y) z)) (lines-string "x.y = z;")))
(check (equal? (syntax-c $env #`(set (x (10)) z)) (lines-string "x[10] = z;")))
(check (equal? (syntax-c $env #`(set (x ((+ a b))) z)) (lines-string "x[a + b] = z;")))
(check (equal? (syntax-c $env #`(set (x *) z)) (lines-string "*x = z;")))
(check (equal? (syntax-c $env #`(set (x y a b (10) *) z)) (lines-string "*x.y.a.b[10] = z;")))

(check
  (equal?
    (syntax-c $env #`(set a (? x y z)))
    (lines-string "a = x ? y : z;")))

(check
  (equal?
    (syntax-c $env
      #`(if x
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
      "}"
      "else {"
      "  x = 30;"
      "  x = 40;"
      "}")))

(check
  (equal?
    (syntax-c $env
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
    (syntax-c $env
      #`(set+ x (bitwise-and (- (+ x 10) 20) #xff)))
    (lines-string
      "x += x + 10 - 20 & 255;")))

(check
  (equal?
    (syntax-c $env
      #`(defer (free))
      #`(alloc))
    (lines-string
      "alloc();"
      "free();")))

(check
  (equal?
    (syntax-c $env
      #`(break-if x (alloc))
      #`(defer (free))
      #`(update))
    (lines-string
      "if (x) {"
      "  alloc();"
      "}"
      "else {"
      "  update();"
      "  free();"
      "}")))

(check
  (equal?
    (syntax-c $env #`(printf "%i\\n" 10))
    (lines-string "printf(\"%i\\n\", 10);")))

(check
  (equal?
    (syntax-c $env #`(SDL_CopyTexture x (SDL_Rect 0 0 20 30)))
    (lines-string "SDL_CopyTexture(x, SDL_Rect(0, 0, 20, 30));")))

(check
  (equal?
    (syntax-c $env #`(foo 10 (foo 20 30)))
    (lines-string "fooed(10, fooed(20, 30));")))

(check
  (equal?
    (syntax-c $env
      #`(macro (zero s) (set s 0)))
    ""))

(check
  (equal?
    (syntax-c $env
      #`(macro (zero s) (set s 0))
      #`(zero y))
    (lines-string "y = 0;")))

(check
  (equal?
    (syntax-c $env
      #`(macro (zero v) (set v 0))
      #`(macro (one v) (set v 1))
      #`(zero x)
      #`(one y))
    (lines-string
    "x = 0;"
    "y = 1;")))

(check
  (equal?
    (syntax-c $env
      #`(macro (zero v) (set v 0))
      #`(begin
        (macro (zero v) (set v 1))
        (zero x))
      #`(zero y))
    (lines-string
    "{"
    "  x = 1;"
    "}"
    "y = 0;")))
