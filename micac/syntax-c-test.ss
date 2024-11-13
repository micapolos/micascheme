(import (micascheme) (micac syntax) (micac syntax-c) (micac env) (check) (micac scope) (micac expr))

; for testing
(pretty-expr? #t)

(define-aux-keywords foo)

(define $env
  (env
    (lambda ($id)
      (and
        (or (free-identifier=? $id #'foo) (free-identifier=? $id #'zero?))
        (lambda ($syntax)
          (syntax-case $syntax (foo zero?)
            ((foo arg ...)
              #`(fooed arg ...))
            ((zero? expr)
              #`(= expr 0))
            (_
              (syntax-error $syntax "wtf"))))))
    (scope-with
      (cons #`a (identifier->expr #`a))
      (cons #`b (identifier->expr #`b))
      (cons #`c (identifier->expr #`c))
      (cons #`ptr (identifier->expr #`ptr))
      (cons #`x (identifier->expr #`x))
      (cons #`y (identifier->expr #`y))
      (cons #`z (identifier->expr #`z))
      (cons #`pixels (identifier->expr #`pixels))
      (cons #`alloc (identifier->expr #`alloc))
      (cons #`update (identifier->expr #`update))
      (cons #`free (identifier->expr #`free))
      (cons #`printf (identifier->expr #`printf))
      (cons #`SDL_CopyTexture (identifier->expr #`SDL_CopyTexture))
      (cons #`SDL_Rect (identifier->expr #`SDL_Rect))
      (cons #`fooed (identifier->expr #`fooed)))))

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
      #`(set x (bitwise-not a))
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
      #`(set x (+ 10))
      #`(set x (+ 10 20))
      #`(set x (+ 10 20 30)))
    (lines-string
      "x = 0;"
      "x = 10;"
      "x = 30;"
      "x = 60;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (- 30))
      #`(set x (- 30 20))
      #`(set x (- 30 20 10)))
    (lines-string
      "x = -30;"
      "x = 10;"
      "x = 0;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (*))
      #`(set x (* 10))
      #`(set x (* 10 20))
      #`(set x (* 10 20 30)))
    (lines-string
      "x = 1;"
      "x = 10;"
      "x = 200;"
      "x = 6000;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (div 8 4)))
    (lines-string "x = 2;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (and))
      #`(set x (and #t))
      #`(set x (and #f))
      #`(set x (and #t #t))
      #`(set x (and #t #f))
      #`(set x (and #t #t #t))
      #`(set x (and #t #f #t)))
    (lines-string
      "x = true;"
      "x = true;"
      "x = false;"
      "x = true;"
      "x = false;"
      "x = true;"
      "x = false;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (or))
      #`(set x (or #f))
      #`(set x (or #t))
      #`(set x (or #f #f))
      #`(set x (or #f #t))
      #`(set x (or #f #f #f))
      #`(set x (or #f #t #f)))
    (lines-string
      "x = false;"
      "x = false;"
      "x = true;"
      "x = false;"
      "x = true;"
      "x = false;"
      "x = true;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (not #f))
      #`(set x (not #t)))
    (lines-string
      "x = true;"
      "x = false;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (bitwise-and))
      #`(set x (bitwise-and 15))
      #`(set x (bitwise-and 15 5))
      #`(set x (bitwise-and 15 5 1)))
    (lines-string
      "x = -1;"
      "x = 15;"
      "x = 5;"
      "x = 1;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (bitwise-ior))
      #`(set x (bitwise-ior 5))
      #`(set x (bitwise-ior 4 1))
      #`(set x (bitwise-ior 8 4 1)))
    (lines-string
      "x = 0;"
      "x = 5;"
      "x = 5;"
      "x = 13;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (bitwise-xor))
      #`(set x (bitwise-xor 15))
      #`(set x (bitwise-xor 15 5))
      #`(set x (bitwise-xor 15 5 1)))
    (lines-string
      "x = 0;"
      "x = 15;"
      "x = 10;"
      "x = 11;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (bitwise-not 15)))
    (lines-string
      "x = -16;")))

(check
  (equal?
    (syntax-c $env
      #`(set x (bitwise-arithmetic-shift-left 5 1))
      #`(set x (bitwise-arithmetic-shift-right 5 1)))
    (lines-string
      "x = 10;"
      "x = 2;")))

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
      #`(set x (div a b)))
    (lines-string
      "x = a / b;")))

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
      "} else {"
      "  x = 30;"
      "  x = 40;"
      "}")))

(check
  (equal?
    (syntax-c $env
      #`(while x
          (set x 10)
          (set x 20)))
    (lines-string
      "while (x) {"
      "  x = 10;"
      "  x = 20;"
      "}")))

(check
  (equal?
    (syntax-c $env
      #`(when x
          (set x 10)
          (set x 20)))
    (lines-string
      "if (x) {"
      "  x = 10;"
      "  x = 20;"
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
      "} else {"
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
    (syntax-c $env #`(printf (zero? x)))
    (lines-string "printf(x == 0);")))

; todo: this should expand to x != 0
(check
  (equal?
    (syntax-c $env #`(printf (not (zero? x))))
    (lines-string "printf(x != 0);")))

(check
  (equal?
    (syntax-c $env
      #`(macro (zero s) (set s 0)))
    ""))

(check
  (equal?
    (syntax-c $env
      #`(macro one 1)
      #`(macro two 2)
      #`(macro three 3)
      #`(macro sum (+ one two three))
      #`(set x sum))
    (lines-string "x = 6;")))

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
