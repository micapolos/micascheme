(import
  (micascheme)
  (micalog scheme transformer)
  (prefix (micalog keywords) %)
  (prefix (micalog scheme keywords) %))

(define-check-datum-> scheme)

; === values ===

(check-scheme (value foo) foo)
(check-scheme (value 123) 123)

; === expressions ===

(check-scheme (expr foo) foo)
(check-scheme (expr 123) 123)

(check-scheme
  (expr (%append (2 a)))
  a)

(check-scheme
  (expr (%append (2 a) (4 b)))
  (bitwise-ior (bitwise-arithmetic-shift-left a 4) b))

(check-scheme
  (expr (%append (2 a) (4 b) (5 c)))
  (bitwise-ior
    (bitwise-arithmetic-shift-left
      (bitwise-ior
        (bitwise-arithmetic-shift-left a 4)
        b)
      5)
    c))

(check-scheme
  (expr (%take 6 a 6))
  (bitwise-and a #x3f))

(check-scheme
  (expr (%drop 6 a 2))
  (bitwise-arithmetic-shift-right a 2))

(check-scheme (expr (%= 6 a b)) (= a b))
(check-scheme (expr (%!= 6 a b)) (not (= a b)))
(check-scheme (expr (%< 6 a b)) (< a b))
(check-scheme (expr (%<= 6 a b)) (<= a b))
(check-scheme (expr (%> 6 a b)) (> a b))
(check-scheme (expr (%>= 6 a b)) (>= a b))

(check-scheme (expr (%wrap+ 6 a b)) (bitwise-and (+ a b) #x3f))
(check-scheme (expr (%wrap- 6 a b)) (bitwise-and (- a b) #x3f))
(check-scheme (expr (%wrap* 6 a b)) (bitwise-and (* a b) #x3f))
(check-scheme (expr (%wrap- 6 a)) (bitwise-and (- a) #x3f))

(check-scheme (expr (%+ 6 a b)) (+ a b))
(check-scheme (expr (%- 6 a b)) (- a b))
(check-scheme (expr (%* 6 a b)) (* a b))
(check-scheme (expr (%- 6 a)) (- a))

(check-scheme (expr (%and 1 a b)) (and a b))
(check-scheme (expr (%or 1 a b)) (or a b))
(check-scheme (expr (%xor 1 a b)) (bitwise-xor a b))

(check-scheme (expr (%and 6 a b)) (bitwise-and a b))
(check-scheme (expr (%or 6 a b)) (bitwise-ior a b))
(check-scheme (expr (%xor 6 a b)) (bitwise-xor a b))

(check-scheme (expr (%nand 1 a b)) (bitwise-and (not (and a b)) 1))
(check-scheme (expr (%nor 1 a b)) (bitwise-and (not (or a b)) 1))
(check-scheme (expr (%xnor 1 a b)) (bitwise-and (not (bitwise-xor a b)) 1))

(check-scheme (expr (%nand 6 a b)) (bitwise-and (bitwise-not (bitwise-and a b)) #x3f))
(check-scheme (expr (%nor 6 a b)) (bitwise-and (bitwise-not (bitwise-ior a b)) #x3f))
(check-scheme (expr (%xnor 6 a b)) (bitwise-and (bitwise-not (bitwise-xor a b)) #x3f))

(check-scheme (expr (%not 1 a)) (bitwise-and (not a) 1))
(check-scheme (expr (%not 6 a)) (bitwise-and (bitwise-not a) #x3f))

(check-scheme (expr (%if 6 a b c)) (if a b c))

(check-scheme
  (expr (%if 6 (%not 1 a) (%not 6 b) (%and 6 c d)))
  (if
    (bitwise-and (not a) 1)
    (bitwise-and (bitwise-not b) #x3f)
    (bitwise-and c d)))

; === registers ===

(check-scheme
  (register (%register 8 foo))
  (foo 0))

; === instructions ===

(check-scheme
  (instruction (%wire 8 foo bar))
  (foo bar))

(check-scheme
  (instruction (%output 8 foo bar))
  (run))

(check-scheme
  (instruction (%set 8 foo bar))
  (run (set! foo bar)))

(check-scheme
  (instruction (%set 8 foo bar))
  (run (set! foo bar)))

(check-scheme
  (instruction (%log foo 16 (%wrap+ 16 1 2)))
  (run (display (format "~a: ~a\\n" (bitwise-and (+ 1 2) 65535)))))

(check-scheme
  (instruction (%on (%posedge prev next)))
  (run
    (when (not (= prev next))
      (when (= next 1)
        (lets (void))))))

(check-scheme
  (instruction (%on (%negedge prev next)))
  (run
    (when (not (= prev next))
      (when (= next 0)
        (lets (void))))))

(check-scheme
  (instruction
    (%on (%posedge prev next)
      (%set 8 foo bar)
      (%set 8 goo gar)))
  (run
    (when (not (= prev next))
      (when (= next 1)
        (lets
          (run (set! foo bar))
          (run (set! goo gar))
          (void))))))

(check-scheme
  (instruction
    (%cond
      (foo (%set 8 foo bar))
      (%else (%set 8 zoo zar))))
  (run
    (cond
      (foo (lets (run (set! foo bar)) (void)))
      (else (lets (run (set! zoo zar)) (void))))))

(check-scheme
  (instruction
    (%cond
      (foo (%set 8 foo bar))
      (bar (%set 8 zoo zar))))
  (run
    (cond
      (foo (lets (run (set! foo bar)) (void)))
      (bar (lets (run (set! zoo zar)) (void))))))

; === block ===

(check-scheme
  (block ())
  (lets (void)))

(check-scheme
  (block
    (
      (%wire 4 foo bar)
      (%set 4 goo gar)))
  (lets
    (foo bar)
    (run (set! goo gar))
    (void)))

; === module ===

(check-scheme
  (module (%module foo))
  (eval (quote (lets (run (do () (#f (void)) (lets (void)))) (void))) (environment (quote (micascheme)))))

(check-scheme
  (module
    (%module foo
      (%input 1 %clock)
      (%input 1 %reset?)
      (%output 1 %exit? (%not 1 %reset?))))
   (eval
    (quote
      (lets
        (clock 0)
        (reset-counter 32)
        (reset? 1)
        (run
          (do ()
            ((= (bitwise-and (not %reset?) 1) 1) (void))
            (lets
              (run (set! clock (xor clock 1)))
              (run
                (cond
                  ((= reset-counter 0) (set! reset? 0))
                  (else (set! reset-counter (- reset-counter 1)))))
              (run)
              (void))))
        (void)))
    (environment (quote (micascheme)))) )









