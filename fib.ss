#!chezscheme
(library (fib)
  (export fib fxfib fx3fib flfib flsinglefib)
  (import (micascheme))

  (define (fib n)
    (if (< n 2)
      n
      (+
        (fib (- n 2))
        (fib (- n 1)))))

  (define (fxfib n)
    (if (fx< n 2)
      n
      (fx+/wraparound
        (fxfib (fx-/wraparound n 2))
        (fxfib (fx-/wraparound n 1)))))

  (define (fx3fib n)
    (if (#3%fx< n 2)
      n
      (#3%fx+/wraparound
        (fxfib (#3%fx-/wraparound n 2))
        (fxfib (#3%fx-/wraparound n 1)))))

  (define (flfib n)
    (if (fl< n 2.0)
      n
      (fl+
        (flfib (fl- n 2.0))
        (flfib (fl- n 1.0)))))

  (define (flsinglefib n)
    (if (fl< n (flsingle 2.0))
      n
      (flsingle
        (fl+
          (flsinglefib (flsingle (fl- n (flsingle 2.0))))
          (flsinglefib (flsingle (fl- n (flsingle 1.0))))))))
)
