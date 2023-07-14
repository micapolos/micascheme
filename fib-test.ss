(define fib
  (rec fib
    (lambda (n) 
      (if (fx< n 2) n (fx+ (fib (fx- n 2)) (fib (fx- n 1)))))))

(writeln (time (fib 42)))
