(import (scheme) (syntaxes) (minic asm) (minic prim))

(let ()
  (define io
    (case-lambda
      ((addr) 0)
      ((addr value)
        (if (prim-zero? addr)
          (pretty-print value)
          (write-char (integer->char value))))))

  (time
    (asm (256 io)
      (alloc 5)

      (const 0 0) ; pretty-print port
      (const 1 1) ; write-char port

      ; Print start value
      (const 2 (char->integer #\S)) (out 2 1)
      (const 2 (char->integer #\t)) (out 2 1)
      (const 2 (char->integer #\a)) (out 2 1)
      (const 2 (char->integer #\r)) (out 2 1)
      (const 2 (char->integer #\t)) (out 2 1)
      (const 2 (char->integer #\:)) (out 2 1)
      (const 2 (char->integer #\space)) (out 2 1)
      (out 4 0)

      ; Loop
      (const 3 350000000) ; counter down
      (const 4 0) ; counter up
      (loop 3 (dec 3) (inc 4))

      ; Print end value
      (const 2 (char->integer #\E)) (out 2 1)
      (const 2 (char->integer #\n)) (out 2 1)
      (const 2 (char->integer #\d)) (out 2 1)
      (const 2 (char->integer #\:)) (out 2 1)
      (const 2 (char->integer #\space)) (out 2 1)
      (out 4 0)

      (free 5))))
