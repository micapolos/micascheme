(import (scheme) (syntaxes) (minic asm))

(let ()
  (define regs (make-fxvector 256))
  (define sp (fxvector-length regs))
  (define-rules-syntaxes
    ((io addr) 0)
    ((io addr value)
      (case addr
        ((0) (pretty-print value))
        ((1) (write-char (integer->char value))))))

  (time
    (asm (regs sp io)
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
