(import (scheme) (syntaxes) (minic asm))

(let ()
  (define regs (make-fxvector 2))
  (define sp (fxvector-length regs))
  (define-rules-syntaxes
    ((io addr) 0)
    ((io addr value) (pretty-print value)))

  (time
    (asm (regs sp io)
      (alloc 2)
      (const 0 35000000)
      (const 1 0)
      (loop 0
        (inc 1)
        (dec 0))
      (out 1 0)
      (free 2))))
