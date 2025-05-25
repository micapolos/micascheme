(import (micascheme) (sjasm split))

(check
  (equal?
    (syntax->datum
      (splita
        (syntaxes
          (define len (- end start))
          (define start)
          (define bar 10)
          (define goo)
          (display 10)
          (newline)
          (define end))))
    '(let ()
      (define start (random 1))
      (define goo (random 1))
      (define end (random 1))
      (define len (- end start))
      (define bar 10)
      (display 10)
      (newline)
      (void))))
