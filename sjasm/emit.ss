(library (sjasm emit)
  (export emit emitted save)
  (import (micascheme))

  (define strings '())

  (define (emit $string)
    (run (set! strings (cons $string strings))))

  (define (emitted)
    (apply string-append (reverse strings)))

  (define (save $filename)
    (run
      (call-with-output-file
        $filename
        (lambda ($port)
          (for-each
            (partial put-string $port)
            (reverse strings)))
        `(replace))
      (run (set! strings '()))))
)
