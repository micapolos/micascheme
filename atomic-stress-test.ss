(import (scheme))

(define (run-benchmark iterations)
  (let ([table (make-hashtable equal-hash equal?)])
    (let loop ([i iterations])
      (unless (zero? i)
        (let ([data (list i 'constant-symbol)])
          (hashtable-set! table (car data) data))
          (loop (- i 1))))))

(display `(threaded ,(threaded?)))
(newline)

(time (run-benchmark (string->number (car (command-line-arguments)))))
