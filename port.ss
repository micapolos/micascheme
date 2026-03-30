(library (port)
  (export
    put-u16 with-bytevector-output-port
    make-prefixed-textual-output-port
    make-prefixed-textual-input-port)
  (import (scheme) (bytevector) (syntax))

  (define (put-u16 $port $u16 $endianness)
    (put-bytevector $port (u16-bytevector $u16 $endianness)))

  (define-rule-syntax (with-bytevector-output-port port body ...)
    (call-with-bytevector-output-port
      (lambda (port) body ...)))

  (define (make-prefixed-textual-output-port $port $prefix)
    (let ((new-line? #t))
      (make-custom-textual-output-port
        (string-append "prefixed-" (port-name $port))
        (lambda ($string $start $count)
          (let loop (($index $start) (written 0))
            (cond
              ((< $index (+ $start $count))
                (let ((char (string-ref $string $index)))
                  (when new-line?
                    (display $prefix $port)
                    (set! new-line? #f))
                  (write-char char $port)
                  (when (char=? char #\newline)
                    (set! new-line? #t))
                  (loop (+ $index 1) (+ written 1))))
              (else written))))
        (lambda ()
          (port-position $port))
        (lambda ($position)
          (set-port-position! $port $position))
        (lambda ()
          (close-port $port)))))

  (define (make-prefixed-textual-input-port $port $prefix $output-port)
    (let ((new-line? #f))
      (make-custom-textual-input-port
        (string-append "prefixed-" (port-name $port))
        (lambda ($string $start $count)
          (let loop (($index $start) (read-count 0))
            (cond
              ((< $index (+ $start $count))
                (when new-line?
                  (display $prefix $output-port)
                  (flush-output-port $output-port)
                  (set! new-line? #f))
                (let (($char (get-char $port)))
                  (cond
                    ((eof-object? $char) read-count)
                    (else
                      (string-set! $string $index $char)
                      (cond
                        ((char=? $char #\newline)
                          (set! new-line? #t)
                          (+ read-count 1))
                        (else
                          (loop (+ $index 1) (+ read-count 1))))))))
              (else read-count))))
        (lambda ()
          (port-position $port))
        (lambda ($position)
          (set-port-position! $port $position))
        (lambda ()
          (close-port $port)))))
)
