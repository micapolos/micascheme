(library (get)
  (export
    substring->get-char/eof!
    string->get-char/eof!
    textual-port->get-char/eof!)
  (import
    (scheme)
    (eof)
    (lets))

  (define (substring->get-char/eof! $string $from $to)
    (lambda ()
      (if (= $from $to)
        eof
        (lets
          ($char (string-ref $string $from))
          (run (set! $from (+ $from 1)))
          $char))))

  (define (string->get-char/eof! $string)
    (substring->get-char/eof! $string 0 (string-length $string)))

  (define (textual-port->get-char/eof! $textual-port)
    (lambda ()
      (get-char $textual-port)))
)
