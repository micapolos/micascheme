(library (get)
  (export
    substring->get!
    string->get!
    textual-port->get!)
  (import
    (scheme)
    (eof)
    (lets))

  (define (substring->get! $string $from $to)
    (lambda ()
      (if (= $from $to)
        eof
        (lets
          ($char (string-ref $string $from))
          (run (set! $from (+ $from 1)))
          $char))))

  (define (string->get! $string)
    (substring->get! $string 0 (string-length $string)))

  (define (textual-port->get! $textual-port)
    (lambda ()
      (get-char $textual-port)))
)
