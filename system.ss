(library (system)
  (export
    displayln
    writeln
    logging
    current-seconds)
  (import
    (scheme)
    (lets)
    (current))

  (define (displayln x)
    (display x)
    (newline))

  (define (writeln x)
    (write x)
    (newline))

  (define-syntax logging
    (syntax-rules ()
      ((_ $value)
        (let ()
          (writeln $value)
          $value))
      ((_ $label $value)
        (let ()
          (display (symbol->string (quote $label)))
          (display ": ")
          (writeln $value)
          $value))))

  (define current-seconds
    (unsafe-current
      (lets
        ($time (current-time `time-monotonic))
        (+
          (time-second $time)
          (/ (time-nanosecond $time) 1000000000.0)))))
)
