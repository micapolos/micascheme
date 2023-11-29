(library (system)
  (export
    displayln
    writeln
    logging
    current-seconds
    current-sleep
    print-current
    run-current)
  (import
    (scheme)
    (lets)
    (number)
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

  (define (current-sleep $seconds)
    (unsafe-current
      (sleep
        (make-time 'time-duration
          (exact (floor (* (fract $seconds) 1000000000)))
          (exact (floor $seconds))))))

  (define (print-current $current)
    (unsafe-current
      (pretty-print
        (unsafe-current-get $current))))

  (define (run-current $current)
    (do () (#f)
      (unsafe-current-get $current)))

  (define current-seconds
    (unsafe-current
      (lets
        ($time (current-time `time-monotonic))
        (+
          (time-second $time)
          (/ (time-nanosecond $time) 1000000000.0)))))
)
