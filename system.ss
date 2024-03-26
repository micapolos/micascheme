(library (system)
  (export
    displayln
    writeln
    logging
    current-seconds
    current-file-string
    pretty-print-current
    display-current)
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
        (let (($var $value))
          (writeln $var)
          $var))
      ((_ $label $value)
        (let (($var $value))
          (display (symbol->string (quote $label)))
          (display ": ")
          (writeln $var)
          $var))))

  (define (pretty-print-current $current)
    (do () (#f)
      (display "\x1B;[2J")
      (display "\x1B;[0;0H")
      (pretty-print (unsafe-current-get $current))))

  (define (display-current $current)
    (do () (#f)
      (display "\x1B;[2J")
      (display "\x1B;[0;0H")
      (display (unsafe-current-get $current))))

  (define (current-file-string $path)
    (unsafe-current
      (call-with-input-file $path get-string-all)))

  (define current-seconds
    (unsafe-current
      (lets
        ($time (current-time `time-monotonic))
        (+
          (time-second $time)
          (/ (time-nanosecond $time) 1000000000.0)))))
)
