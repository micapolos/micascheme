(library (leo code)
  (export
    limited-length+?
    limited-length+leo?
    limited-length+leos?

    atom-code?
    limited-simple-string?)
  (import
    (micascheme)
    (leo datum)
    (code))

  (define (limited-length+? $limited-length $number)
    (make-limited?
      (+ (limited-ref $limited-length) $number)
      (- (limited-limit $limited-length) $number)))

  (define (limited-length+leo? $limited-length $leo)
    (switch $leo
      ((null? _) $limited-length)
      ((pair? $pair)
        (lets?
          ($limited-length (limited-length+leo? $limited-length (car $pair)))
          (limited-length+leo? $limited-length (cdr $pair))))
      ((char? $char)
        (limited-length+? $limited-length 2))
      ((bytevector? $bytevector)
        (limited-length+? $limited-length
          (+ (bytevector-length $bytevector) 1)))
      ((vector? $vector)
        (lets?
          ($limited-length (limited-length+? $limited-length 1))
          (limited-length+leos? $limited-length (vector->list $vector))))
      ((else $other)
        (limited-length+? $limited-length 1))))

  (define (limited-length+leos? $limited-length $leos)
    (fold-left
      (lambda ($limited-length? $leo)
        (and $limited-length?
          (limited-length+leo? $limited-length? $leo)))
      $limited-length
      $leos))

  (define (atom-code? $datum)
    (switch $datum
      ((pair? $pair) #f)
      ((else $other) (string-code (format "~s" $other)))))

  (define (atom-code?-limiter $datum)
    (switch (atom-code? $datum)
      ((false? _) (limiter #f))
      ((else $code) (limiter-using $code 1))))

  (define (simple-code?-limiter $datum)
    (limiter-switch (atom-code?-limiter $datum)
      ((false? _)
        (switch $datum
          ((pair? $pair)
            (limiter-lets?
              ($car-code (atom-code?-limiter (car $pair)))
              (switch (cdr $pair)
                ((null? _)
                  (limiter $car-code))
                ((pair? $cdr)
                  (limiter-lets?
                    ($cdr-code (simple-code?-limiter $cdr))
                    (limiter (code $car-code " " $cdr-code))))
                ((else $cdr)
                  (limiter-lets?
                    ($cdr-code (atom-code?-limiter $cdr))
                    (limiter (code $car-code " . " $cdr-code)))))))
          ((else _)
            (limiter #f))))
      ((else $atom-code)
        (limiter $atom-code))))

  (define (limited-simple-code? $datum $limit)
    (limiter-apply (simple-code?-limiter $datum) $limit))

  (define (limited-simple-string? $datum $limit)
    (lets?
      ($limited (limited-simple-code? $datum $limit))
      (limited-map $limited code-string)))
)
