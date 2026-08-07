(library (tt option)
  (export
    option
    some
    none
    none?
    option=?
    option->datum
    option-map
    option-bind
    option-match)
  (import
    (tt lang)
    (prefix (option) %)
    (prefix (boolean) %)
    (prefix (scheme) %))

  (define-class (option _))

  (define option=?
    (unchecked
      (lambda (x) (pi ((pi (x x) boolean) (option x) (option x)) boolean))
      %option=?))

  (define option->datum
    (unchecked
      (lambda (x) (pi ((pi (x) datum) (option x)) datum))
      %option->datum))

  (define some
    (unchecked
      (lambda (x) (pi (x) (option x)))
      (%lambda (x) x)))

  (define none
    (unchecked
      (lambda (x) (option x))
      #f))

  (define none?
    (unchecked
      (lambda (x) (pi ((option x)) boolean))
      %false?))

  (define option-map
    (unchecked
      (lambda (a b) (pi ((pi (a) b) (option a)) (option (b))))
      (%lambda (f opt) (%and opt (f opt)))))

  (define option-bind
    (unchecked
      (lambda (a b) (pi ((pi (a) (option b)) (option a)) (option b)))
      (%lambda (f opt) (%and opt (f opt)))))

  (define option-match
    (unchecked
      (lambda (r x)
        (pi
          ((pi () r) (pi (x) r) (option x))
          r))
      (%lambda (absent-proc present-proc option)
        (%if option (present-proc option) (absent-proc)))))
)
