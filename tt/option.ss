(library (tt option)
  (export
    option
    some
    none
    option-match)
  (import
    (tt lang)
    (prefix (scheme) %))

  (define-class (option _))

  (define some
    (unchecked
      (forall (x) (pi (x) (option x)))
      (%lambda (x) x)))

  (define none
    (unchecked
      (forall (x) (option x))
      #f))

  (define option-match
    (unchecked
      (forall (r x)
        (pi
          ((pi () r) (pi (x) r) (option x))
          r))
      (%lambda (absent-proc present-proc option)
        (%if option (present-proc option) (absent-proc)))))
)
