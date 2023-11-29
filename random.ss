(library (random)
  (export
    current-random
    current-random-below
    current-random-seed
    set-current-random-seed)
  (import
    (scheme)
    (current))

  (define (current-random-below $number)
    (unsafe-current
      (random $number)))

  (define current-random
    (current-random-below 1.0))

  (define current-random-seed
    (unsafe-current
      (random-seed)))

  (define (set-current-random-seed $seed)
    (unsafe-current
      (random-seed $seed)
      (void)))
)
