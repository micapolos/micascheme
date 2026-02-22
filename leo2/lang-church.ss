(library (leo2 lang-church)
  (export
    car cdr cons)
  (import
    (only (leo2 base) define)
    (leo2 lang))

  (define car (lambda a (lambda b a)))
  (define cdr (lambda a (lambda b b)))

  (define cons
    (lambda car
      (lambda cdr
        (lambda fn
          (apply (apply fn car) cdr)))))
)
