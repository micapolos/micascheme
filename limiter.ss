(library (limiter)
  (export
    limiter
    limiter-using
    limiter-apply
    limiter-ref?
    limiter-limited?
    limiter-unlimited-ref
    limiter-bind
    limiter-map
    limiter-lets
    limiter-lets?
    limiter-switch
    list->limiter
    replace-limiter
    append-limiter
    apply-limiter
    limiter-try)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (limited)
    (lets)
    (list)
    (switch)
    (boolean)
    (number)
    (monadic))

  (define-rules-syntaxes
    ((limiter ($limit) limited?-body)
      (lambda ($limit) limited?-body))
    ((limiter $value)
      (limiter ($limit)
        (make-limited? $value $limit))))

  (define (limiter-using $value $used)
    (limiter ($limit)
      (make-limited? $value
        (- $limit $used))))

  (define (limiter-apply $limiter $limit)
    ($limiter $limit))

  (define (limiter-limited? $limiter $limit)
    ($limiter $limit))

  (define (limiter-ref? $limiter $limit)
    (switch (limiter-apply $limiter $limit)
      ((false? _) #f)
      ((else $limited) (limited-ref $limited))))

  (define (limiter-unlimited-ref $limiter)
    (limited-ref (limiter-apply $limiter infinity)))

  (define (limiter-bind $limiter $fn)
    (limiter ($limit)
      (lets?
        ($limited (limiter-apply $limiter $limit))
        (limiter-apply ($fn (limited-ref $limited)) (limited-limit $limited)))))

  (define-rules-syntax
    ((limiter-try) (limiter #f))
    ((limiter-try x xs ...)
      (limiter ($limit)
        (switch (limiter-limited? x $limit)
          ((false? _) (limiter-limited? (limiter-try xs ...) $limit))
          ((else $limited) $limited)))))

  (define-monadic limiter)
)
