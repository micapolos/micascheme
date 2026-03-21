(library (limiter)
  (export
    limiter
    limiter-using
    limiter-apply
    limiter-bind
    limiter-map
    limiter-lets
    limiter-switch
    list->limiter
    replace-limiter
    append-limiter
    apply-limiter)
  (import
    (scheme)
    (syntaxes)
    (limited)
    (lets)
    (list)
    (switch)
    (boolean)
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

  (define (limiter-bind $limiter $fn)
    (limiter ($limit)
      (lets?
        ($limited (limiter-apply $limiter $limit))
        (limiter-apply ($fn (limited-ref $limited)) (limited-limit $limited)))))

  (define-monadic limiter)
)
