(library (limited)
  (export
    limited?
    limited=?
    make-limited?
    limited-ref
    limited-limit
    limited-use?
    limited-map)
  (import
    (scheme)
    (data))

  (data (limited ref limit))

  (define (limited=? $ref=? $limited-a $limited-b)
    (and
      ($ref=? (limited-ref $limited-a) (limited-ref $limited-b))
      (= (limited-limit $limited-a) (limited-limit $limited-b))))

  (define (make-limited? $ref $limit)
    (and
      (nonnegative? $limit)
      (limited $ref $limit)))

  (define (limited-use? $limited $number)
    (make-limited?
      (limited-ref $limited)
      (- (limited-limit $limited) $number)))

  (define (limited-map $limited $fn)
    (limited
      ($fn (limited-ref $limited))
      (limited-limit $limited)))
)
