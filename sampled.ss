(library (sampled)
  (export
    sampled
    sampled-bind
    sampled-iterator
    sampled-logger
    sampled-list)
  (import (micascheme))

  (define (sampled $value)
    (lambda () $value))

  (define (sampled-bind $sampled $fn)
    (lambda ()
      (($fn ($sampled)))))

  (define (sampled-iterator $initial $update)
    (define $box #f)
    (lambda ()
      (cond
        ($box
          (lets
            ($updated ($update (unbox $box)))
            (set-box! $box $updated)
            $updated))
        (else
          (set! $box (box $initial))
          $initial))))

  (define (sampled-logger $sampled)
    (sampled-bind $sampled
      (lambda ($value)
        (writeln $value)
        (sampled $value))))

  (define (push-sampled $stack $size $sampled)
    (switch $size
      ((zero? _) $stack)
      ((else $size)
        (push-sampled
          (push $stack ($sampled))
          (sub1 $size)
          $sampled))))

  (define (sampled-list $size $sampled)
    (reverse (push-sampled (stack) $size $sampled)))
)
