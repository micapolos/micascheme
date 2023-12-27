(library (stax)
  (export
    stax stax? stax-bytevector stax-top-box
    stax-top
    make-stax
    stax-u8-ref
    stax-u8-set!
    stax-u8-push
    stax-u8-pop
    stax-resize!)
  (import (micascheme))

  (data (stax top-box bytevector))

  (define (make-stax $size)
    (stax
      (box -1)
      (make-bytevector $size 0)))

  (define (stax-top $stax)
    (unbox (stax-top-box $stax)))

  (define (set-stax-top! $stax $top)
    (set-box! (stax-top-box $stax) $top))

  (define (stax-u8-ref $stax $offset)
    (bytevector-u8-ref
      (stax-bytevector $stax)
      (- (stax-top $stax) $offset)))

  (define (stax-u8-set! $stax $offset $u8)
    (bytevector-u8-set!
      (stax-bytevector $stax)
      (- (stax-top $stax) $offset)
      $u8))

  (define (stax-resize! $stax $offset)
    (set-stax-top!
      $stax
      (+ (stax-top $stax) $offset)))

  (define (stax-u8-push $stax $u8)
    (run
      (stax-resize! $stax 1)
      (stax-u8-set! $stax 0 $u8)))

  (define (stax-u8-pop $stax $u8)
    (lets
      ($top (stax-u8-ref $stax 0))
      (run (stax-resize! $stax -1))
      $top))
)
