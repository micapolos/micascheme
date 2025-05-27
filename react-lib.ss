(library (react-lib)
  (export
    noise
    message
    osc
    rect
    audio
    mouse-x
    mouse-y
    canvas-width
    canvas-height
    space?
    seconds
    frames
    mix
    modulate
    inverse
    sine
    square
    pulse
    triangle
    make
    steps
    sample-rate)
  (import (micascheme))

  (define-keyword noise)
  (define-keyword rect)
  (define-keyword message)
  (define-keyword audio)
  (define-keyword osc)
  (define-keyword mouse-x)
  (define-keyword mouse-y)
  (define-keyword canvas-width)
  (define-keyword canvas-height)
  (define-keyword space?)
  (define-keyword seconds)
  (define-keyword frames)
  (define-keyword sample-rate)
  (define-keyword make)

  (define (inverse $value)
    (- 1 $value))

  (define (mix . $values)
    (/ (apply + $values) (length $values)))

  (define (modulate . $values)
    (apply * $values))

  (define (sine $x)
    (* 0.5 (+ (sin (* $x pi2)) 1)))

  (define (square $x)
    (pulse $x 0.5))

  (define (triangle $x)
    (if (< $x 0.5)
      (* 2 $x)
      (- 2 (* 2 $x))))

  (define (pulse $x $w)
    (if (< $x $w) 0 1))

  (define (steps $t $n)
    (min (- $n 1) (max 0 (inexact->exact (floor (* $t $n))))))
)
