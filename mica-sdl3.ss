(library (mica-sdl3)
  (export
    with-sdl
    with-sdl-window
    with-sdl-renderer
    with-sdl-rgb-surface-with-format
    with-sdl-texture
    with-sdl-texture-from-surface
    sdl-surface-pixels
    sdl-surface-pitch
    with-sdl-event-loop)

  (import
    (scheme)
    (sdl3)
    (syntax))

  (export (import (sdl3)))

  (define (sdl-error)
    (error `sdl (sdl-get-error)))

  (define-rule-syntax (with-sdl ($flag $flags ...) $body ...)
    (case (sdl-init $flag $flags ...)
      ((0)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-quit))))
      (else (sdl-error))))

  (define-rule-syntax (with-sdl-window ($window $title $x $y $w $h $flag ...) $body ...)
    (switch (sdl-create-window $title $x $y $w $h $flag ...)
      ((zero? _) (sdl-error))
      ((else $window)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-window $window))))))

  (define-rule-syntax (with-sdl-renderer ($renderer $window $index $flag ...) $body ...)
    (switch (sdl-create-renderer $window $index $flag ...)
      ((zero? _) (sdl-error))
      ((else $renderer)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-renderer $renderer))))))

  (define-rule-syntax (with-sdl-rgb-surface-with-format ($surface $flags $width $height $bits-per-pixel $pixel-format) $body ...)
    (switch (sdl-create-rgb-surface-with-format $flags $width $height $bits-per-pixel $pixel-format)
      ((ftype-pointer-null? _) (sdl-error))
      ((else $surface)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-free-surface $surface))))))

  (define-rule-syntax (with-sdl-texture ($texture $renderer $format $access $width $height) $body ...)
    (switch (sdl-create-texture $renderer $format $access $width $height)
      ((zero? _) (sdl-error))
      ((else $texture)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-texture $texture))))))

  (define-rule-syntax (with-sdl-texture-from-surface ($texture $renderer $surface) $body ...)
    (switch (sdl-create-texture-from-surface $renderer $surface)
      ((zero? _) (sdl-error))
      ((else $texture)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-texture $texture))))))

  (define-rule-syntax (sdl-surface-pixels $surface)
    (ftype-ref SDL_Surface (pixels) $surface))

  (define-rule-syntax (sdl-surface-pitch $surface)
    (ftype-ref SDL_Surface (pitch) $surface))


  (define-rule-syntax (with-sdl-event-loop $body ...)
    (do
      (($event (sdl-poll-event) (sdl-poll-event)))
      ((sdl-event-quit?) (void))
      $body ...))
)
