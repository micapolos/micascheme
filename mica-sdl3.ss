(library (mica-sdl3)
  (export
    with-sdl-init
    with-sdl-window
    with-sdl-renderer
    with-sdl-rgb-surface-with-format
    with-sdl-surface
    with-sdl-surface-from
    with-locked-sdl-surface
    with-sdl-bmp-surface
    with-sdl-png-surface
    with-sdl-texture
    with-sdl-texture-from-surface
    sdl-surface-pixels
    sdl-surface-pitch
    with-sdl-event-loop)

  (import
    (scheme)
    (sdl3)
    (syntax)
    (switch))

  (export (import (sdl3)))

  (define (sdl-error)
    (error `sdl (sdl-get-error)))

  (define-rule-syntax (with-sdl-init ($flag $flags ...) $body ...)
    (if (sdl-init $flag $flags ...)
      (dynamic-wind
        (lambda () #f)
        (lambda () $body ...)
        (lambda () (sdl-quit)))
      (sdl-error)))

  (define-rule-syntax (with-sdl-window ($window $title $w $h $flag ...) $body ...)
    (switch (sdl-create-window $title $w $h (bitwise-ior $flag ...))
      ((zero? _) (sdl-error))
      ((else $window)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-window $window))))))

  (define-rule-syntax (with-sdl-renderer ($renderer $window $flag ...) $body ...)
    (switch (sdl-create-renderer $window $flag ...)
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

  (define-rule-syntax (with-sdl-bmp-surface ($surface $file) $body ...)
    (switch (sdl-load-bmp $file)
      ((zero? _) (sdl-error))
      ((else $surface)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-surface $surface))))))

  (define-rule-syntax (with-sdl-png-surface ($surface $file) $body ...)
    (switch (sdl-load-png $file)
      ((zero? _) (sdl-error))
      ((else $surface)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-surface $surface))))))

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

  (define-rule-syntax (with-sdl-surface ($surface $width $height $format) body ...)
    (switch (sdl-create-surface $width $height $format)
      ((zero? _) (sdl-error))
      ((else $surface)
        (dynamic-wind
          (lambda () #f)
          (lambda () body ...)
          (lambda () (sdl-destroy-surface $surface))))))

  (define-rule-syntax (with-sdl-surface-from ($surface $width $height $format $address $pitch) body ...)
    (switch (sdl-create-surface-from $width $height $format $address $pitch)
      ((zero? _) (sdl-error))
      ((else $surface)
        (dynamic-wind
          (lambda () #f)
          (lambda () body ...)
          (lambda () (sdl-destroy-surface $surface))))))

  (define-rule-syntax (with-locked-sdl-surface surface x xs ...)
    (let
      (($surface surface))
      (if (sdl-lock-surface $surface)
        (dynamic-wind
          (lambda () #f)
          (lambda () x xs ...)
          (lambda () (sdl-unlock-surface $surface)))
        (sdl-error))))

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
