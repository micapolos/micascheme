(library (mica-sdl3)
  (export
    sdl
    sdl-window
    sdl-surface
    sdl-window-surface
    sdl-bmp-surface
    sdl-png-surface)

  (import
    (scheme)
    (sdl3)
    (syntax)
    (switch)
    (scoped))

  (export (import (sdl3)))

  (define (sdl-error)
    (error `sdl (sdl-get-error)))

  (define-rule-syntax (sdl-non-zero expr)
    (switch expr
      ((zero? _) (sdl-error))
      ((else $x) $x)))

  (define-rule-syntax (sdl-non-false expr)
    (or expr (sdl-error)))

  (define-scoped (sdl $flags (... ...))
    ($sdl (sdl-non-false (sdl-init (bitwise-ior $flags (... ...)))))
    (sdl-quit))

  (define-scoped (sdl-window $title $w $h $flag (... ...))
    ($window (sdl-non-zero (sdl-create-window $title $w $h (bitwise-ior $flag (... ...)))))
    (sdl-destroy-window $window))

  (define-scoped (sdl-bmp-surface $file)
    ($surface (sdl-non-zero (sdl-load-bmp $file)))
    (sdl-destroy-surface $surface))


  (define-scoped (sdl-png-surface $file)
    ($surface (sdl-non-zero (sdl-load-png $file)))
    (sdl-destroy-surface $surface))

  (define-scoped (sdl-surface $width $height $format)
    ($surface (sdl-non-zero (sdl-create-surface $width $height $format)))
    (sdl-destroy-surface $surface))

  (define (sdl-window-surface $window)
    (sdl-non-zero (sdl-get-window-surface $window)))
)
