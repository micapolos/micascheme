(library (mica-sdl)
  (export)
  (import (micascheme) (sdl))

  (data (sdl))

  (define-syntax-rule (with-sdl $sdl $body)
    (begin
      (sdl-set-main-ready)
      (sdl-init SDL-INIT-VIDEO)
      (lets ($sdl (sdl)) $body)
      (sdl-quit)))
)
