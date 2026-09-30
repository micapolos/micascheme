(library (sdl3-image)
  (export
    img-load)
  (import
    (chezscheme)
    (sdl3)
    (shared-library))

  (define *sdl3-image* (load-shared-library "SDL3_image"))

  (define img-load
    (foreign-procedure "IMG_Load" (string) uptr))
)
