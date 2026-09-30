(library (sdl3-image)
  (export
    img-load)
  (import
    (chezscheme)
    (sdl3)
    (shared-library))

  (define *sdl3-image* (load-shared-library "libSDL3_image.dylib"))

  (define img-load
    (foreign-procedure "IMG_Load" (string) uptr))
)
