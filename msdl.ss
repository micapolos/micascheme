(library (msdl)
  (export
    sdl-set-main-ready
    sdl-init)
  (import (chezscheme))

  (define SDL2
    (load-shared-object
      (let (($machine-type (machine-type)))
        (case machine-type
          ((i3nt ti3nt a6nt ta6nt) "SDL2.dll")
          ((i3le ti3le a6le ta6le) "libSDL2.so")
          ((i3osx ti3osx a6osx ta6osx) "libSDL2.dylib")
          (else (error 'SDL "unsupported machine type" machine-type))))))

  (define SDL_SetMainReady (foreign-procedure "SDL_SetMainReady" () void))

  (define SDL_Init (foreign-procedure "SDL_Init" (unsigned-32) int))
  (define SDL-INIT-TIMER #x00000001)
  (define SDL-INIT-AUDIO #x00000010)
  (define SDL-INIT-VIDEO #x00000020)
  (define SDL-INIT-JOYSTICK #x00000200)
  (define SDL-INIT-HAPTIC #x00001000)
  (define SDL-INIT-GAMECONTROLLER #x00002000)
  (define SDL-INIT-EVENTS #x00004000)
  (define SDL-INIT-EVERYTHING
    (bitwise-ior
      SDL-INIT-TIMER
      SDL-INIT-AUDIO
      SDL-INIT-VIDEO
      SDL-INIT-EVENTS
      SDL-INIT-JOYSTICK
      SDL-INIT-HAPTIC
      SDL-INIT-GAMECONTROLLER))

  (define-syntax sdl-set-main-ready
    (syntax-rules ()
      ((_) (SDL_SetMainReady))))

  (define-syntax sdl-init
    (syntax-rules ()
      ((_ $flag ...)
        (SDL_Init (bitwise-ior $flag ...)))))
)
