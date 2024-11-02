(import (micascheme) (micac run))

(micac-externs
  SDL_GetError
  SDL_Init
  SDL_Quit
  SDL_Window
  SDL_CreateWindow
  SDL_DestroyWindow
  SDL_PollEvent
  SDL_INIT_VIDEO
  SDL_QUIT
  SDL_WINDOWPOS_UNDEFINED)

(micac-macro (print-sdl-error)
  (printf "SDL could not initialize! SDL Error: %s\\n" (SDL_GetError)))

(micac-macro (sdl-init)
  (break-if (!= (SDL_Init SDL_INIT_VIDEO) 0) (print-sdl-error))
  (defer (SDL_Quit)))

(micac-macro (create-sdl-window window title width height)
  (var (* SDL_Window) window)
  (set window (SDL_CreateWindow title SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED width height 0))
  (break-if (not window) (print-sdl-error))
  (defer (SDL_DestroyWindow window)))

(micac-macro (sdl-event-loop body ...)
  (var u8 running)
  (var SDL_Event event)
  (set running 1)
  (while running
    (while (SDL_PollEvent (&ref event))
      (if (= (ref event type) SDL_QUIT)
        (set running 0)
        (begin body ...)))))

(micac-run
  (sdl-init)
  (printf "Initialized...\\n")
  (create-sdl-window $window "My window" 640 480)
  (printf "Window created %p\\n" $window)
  (sdl-event-loop
    (printf "Game loop\\n")))
