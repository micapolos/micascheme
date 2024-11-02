(import (micascheme) (micac run))

(micac-externs
  SDL_GetError
  SDL_Init
  SDL_Quit
  SDL_Window
  SDL_CreateWindow
  SDL_DestroyWindow
  SDL_CreateRenderer
  SDL_DestroyRenderer
  SDL_SetRenderDrawColor
  SDL_RenderClear
  SDL_RenderPresent
  SDL_PollEvent
  SDL_INIT_VIDEO
  SDL_QUIT
  SDL_WINDOWPOS_UNDEFINED
  SDL_RENDERER_ACCELERATED
  SDL_RENDERER_PRESENTVSYNC)

(micac-macro (print-sdl-error)
  (printf "SDL could not initialize! SDL Error: %s\\n" (SDL_GetError)))

(micac-macro (sdl-init)
  (break-if (!= (SDL_Init SDL_INIT_VIDEO) 0) (print-sdl-error))
  (defer (SDL_Quit)))

(micac-macro (create-sdl-window window title width height)
  (var (* SDL_Window) window
    (SDL_CreateWindow title SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED width height 0))
  (break-if (not window) (print-sdl-error))
  (defer (SDL_DestroyWindow window)))

(micac-macro (create-sdl-renderer renderer window)
  (var (* SDL_Renderer) renderer
    (SDL_CreateRenderer window -1 (or SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC)))
  (break-if (not renderer) (print-sdl-error))
  (defer (SDL_DestroyRenderer renderer)))

(micac-macro (sdl-event-loop body ...)
  (var u8 running 1)
  (var SDL_Event event)
  (while running
    (while (SDL_PollEvent (&ref event))
      (if (= (ref event type) SDL_QUIT)
        (set running 0)))
    (begin body ...)))

(micac-macro (sdl-set-render-draw-color renderer red green blue alpha)
  (SDL_SetRenderDrawColor renderer red green blue alpha))

(micac-macro (sdl-render-clear renderer)
  (SDL_RenderClear renderer))

(micac-macro (sdl-render-present renderer)
  (SDL_RenderPresent renderer))

(micac-run
  (sdl-init)
  (create-sdl-window window "My window" 640 480)
  (create-sdl-renderer renderer window)
  (var u8 color 0)
  (sdl-event-loop
    (add color 1)
    (sdl-set-render-draw-color renderer color 0 0 #xff)
    (sdl-render-clear renderer)
    (sdl-render-present renderer)))
