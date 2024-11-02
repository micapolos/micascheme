(library (micac sdl)
  (export
    sdl-init
    sdl-create-window
    sdl-create-renderer
    sdl-event-loop
    sdl-set-render-draw-color
    sdl-render-clear
    sdl-render-present)
  (import (micascheme) (micac c))

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

  (micac-macro (print-sdl-error message)
    (printf "%s SDL Error: %s\\n" message (SDL_GetError)))

  (micac-macro (sdl-init)
    (break-if (!= (SDL_Init SDL_INIT_VIDEO) 0)
      (print-sdl-error "Could not initialize."))
    (defer (SDL_Quit)))

  (micac-macro (sdl-create-window window title width height)
    (var (* SDL_Window) window
      (SDL_CreateWindow title SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED width height 0))
    (break-if (not window)
      (print-sdl-error "Could not create window."))
    (defer (SDL_DestroyWindow window)))

  (micac-macro (sdl-create-renderer renderer window)
    (var (* SDL_Renderer) renderer
      (SDL_CreateRenderer window -1 (or SDL_RENDERER_ACCELERATED SDL_RENDERER_PRESENTVSYNC)))
    (break-if (not renderer)
      (print-sdl-error "Could not create renderer."))
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
)
