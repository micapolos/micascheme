(library (sdl3)
  (export
    sdl-init
    sdl-create-window
    sdl-create-renderer
    sdl-create-window-and-renderer
    sdl-get-window-surface
    sdl-update-window-surface
    sdl-blit-surface
    sdl-destroy-window
    sdl-destroy-renderer
    sdl-poll-event
    sdl-quit
    sdl-get-error
    sdl-get-ticks
    sdl-delay
    sdl-create-surface-from
    sdl-convert-surface
    sdl-destroy-surface
    sdl-create-texture
    sdl-update-texture
    sdl-destroy-texture
    sdl-render-texture
    sdl-render-clear
    sdl-set-render-draw-color
    sdl-render-present
    sdl-set-texture-blend-mode
    sdl-set-texture-scale-mode
    SDL_INIT_VIDEO
    SDL_WINDOW_VISIBLE
    SDL_WINDOW_HIGH_PIXEL_DENSITY
    SDL_PIXELFORMAT_BGRA8888
    SDL_PIXELFORMAT_ABGR8888
    SDL_TEXTUREACCESS_STATIC
    SDL_TEXTUREACCESS_STREAMING
    SDL_BLENDMODE_NONE
    SDL_BLENDMODE_BLEND
    SDL_BLENDMODE_ADD
    SDL_BLENDMODE_MOD
    SDL_SCALEMODE_NEAREST
    SDL_SCALEMODE_LINEAR
    SDL_SCALEMODE_BEST
    SDL_EVENT_QUIT
    SDL_EVENT_KEY_DOWN
    SDLK_SPACE)
  (import
    (scheme)
    (shared-library))

  (define *sdl3* (load-shared-library "SDL3"))

  ;; 2. Foreign procedures execute after *sdl3* is initialized
  (define sdl-init
    (foreign-procedure "SDL_Init" (unsigned-32) boolean))

  (define sdl-create-window
    (foreign-procedure "SDL_CreateWindow" (string int int unsigned-64) uptr))

  (define sdl-create-renderer
    (foreign-procedure "SDL_CreateRenderer" (uptr string) uptr))

  (define sdl-create-window-and-renderer
    (foreign-procedure "SDL_CreateWindowAndRenderer" (string int int unsigned-64 uptr uptr) boolean))

  (define sdl-get-window-surface
    (foreign-procedure "SDL_GetWindowSurface" (uptr) uptr))

  (define sdl-update-window-surface
    (foreign-procedure "SDL_UpdateWindowSurface" (uptr) boolean))

  (define sdl-destroy-window
    (foreign-procedure "SDL_DestroyWindow" (uptr) void))

  (define sdl-destroy-renderer
    (foreign-procedure "SDL_DestroyRenderer" (uptr) void))

  (define sdl-poll-event
    (foreign-procedure "SDL_PollEvent" (uptr) boolean))

  (define sdl-quit
    (foreign-procedure "SDL_Quit" () void))

  (define sdl-get-error
    (foreign-procedure "SDL_GetError" () string))

  (define sdl-get-ticks
    (foreign-procedure "SDL_GetTicks" () unsigned-64))

  (define sdl-delay
    (foreign-procedure "SDL_Delay" (unsigned-32) void))

  (define sdl-blit-surface
    (foreign-procedure "SDL_BlitSurface" (uptr uptr uptr uptr) boolean))

  (define sdl-create-surface-from
    (foreign-procedure "SDL_CreateSurfaceFrom" (uptr int int int unsigned-32) uptr))

  (define sdl-convert-surface
    (foreign-procedure "SDL_ConvertSurface" (uptr int) uptr))

  (define sdl-destroy-surface
    (foreign-procedure "SDL_DestroySurface" (uptr) void))

  (define sdl-create-texture
    (foreign-procedure "SDL_CreateTexture" (uptr unsigned-32 int int int) uptr))

  (define sdl-update-texture
    (foreign-procedure "SDL_UpdateTexture" (uptr uptr uptr int) boolean))

  (define sdl-destroy-texture
    (foreign-procedure "SDL_DestroyTexture" (uptr) void))

  (define sdl-render-texture
    (foreign-procedure "SDL_RenderTexture" (uptr uptr uptr uptr) boolean))

  (define sdl-render-clear
    (foreign-procedure "SDL_RenderClear" (uptr) boolean))

  (define sdl-set-render-draw-color
    (foreign-procedure "SDL_SetRenderDrawColor" (uptr unsigned-8 unsigned-8 unsigned-8 unsigned-8) boolean))

  (define sdl-render-present
    (foreign-procedure "SDL_RenderPresent" (uptr) boolean))

  (define sdl-set-texture-blend-mode
    (foreign-procedure "SDL_SetTextureBlendMode" (uptr int) boolean))

  (define sdl-set-texture-scale-mode
    (foreign-procedure "SDL_SetTextureScaleMode" (uptr int) boolean))

  ;; Constants
  (define SDL_INIT_VIDEO #x00000020)
  (define SDL_WINDOW_VISIBLE #x00000004)
  (define SDL_WINDOW_HIGH_PIXEL_DENSITY #x00002000)

  (define SDL_PIXELFORMAT_BGRA8888 376721412)
  (define SDL_PIXELFORMAT_ABGR8888 376840196)

  (define SDL_TEXTUREACCESS_STATIC 0)
  (define SDL_TEXTUREACCESS_STREAMING 1)

  (define SDL_BLENDMODE_NONE 0)
  (define SDL_BLENDMODE_BLEND 1)
  (define SDL_BLENDMODE_ADD 2)
  (define SDL_BLENDMODE_MOD 3)

  (define SDL_SCALEMODE_NEAREST 0)
  (define SDL_SCALEMODE_LINEAR 1)
  (define SDL_SCALEMODE_BEST 2)

  (define SDL_EVENT_QUIT #x100)
  (define SDL_EVENT_KEY_DOWN #x300)
  (define SDLK_SPACE 32)
)
