(import (chezscheme) (system))

;; Load shared libraries
(load-shared-object "libSDL3.dylib")
(load-shared-object "libSDL3_image.dylib")

;; FFI Declarations
(define sdl-init
  (foreign-procedure "SDL_Init" (unsigned-32) boolean))

(define sdl-create-window-and-renderer
  (foreign-procedure "SDL_CreateWindowAndRenderer" (string int int unsigned-64 uptr uptr) boolean))

(define sdl-set-render-vsync
  (foreign-procedure "SDL_SetRenderVSync" (uptr int) boolean))

(define sdl-set-render-logical-presentation
  (foreign-procedure "SDL_SetRenderLogicalPresentation" (uptr int int int) boolean))

(define SDL_LOGICAL_PRESENTATION_LETTERBOX 1)
(define SDL_WINDOW_RESIZABLE #x00000020)

(define img-load
  (foreign-procedure "IMG_Load" (string) uptr))

(define sdl-create-texture-from-surface
  (foreign-procedure "SDL_CreateTextureFromSurface" (uptr uptr) uptr))

(define sdl-destroy-surface
  (foreign-procedure "SDL_DestroySurface" (uptr) void))

(define sdl-destroy-texture
  (foreign-procedure "SDL_DestroyTexture" (uptr) void))

(define sdl-destroy-renderer
  (foreign-procedure "SDL_DestroyRenderer" (uptr) void))

(define sdl-destroy-window
  (foreign-procedure "SDL_DestroyWindow" (uptr) void))

(define sdl-poll-event
  (foreign-procedure "SDL_PollEvent" (uptr) boolean))

(define sdl-render-clear
  (foreign-procedure "SDL_RenderClear" (uptr) boolean))

(define sdl-render-texture
  (foreign-procedure "SDL_RenderTexture" (uptr uptr uptr uptr) boolean))

(define sdl-render-present
  (foreign-procedure "SDL_RenderPresent" (uptr) void))

(define sdl-set-texture-scale-mode
  (foreign-procedure "SDL_SetTextureScaleMode" (uptr int) boolean))

(define (get-surface-width surface-ptr)
  (foreign-ref 'int surface-ptr 16))

(define (get-surface-height surface-ptr)
  (foreign-ref 'int surface-ptr 20))

(define SDL_SCALEMODE_NEAREST 0)
(define SDL_SCALEMODE_LINEAR 1)

(define sdl-quit
  (foreign-procedure "SDL_Quit" () void))

(define sdl-get-error
  (foreign-procedure "SDL_GetError" () string))

;; Constants
(define SDL_INIT_VIDEO #x00000020)
(define SDL_WINDOW_VISIBLE #x00000004)
(define SDL_EVENT_QUIT #x100)
(define SDL_EVENT_KEY_DOWN #x300)

;; Helper to read event type from SDL_Event buffer
(define (get-event-type event-ptr)
  (foreign-ref 'unsigned-32 event-ptr 0))

;; Main loop capped to screen refresh rate via VSync
(define (run-main-loop renderer texture event-ptr)
  (let loop ()
    (let poll ([running? #t])
      (let ([has-event? (sdl-poll-event event-ptr)])
        (if (not has-event?)
            (if (not running?)
                #f ; Exit main loop
                (begin
                  ;; Clear render target
                  (sdl-render-clear renderer)

                  ;; Draw texture full window
                  (sdl-render-texture renderer texture 0 0)

                  ;; SDL_RenderPresent blocks until the display's V-Blank interval.
                  ;; This dynamically caps FPS to display refresh rate (e.g. 60Hz, 120Hz, 144Hz)
                  (sdl-render-present renderer)

                  (pretty-print (current-seconds))

                  (loop)))
            (let ([event-type (get-event-type event-ptr)])
              (if (or (= event-type SDL_EVENT_QUIT))
                  (poll #f)
                  (poll running?))))))))

;; Main Execution Entry Point
(define (main)
  (let ([init-ok? (sdl-init SDL_INIT_VIDEO)])
    (if (not init-ok?)
        (error 'main "SDL_Init failed" (sdl-get-error))
        (let* ([win-ptr-alloc (foreign-alloc 8)]
               [ren-ptr-alloc (foreign-alloc 8)]
               [created? (sdl-create-window-and-renderer
                          "SDL3 Refresh Rate Capped" 1024 128
                          (bitwise-ior SDL_WINDOW_VISIBLE SDL_WINDOW_RESIZABLE) win-ptr-alloc ren-ptr-alloc)])
          (if (not created?)
              (begin
                (foreign-free win-ptr-alloc)
                (foreign-free ren-ptr-alloc)
                (sdl-quit)
                (error 'main "Failed to create window and renderer" (sdl-get-error)))
              (let* ([window (foreign-ref 'uptr win-ptr-alloc 0)]
                     [renderer (foreign-ref 'uptr ren-ptr-alloc 0)])
                (begin
                  (foreign-free win-ptr-alloc)
                  (foreign-free ren-ptr-alloc)

                  ;; Enable VSync (1 = Sync to monitor refresh rate)
                  (sdl-set-render-vsync renderer 1)

                  (let ([surface (img-load "image.png")])
                    (if (zero? surface)
                        (begin
                          (sdl-destroy-renderer renderer)
                          (sdl-destroy-window window)
                          (sdl-quit)
                          (error 'main "Failed to load PNG image" (sdl-get-error)))
                        (let ([img-width (get-surface-width surface)]
                            [img-height (get-surface-height surface)]
                            [texture (sdl-create-texture-from-surface renderer surface)])
                          (begin
                            (sdl-destroy-surface surface)
                            (if (zero? texture)
                                (begin
                                  (sdl-destroy-renderer renderer)
                                  (sdl-destroy-window window)
                                  (sdl-quit)
                                  (error 'main "Failed to create texture" (sdl-get-error)))
                                (let ([event-ptr (foreign-alloc 128)])
                                  (begin
                                    (sdl-set-texture-scale-mode texture SDL_SCALEMODE_NEAREST)

                                    ; (sdl-set-render-logical-presentation
                                    ;    renderer
                                    ;    256
                                    ;    32
                                    ;    SDL_LOGICAL_PRESENTATION_LETTERBOX)

                                    ;; Run event loop
                                    (run-main-loop renderer texture event-ptr)

                                    ;; Teardown
                                    (foreign-free event-ptr)
                                    (sdl-destroy-texture texture)
                                    (sdl-destroy-renderer renderer)
                                    (sdl-destroy-window window)
                                    (sdl-quit)))))))))))))))

(main)
