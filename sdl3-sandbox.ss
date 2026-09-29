(import (chezscheme) (system))

(optimize-level 3)

(load-shared-object "libSDL3.dylib")

;; Foreign Procedure Definitions
(define sdl-init
  (foreign-procedure "SDL_Init" (unsigned-32) boolean))

(define sdl-create-window-and-renderer
  (foreign-procedure "SDL_CreateWindowAndRenderer" (string int int unsigned-64 uptr uptr) boolean))

(define sdl-create-texture
  (foreign-procedure "SDL_CreateTexture" (uptr int int int int) uptr))

(define sdl-update-texture
  (foreign-procedure "SDL_UpdateTexture" (uptr uptr uptr int) boolean))

(define sdl-render-clear
  (foreign-procedure "SDL_RenderClear" (uptr) boolean))

(define sdl-render-texture
  (foreign-procedure "SDL_RenderTexture" (uptr uptr uptr uptr) boolean))

(define sdl-render-present
  (foreign-procedure "SDL_RenderPresent" (uptr) void))

(define sdl-destroy-texture
  (foreign-procedure "SDL_DestroyTexture" (uptr) void))

(define sdl-destroy-renderer
  (foreign-procedure "SDL_DestroyRenderer" (uptr) void))

(define sdl-destroy-window
  (foreign-procedure "SDL_DestroyWindow" (uptr) void))

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

;; Constants
(define SDL_INIT_VIDEO #x00000020)
(define SDL_WINDOW_VISIBLE #x00000004)
(define SDL_WINDOW_HIGH_PIXEL_DENSITY #x00002000)
(define SDL_PIXELFORMAT_BGRA8888 376721412)
(define SDL_TEXTUREACCESS_STREAMING 1)
(define SDL_EVENT_QUIT #x100)
(define SDL_EVENT_KEY_DOWN #x300)
(define SDLK_SPACE 32)

;; Dimensions
(define BASE_WIDTH 480)
(define BASE_HEIGHT 256)
(define MATRIX_SIZE 6)

;; Physical Framebuffer Dimensions (2880 x 1536)
(define SCALED_WIDTH (fx* BASE_WIDTH MATRIX_SIZE))
(define SCALED_HEIGHT (fx* BASE_HEIGHT MATRIX_SIZE))

;; Logical Window Dimensions for 2x Retina Display
(define RETINA_SCALE 2)
(define WINDOW_WIDTH (fxquotient SCALED_WIDTH RETINA_SCALE))
(define WINDOW_HEIGHT (fxquotient SCALED_HEIGHT RETINA_SCALE))

;; Buffer sizes in bytes (4 bytes per u32 pixel)
(define SRC_BUFFER_SIZE (fx* BASE_WIDTH (fx* BASE_HEIGHT 4)))
(define SCALED_BUFFER_SIZE (fx* SCALED_WIDTH (fx* SCALED_HEIGHT 4)))

;; Exact float weights converted to 8-bit fixed-point scale (1.0f = 128)
(define *light-matrix*
  (let ([bv (make-immobile-bytevector 36)]
        [vals '#vu8( 64  90 102 102  90  64
                     90 147 173 173 147  90
                    102 173 198 198 173 102
                    102 173 198 198 173 102
                     90 147 173 173 147  90
                     64  90 102 102  90  64)])
    (let loop ([i 0])
      (if (fx< i 36)
          (begin
            (bytevector-u8-set! bv i (bytevector-u8-ref vals i))
            (loop (fx+ i 1)))
          bv))))

;; C Pointer Address Resolution for Immobile Bytevector
(define bytevector-data-pointer
  (lambda (bv)
    (fx+ (object->reference-address bv)
         (if (fx= (foreign-sizeof 'uptr) 8) 9 5))))

;; Lookup Table matching Java's exact `(int)(color * weight)` clamp to 255 logic
(define *mul-lut* (make-immobile-bytevector 65536))

(define init-mul-lut!
  (lambda ()
    (let loop-v ([v 0])
      (if (fx< v 256)
          (begin
            (let loop-w ([w 0])
              (if (fx< w 256)
                  (let* ([scaled (fxsrl (fx* v w) 7)]
                         [res (if (fx> scaled 255) 255 scaled)]
                         [offset (fx+ (fxsll v 8) w)])
                    (bytevector-u8-set! *mul-lut* offset res)
                    (loop-w (fx+ w 1)))
                  #f))
            (loop-v (fx+ v 1)))
          #f))))

(init-mul-lut!)

;; Pattern Generator - Writing Whole u32 Pixels
(define generate-source-garbage
  (lambda (src-bv frame-count)
    (let loop-y ([y 0] [src-offset 0])
      (if (fx< y BASE_HEIGHT)
          (begin
            (let loop-x ([x 0] [curr-offset src-offset])
              (if (fx< x BASE_WIDTH)
                  (let* ([t frame-count]
                         [b (fxlogand (fxlogxor x (fxlogxor y t)) #xFF)]
                         [g (fxlogand (fx+ (fx* x 3) (fx+ (fx* y 2) t)) #xFF)]
                         [r (fxlogand (fxlogxor (fx* x y) (fx* t 5)) #xFF)]
                         [a 255]
                         ;; BGRA32 / ARGB32 integer packing
                         [pixel (fxlogior (fxsll a 24)
                                          (fxlogior (fxsll r 16)
                                                    (fxlogior (fxsll g 8) b)))])
                    (bytevector-u32-native-set! src-bv curr-offset pixel)
                    (loop-x (fx+ x 1) (fx+ curr-offset 4)))
                  #f))
            (loop-y (fx+ y 1) (fx+ src-offset (fx* BASE_WIDTH 4))))
          #f))))

;; Exact Light Point Matrix Filter - 32-bit Read/Write with Increments Only
(define apply-light-point-matrix-op
  (lambda (src-bv dst-bv mat-bv lut-bv)
    (let ([scaled-stride (fx* SCALED_WIDTH 4)])
      (let loop-y ([y 0] [src-offset 0] [dst-row-base 0])
        (if (fx< y BASE_HEIGHT)
            (begin
              (let loop-x ([x 0] [curr-src src-offset] [dst-pixel-base dst-row-base])
                (if (fx< x BASE_WIDTH)
                    (let* ([argb (bytevector-u32-native-ref src-bv curr-src)]
                           [a (fxlogand (fxsrl argb 24) #xFF)]
                           [r (fxlogand (fxsrl argb 16) #xFF)]
                           [g (fxlogand (fxsrl argb 8) #xFF)]
                           [b (fxlogand argb #xFF)]
                           [r-lut-base (fxsll r 8)]
                           [g-lut-base (fxsll g 8)]
                           [b-lut-base (fxsll b 8)]
                           [alpha-part (fxsll a 24)])
                      (let loop-sub-y ([sub-y 0] [d-row dst-pixel-base] [m-row 0])
                        (if (fx< sub-y 6)
                            (begin
                              (let loop-sub-x ([sub-x 0] [d-pixel d-row] [m-idx m-row])
                                (if (fx< sub-x 6)
                                    (let* ([weight (bytevector-u8-ref mat-bv m-idx)]
                                           [pb (bytevector-u8-ref lut-bv (fx+ b-lut-base weight))]
                                           [pg (bytevector-u8-ref lut-bv (fx+ g-lut-base weight))]
                                           [pr (bytevector-u8-ref lut-bv (fx+ r-lut-base weight))]
                                           [out-pixel (fxlogior alpha-part
                                                                (fxlogior (fxsll pr 16)
                                                                          (fxlogior (fxsll pg 8) pb)))])
                                      (bytevector-u32-native-set! dst-bv d-pixel out-pixel)
                                      (loop-sub-x (fx+ sub-x 1) (fx+ d-pixel 4) (fx+ m-idx 1)))
                                    #f))
                              (loop-sub-y (fx+ sub-y 1) (fx+ d-row scaled-stride) (fx+ m-row 6)))
                            #f))
                      (loop-x (fx+ x 1) (fx+ curr-src 4) (fx+ dst-pixel-base 24)))
                    #f))
              (loop-y (fx+ y 1)
                      (fx+ src-offset (fx* BASE_WIDTH 4))
                      (fx+ dst-row-base (fx* scaled-stride 6))))
            #f)))))

;; Disabled Filter Mode: Direct 6x6 Nearest Neighbor Expansion - Single u32 Block Reads/Writes
(define apply-direct-6x-scale
  (lambda (src-bv dst-bv)
    (let ([scaled-stride (fx* SCALED_WIDTH 4)])
      (let loop-y ([y 0] [src-offset 0] [dst-row-base 0])
        (if (fx< y BASE_HEIGHT)
            (begin
              (let loop-x ([x 0] [curr-src src-offset] [dst-pixel-base dst-row-base])
                (if (fx< x BASE_WIDTH)
                    (let ([pixel (bytevector-u32-native-ref src-bv curr-src)])
                      (let loop-sub-y ([sub-y 0] [d-row dst-pixel-base])
                        (if (fx< sub-y 6)
                            (begin
                              (let loop-sub-x ([sub-x 0] [d-pixel d-row])
                                (if (fx< sub-x 6)
                                    (begin
                                      (bytevector-u32-native-set! dst-bv d-pixel pixel)
                                      (loop-sub-x (fx+ sub-x 1) (fx+ d-pixel 4)))
                                    #f))
                              (loop-sub-y (fx+ sub-y 1) (fx+ d-row scaled-stride)))
                            #f))
                      (loop-x (fx+ x 1) (fx+ curr-src 4) (fx+ dst-pixel-base 24)))
                    #f))
              (loop-y (fx+ y 1)
                      (fx+ src-offset (fx* BASE_WIDTH 4))
                      (fx+ dst-row-base (fx* scaled-stride 6))))
            #f)))))

;; Main Render Loop
(define run-main-loop
  (lambda (renderer texture src-bv dst-bv mat-bv lut-bv dst-ptr event-ptr)
    (let loop ([frame-count 0] [filter-enabled? #t])
      (let ([frame-start (sdl-get-ticks)])
        (let poll-events ([keep-running? #t] [filter-state filter-enabled?])
          (if (sdl-poll-event event-ptr)
              (let ([type (foreign-ref 'unsigned-32 event-ptr 0)])
                (cond
                  [(fx= type SDL_EVENT_QUIT)
                   (poll-events #f filter-state)]
                  [(fx= type SDL_EVENT_KEY_DOWN)
                   (let ([repeat (foreign-ref 'unsigned-8 event-ptr 32)]
                         [key (foreign-ref 'unsigned-32 event-ptr 28)])
                     (if (and (fx= key SDLK_SPACE) (fx= repeat 0))
                         (poll-events keep-running? (not filter-state))
                         (poll-events keep-running? filter-state)))]
                  [else
                   (poll-events keep-running? filter-state)]))
              (if (not keep-running?)
                  #f
                  (begin
                    (generate-source-garbage src-bv frame-count)
                    (if filter-state
                        (time (apply-light-point-matrix-op src-bv dst-bv mat-bv lut-bv))
                        (time (apply-direct-6x-scale src-bv dst-bv)))
                    (sdl-update-texture texture 0 dst-ptr (fx* SCALED_WIDTH 4))
                    (sdl-render-clear renderer)
                    (sdl-render-texture renderer texture 0 0)
                    (sdl-render-present renderer)
                    (pretty-print `(frame (count ,frame-count) (filter ,filter-state) (time ,(inexact->exact (floor (* (current-seconds) 1000))))))
                    (let* ([frame-elapsed (- (sdl-get-ticks) frame-start)]
                           [delay-needed (if (< frame-elapsed 16) (- 16 frame-elapsed) 0)])
                      (when (> delay-needed 0)
                        (sdl-delay delay-needed)))
                    (loop (fx+ frame-count 1) filter-state)))))))))

;; Main Entry Point
(define main
  (lambda ()
    (let ([init-ok? (sdl-init SDL_INIT_VIDEO)])
      (if (not init-ok?)
          (error 'main "SDL_Init failed" (sdl-get-error))
          (let ([win-alloc (foreign-alloc 8)]
                [ren-alloc (foreign-alloc 8)])
            (let ([created? (sdl-create-window-and-renderer
                               "LightPointMatrixOp - Press SPACE to toggle Filter"
                               WINDOW_WIDTH
                               WINDOW_HEIGHT
                               (bitwise-ior SDL_WINDOW_VISIBLE SDL_WINDOW_HIGH_PIXEL_DENSITY)
                               win-alloc
                               ren-alloc)])
              (if (not created?)
                  (begin
                    (foreign-free win-alloc)
                    (foreign-free ren-alloc)
                    (sdl-quit)
                    (error 'main "Failed window creation" (sdl-get-error)))
                  (let ([window (foreign-ref 'uptr win-alloc 0)]
                        [renderer (foreign-ref 'uptr ren-alloc 0)])
                    (begin
                      (foreign-free win-alloc)
                      (foreign-free ren-alloc)

                      (let ([texture (sdl-create-texture renderer
                                                         SDL_PIXELFORMAT_BGRA8888
                                                         SDL_TEXTUREACCESS_STREAMING
                                                         SCALED_WIDTH
                                                         SCALED_HEIGHT)])
                        (if (not texture)
                            (error 'main "Failed texture creation" (sdl-get-error))
                            (let ([src-bv (make-immobile-bytevector SRC_BUFFER_SIZE 0)]
                                  [dst-bv (make-immobile-bytevector SCALED_BUFFER_SIZE 0)]
                                  [event-ptr (foreign-alloc 128)])
                              (let ([dst-ptr (bytevector-data-pointer dst-bv)])
                                (begin
                                  (display "Running Loop... Press SPACE to toggle Light Point Matrix filter.\n")
                                  (run-main-loop renderer texture src-bv dst-bv *light-matrix* *mul-lut* dst-ptr event-ptr)

                                  (foreign-free event-ptr)
                                  (sdl-destroy-texture texture)
                                  (sdl-destroy-renderer renderer)
                                  (sdl-destroy-window window)
                                  (sdl-quit)))))))))))))))

(main)
