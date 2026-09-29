(import (chezscheme) (system))

(optimize-level 3)

(load-shared-object "libSDL3.dylib")

;; FFI Declarations
(define sdl-init
  (foreign-procedure "SDL_Init" (unsigned-32) boolean))

(define sdl-create-window-and-renderer
  (foreign-procedure "SDL_CreateWindowAndRenderer" (string int int unsigned-64 uptr uptr) boolean))

(define sdl-set-render-vsync
  (foreign-procedure "SDL_SetRenderVSync" (uptr int) boolean))

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

(define SRC_BUFFER_SIZE (fx* BASE_WIDTH (fx* BASE_HEIGHT 4)))
(define SCALED_BUFFER_SIZE (fx* SCALED_WIDTH (fx* SCALED_HEIGHT 4)))

;; 6x6 Matrix Weights initialized directly into an Immobile Bytevector
(define *light-matrix*
  (let ([bv (make-immobile-bytevector 36)]
        [vals '#vu8(64  90 102 102  90  64
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

;; Get actual C pointer payload address from an Immobile Bytevector
(define bytevector-data-pointer
  (lambda (bv)
    (fx+ (object->reference-address bv)
         (if (fx= (foreign-sizeof 'uptr) 8) 9 5))))

;; Precomputed 64KB Lookup Table Bytevector
(define *mul-lut* (make-immobile-bytevector 65536))

(define init-mul-lut!
  (lambda ()
    (let loop-v ([v 0])
      (if (fx< v 256)
          (begin
            (let loop-w ([w 0])
              (if (fx< w 256)
                  (let ([res (min 255 (fxsrl (fx* v w) 7))]
                        [offset (fx+ (fxsll v 8) w)])
                    (bytevector-u8-set! *mul-lut* offset res)
                    (loop-w (fx+ w 1)))
                  #f))
            (loop-v (fx+ v 1)))
          #f))))

(init-mul-lut!)

;; Dynamic source pattern generator operating directly on bytevector
(define generate-source-garbage
  (lambda (src-bv frame-count)
    (let loop-y ([y 0])
      (if (fx< y BASE_HEIGHT)
          (begin
            (let loop-x ([x 0])
              (if (fx< x BASE_WIDTH)
                  (let ([offset (fx* (fx+ (fx* y BASE_WIDTH) x) 4)]
                        [t frame-count])
                    (bytevector-u8-set! src-bv offset (fxlogand (fxlogxor x (fxlogxor y t)) #xFF))
                    (bytevector-u8-set! src-bv (fx+ offset 1) (fxlogand (fx+ (fx* x 3) (fx+ (fx* y 2) t)) #xFF))
                    (bytevector-u8-set! src-bv (fx+ offset 2) (fxlogand (fxlogxor (fx* x y) (fx* t 5)) #xFF))
                    (bytevector-u8-set! src-bv (fx+ offset 3) 255)
                    (loop-x (fx+ x 1)))
                  #f))
            (loop-y (fx+ y 1)))
          #f))))

;; Light point matrix filter operating entirely on bytevectors
(define apply-light-point-matrix-op
  (lambda (src-bv dst-bv mat-bv lut-bv)
    (let ([scaled-stride (fx* SCALED_WIDTH 4)])
      (let loop-y ([y 0])
        (if (fx< y BASE_HEIGHT)
            (begin
              (let loop-x ([x 0])
                (if (fx< x BASE_WIDTH)
                    (let* ([src-offset (fx* (fx+ (fx* y BASE_WIDTH) x) 4)]
                           [b (bytevector-u8-ref src-bv src-offset)]
                           [g (bytevector-u8-ref src-bv (fx+ src-offset 1))]
                           [r (bytevector-u8-ref src-bv (fx+ src-offset 2))]
                           [a (bytevector-u8-ref src-bv (fx+ src-offset 3))]
                           [r-lut-base (fxsll r 8)]
                           [g-lut-base (fxsll g 8)]
                           [b-lut-base (fxsll b 8)]
                           [out-x (fx* x 6)]
                           [out-y (fx* y 6)]
                           [dst-base (fx+ (fx* out-y scaled-stride) (fx* out-x 4))])
                      (let loop-sub-y ([sub-y 0])
                        (if (fx< sub-y 6)
                            (let ([d-row (fx+ dst-base (fx* sub-y scaled-stride))]
                                  [m-row (fx* sub-y 6)])
                              (let loop-sub-x ([sub-x 0])
                                (if (fx< sub-x 6)
                                    (let* ([weight (bytevector-u8-ref mat-bv (fx+ m-row sub-x))]
                                           [d-pixel (fx+ d-row (fx* sub-x 4))]
                                           [pb (bytevector-u8-ref lut-bv (fx+ b-lut-base weight))]
                                           [pg (bytevector-u8-ref lut-bv (fx+ g-lut-base weight))]
                                           [pr (bytevector-u8-ref lut-bv (fx+ r-lut-base weight))])
                                      (bytevector-u8-set! dst-bv d-pixel pb)
                                      (bytevector-u8-set! dst-bv (fx+ d-pixel 1) pg)
                                      (bytevector-u8-set! dst-bv (fx+ d-pixel 2) pr)
                                      (bytevector-u8-set! dst-bv (fx+ d-pixel 3) a)
                                      (loop-sub-x (fx+ sub-x 1)))
                                    #f))
                              (loop-sub-y (fx+ sub-y 1)))
                            #f))
                      (loop-x (fx+ x 1)))
                    #f))
              (loop-y (fx+ y 1)))
            #f)))))

;; Main render loop capped to 60 FPS (~16.66ms target frame budget)
(define run-main-loop
  (lambda (renderer texture src-bv dst-bv mat-bv lut-bv dst-ptr event-ptr)
    (let loop ([frame-count 0])
      (let ([frame-start (sdl-get-ticks)])
        (let poll ([running? #t])
          (let ([has-event? (sdl-poll-event event-ptr)])
            (if (not has-event?)
                (if (not running?)
                    #f
                    (begin
                      (generate-source-garbage src-bv frame-count)
                      (apply-light-point-matrix-op src-bv dst-bv mat-bv lut-bv)
                      (sdl-update-texture texture 0 dst-ptr (fx* SCALED_WIDTH 4))
                      (sdl-render-clear renderer)
                      (sdl-render-texture renderer texture 0 0)
                      (sdl-render-present renderer)
                      (pretty-print `(frame (count ,frame-count) (time ,(inexact->exact (floor (* (current-seconds) 1000))))))
                      (let* ([frame-elapsed (- (sdl-get-ticks) frame-start)]
                             [delay-needed (if (< frame-elapsed 16) (- 16 frame-elapsed) 0)])
                        (when (> delay-needed 0)
                          (sdl-delay delay-needed)))
                      (loop (fx+ frame-count 1))))
                (let ([type (foreign-ref 'unsigned-32 event-ptr 0)])
                  (if (fx= type SDL_EVENT_QUIT)
                      (poll #f)
                      (poll running?))))))))))

;; Main Entry Point
(define main
  (lambda ()
    (let ([init-ok? (sdl-init SDL_INIT_VIDEO)])
      (if (not init-ok?)
          (error 'main "SDL_Init failed" (sdl-get-error))
          (let ([win-alloc (foreign-alloc 8)]
                [ren-alloc (foreign-alloc 8)])
            (let ([created? (sdl-create-window-and-renderer
                               "LightPointMatrixOp - Immobile Bytevector Loop"
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

                      (sdl-set-render-vsync renderer 1)

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
                                  (display "Running Immobile Bytevector Processing Loop...\n")
                                  (run-main-loop renderer texture src-bv dst-bv *light-matrix* *mul-lut* dst-ptr event-ptr)

                                  (foreign-free event-ptr)
                                  (sdl-destroy-texture texture)
                                  (sdl-destroy-renderer renderer)
                                  (sdl-destroy-window window)
                                  (sdl-quit)))))))))))))))

(main)
