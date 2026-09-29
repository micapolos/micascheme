(import (chezscheme))

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
    (letrec ([copy-loop
              (lambda (i)
                (if (fx< i 36)
                    (begin
                      (bytevector-u8-set! bv i (bytevector-u8-ref vals i))
                      (copy-loop (fx+ i 1)))
                    bv))])
      (copy-loop 0))))

;; Get actual C pointer payload address from an Immobile Bytevector
(define bytevector-data-pointer
  (lambda (bv)
    (fx+ (object->reference-address bv)
         (if (fx= (foreign-sizeof 'uptr) 8) 9 5))))

;; Precomputed 64KB Lookup Table for Fixed-Point Weight Multiplications
(define *mul-lut* (foreign-alloc 65536))

(define init-mul-lut!
  (lambda ()
    (letrec ([loop-val
              (lambda (v)
                (if (fx< v 256)
                    (letrec ([loop-w
                              (lambda (w)
                                (if (fx< w 256)
                                    (let ([res (min 255 (fxsrl (fx* v w) 7))]
                                          [offset (fx+ (fxsll v 8) w)])
                                      (begin
                                        (foreign-set! 'unsigned-8 *mul-lut* offset res)
                                        (loop-w (fx+ w 1))))
                                    (loop-val (fx+ v 1))))])
                      (loop-w 0))
                    #f))])
      (loop-val 0))))

(init-mul-lut!)

;; Dynamic source pattern generator
(define generate-source-garbage
  (lambda (src-bv frame-count)
    (letrec ([loop-y
              (lambda (y)
                (if (fx< y BASE_HEIGHT)
                    (letrec ([loop-x
                              (lambda (x)
                                (if (fx< x BASE_WIDTH)
                                    (let ([offset (fx* (fx+ (fx* y BASE_WIDTH) x) 4)]
                                          [t frame-count])
                                      (begin
                                        (bytevector-u8-set! src-bv offset (fxlogand (fxlogxor x (fxlogxor y t)) #xFF))
                                        (bytevector-u8-set! src-bv (fx+ offset 1) (fxlogand (fx+ (fx* x 3) (fx+ (fx* y 2) t)) #xFF))
                                        (bytevector-u8-set! src-bv (fx+ offset 2) (fxlogand (fxlogxor (fx* x y) (fx* t 5)) #xFF))
                                        (bytevector-u8-set! src-bv (fx+ offset 3) 255)
                                        (loop-x (fx+ x 1))))
                                    (loop-y (fx+ y 1))))])
                      (loop-x 0))
                    #f))])
      (loop-y 0))))

;; Ultra-fast raw pointer light point matrix filter using LUT lookups
(define apply-light-point-matrix-op
  (lambda (src-ptr dst-ptr mat-ptr lut-ptr)
    (let ([scaled-stride (fx* SCALED_WIDTH 4)])
      (letrec ([loop-y
                (lambda (y)
                  (if (fx< y BASE_HEIGHT)
                      (letrec ([loop-x
                                (lambda (x)
                                  (if (fx< x BASE_WIDTH)
                                      (let* ([src-offset (fx* (fx+ (fx* y BASE_WIDTH) x) 4)]
                                             [s-ptr (fx+ src-ptr src-offset)]
                                             [b (foreign-ref 'unsigned-8 s-ptr 0)]
                                             [g (foreign-ref 'unsigned-8 s-ptr 1)]
                                             [r (foreign-ref 'unsigned-8 s-ptr 2)]
                                             [a (foreign-ref 'unsigned-8 s-ptr 3)]
                                             [r-lut-base (fx+ lut-ptr (fxsll r 8))]
                                             [g-lut-base (fx+ lut-ptr (fxsll g 8))]
                                             [b-lut-base (fx+ lut-ptr (fxsll b 8))]
                                             [out-x (fx* x 6)]
                                             [out-y (fx* y 6)]
                                             [dst-base (fx+ dst-ptr (fx+ (fx* out-y scaled-stride) (fx* out-x 4)))])
                                        (begin
                                          (letrec ([loop-sub-y
                                                    (lambda (sub-y)
                                                      (if (fx< sub-y 6)
                                                          (let* ([d-row (fx+ dst-base (fx* sub-y scaled-stride))]
                                                                 [m-row (fx+ mat-ptr (fx* sub-y 6))])
                                                            (begin
                                                              (letrec ([loop-sub-x
                                                                        (lambda (sub-x)
                                                                          (if (fx< sub-x 6)
                                                                              (let* ([weight (foreign-ref 'unsigned-8 m-row sub-x)]
                                                                                     [d-pixel (fx+ d-row (fx* sub-x 4))]
                                                                                     [pb (foreign-ref 'unsigned-8 b-lut-base weight)]
                                                                                     [pg (foreign-ref 'unsigned-8 g-lut-base weight)]
                                                                                     [pr (foreign-ref 'unsigned-8 r-lut-base weight)])
                                                                                (begin
                                                                                  (foreign-set! 'unsigned-8 d-pixel 0 pb)
                                                                                  (foreign-set! 'unsigned-8 d-pixel 1 pg)
                                                                                  (foreign-set! 'unsigned-8 d-pixel 2 pr)
                                                                                  (foreign-set! 'unsigned-8 d-pixel 3 a)
                                                                                  (loop-sub-x (fx+ sub-x 1))))
                                                                              #f))])
                                                                (loop-sub-x 0))
                                                              (loop-sub-y (fx+ sub-y 1))))
                                                          #f))])
                                            (loop-sub-y 0))
                                          (loop-x (fx+ x 1))))
                                      (loop-y (fx+ y 1))))])
                        (loop-x 0))
                      #f))])
        (loop-y 0)))))

;; Zero-allocation main render loop
(define run-main-loop
  (lambda (renderer texture src-bv dst-bv src-ptr dst-ptr mat-ptr lut-ptr event-ptr)
    (letrec ([loop
              (lambda (frame-count)
                (letrec ([poll
                          (lambda (running?)
                            (let ([has-event? (sdl-poll-event event-ptr)])
                              (if (not has-event?)
                                  (if (not running?)
                                      #f
                                      (begin
                                        (time
                                          (begin
                                            (time (generate-source-garbage src-bv frame-count))
                                            (time (apply-light-point-matrix-op src-ptr dst-ptr mat-ptr lut-ptr))
                                            (time (sdl-update-texture texture 0 dst-ptr (fx* SCALED_WIDTH 4)))
                                            (sdl-render-clear renderer)
                                            (sdl-render-texture renderer texture 0 0)
                                            (sdl-render-present renderer)))
                                        (loop (fx+ frame-count 1))))
                                  (let ([type (foreign-ref 'unsigned-32 event-ptr 0)])
                                    (if (fx= type SDL_EVENT_QUIT)
                                        (poll #f)
                                        (poll running?))))))])
                  (poll #t)))])
      (loop 0))))

;; Main Entry Point
(define main
  (lambda ()
    (let ([init-ok? (sdl-init SDL_INIT_VIDEO)])
      (if (not init-ok?)
          (error 'main "SDL_Init failed" (sdl-get-error))
          (let ([win-alloc (foreign-alloc 8)]
                [ren-alloc (foreign-alloc 8)])
            (let ([created? (sdl-create-window-and-renderer
                             "LightPointMatrixOp - Zero Allocation Loop"
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
                              (let ([src-ptr (bytevector-data-pointer src-bv)]
                                    [dst-ptr (bytevector-data-pointer dst-bv)]
                                    [mat-ptr (bytevector-data-pointer *light-matrix*)])
                                (begin
                                  (display "Running Zero-Allocation Loop...\n")
                                  (run-main-loop renderer texture src-bv dst-bv src-ptr dst-ptr mat-ptr *mul-lut* event-ptr)

                                  (foreign-free *mul-lut*)
                                  (foreign-free event-ptr)
                                  (sdl-destroy-texture texture)
                                  (sdl-destroy-renderer renderer)
                                  (sdl-destroy-window window)
                                  (sdl-quit)))))))))))))))

(main)
