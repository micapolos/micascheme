(import (chezscheme))

(optimize-level 3)

(load-shared-object "libSDL3.dylib")

;; FFI Declarations
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

;; Constants
(define SDL_INIT_VIDEO #x00000020)
(define SDL_WINDOW_VISIBLE #x00000004)
(define SDL_WINDOW_HIGH_PIXEL_DENSITY #x00002000)
(define SDL_PIXELFORMAT_BGRA8888 376721412)
(define SDL_TEXTUREACCESS_STREAMING 1)
(define SDL_EVENT_QUIT #x100)
(define SDL_EVENT_KEY_DOWN #x300)

;; Dimensions
(define BASE_WIDTH 480)
(define BASE_HEIGHT 256)
(define MATRIX_SIZE 6)

;; Physical Framebuffer / Texture Pixel Dimensions (2880 x 1536)
(define SCALED_WIDTH (* BASE_WIDTH MATRIX_SIZE))
(define SCALED_HEIGHT (* BASE_HEIGHT MATRIX_SIZE))

;; Logical Window Dimensions for 2x Retina Display
(define RETINA_SCALE 2)
(define WINDOW_WIDTH (quotient SCALED_WIDTH RETINA_SCALE))
(define WINDOW_HEIGHT (quotient SCALED_HEIGHT RETINA_SCALE))

(define SRC_BUFFER_SIZE (* BASE_WIDTH BASE_HEIGHT 4))
(define SCALED_BUFFER_SIZE (* SCALED_WIDTH SCALED_HEIGHT 4))

;; 6x6 RADIAL_LIGHT_WEIGHTS converted to Q8 Fixed-Point Integer Weights (scaled by 256)
;; Row 0: 0.50f, 0.70f, 0.80f, 0.80f, 0.70f, 0.50f -> 128, 179, 205, 205, 179, 128
;; Row 1: 0.70f, 1.15f, 1.35f, 1.35f, 1.15f, 0.70f -> 179, 294, 346, 346, 294, 179
;; Row 2: 0.80f, 1.35f, 1.55f, 1.55f, 1.35f, 0.80f -> 205, 346, 397, 397, 346, 205
;; Row 3: 0.80f, 1.35f, 1.55f, 1.55f, 1.35f, 0.80f -> 205, 346, 397, 397, 346, 205
;; Row 4: 0.70f, 1.15f, 1.35f, 1.35f, 1.15f, 0.70f -> 179, 294, 346, 346, 294, 179
;; Row 5: 0.50f, 0.70f, 0.80f, 0.80f, 0.70f, 0.50f -> 128, 179, 205, 205, 179, 128
(define *light-matrix*
  '#(128 179 205 205 179 128
     179 294 346 346 294 179
     205 346 397 397 346 205
     205 346 397 397 346 205
     179 294 346 346 294 179
     128 179 205 205 179 128))

;; Get actual C pointer payload address from a Scheme Bytevector
(define (bytevector-data-pointer bv)
  (+ (object->reference-address bv)
     (if (= (foreign-sizeof 'uptr) 8) 9 5)))

;; Dynamic source pattern generator writing to safe Scheme bytevector
(define (generate-source-garbage src-bv frame-count)
  (let loop-y ([y 0])
    (if (< y BASE_HEIGHT)
        (let loop-x ([x 0])
          (if (< x BASE_WIDTH)
              (let ([offset (* (+ (* y BASE_WIDTH) x) 4)]
                    [t frame-count])
                (begin
                  (bytevector-u8-set! src-bv offset (bitwise-and (logxor x y t) #xFF))
                  (bytevector-u8-set! src-bv (+ offset 1) (bitwise-and (+ (* x 3) (* y 2) t) #xFF))
                  (bytevector-u8-set! src-bv (+ offset 2) (bitwise-and (logxor (* x y) (* t 5)) #xFF))
                  (bytevector-u8-set! src-bv (+ offset 3) 255)
                  (loop-x (+ x 1))))
              (loop-y (+ y 1))))
        #f)))

;; Chez Scheme port of LightPointMatrixOp filter using pure integer arithmetic
(define (apply-light-point-matrix-op src-bv dst-bv)
  (let loop-y ([y 0])
    (if (< y BASE_HEIGHT)
        (let loop-x ([x 0])
          (if (< x BASE_WIDTH)
              (let* ([src-offset (* (+ (* y BASE_WIDTH) x) 4)]
                     [b (bytevector-u8-ref src-bv src-offset)]
                     [g (bytevector-u8-ref src-bv (+ src-offset 1))]
                     [r (bytevector-u8-ref src-bv (+ src-offset 2))]
                     [a (bytevector-u8-ref src-bv (+ src-offset 3))]
                     [out-x (* x MATRIX_SIZE)]
                     [out-y (* y MATRIX_SIZE)])
                (begin
                  (let loop-sub-y ([sub-y 0])
                    (if (< sub-y MATRIX_SIZE)
                        (let* ([cur-y (+ out-y sub-y)]
                               [matrix-row-offset (* sub-y MATRIX_SIZE)])
                          (begin
                            (let loop-sub-x ([sub-x 0])
                              (if (< sub-x MATRIX_SIZE)
                                  (let* ([cur-x (+ out-x sub-x)]
                                         [dst-offset (* (+ (* cur-y SCALED_WIDTH) cur-x) 4)]
                                         [weight (vector-ref *light-matrix* (+ matrix-row-offset sub-x))]

                                         ;; Integer fixed-point multiplication (ash val -8 is division by 256)
                                         [pr (min 255 (ash (* r weight) -8))]
                                         [pg (min 255 (ash (* g weight) -8))]
                                         [pb (min 255 (ash (* b weight) -8))])
                                    (begin
                                      (bytevector-u8-set! dst-bv dst-offset pb)
                                      (bytevector-u8-set! dst-bv (+ dst-offset 1) pg)
                                      (bytevector-u8-set! dst-bv (+ dst-offset 2) pr)
                                      (bytevector-u8-set! dst-bv (+ dst-offset 3) a)
                                      (loop-sub-x (+ sub-x 1))))
                                  #f))
                            (loop-sub-y (+ sub-y 1))))
                        #f))
                  (loop-x (+ x 1))))
              (loop-y (+ y 1))))
        #f)))

;; Main render loop
(define (run-main-loop renderer texture src-bv dst-bv dst-ptr event-ptr)
  (let loop ([frame-count 0])
    (let poll ([running? #t])
      (let ([has-event? (sdl-poll-event event-ptr)])
        (if (not has-event?)
            (if (not running?)
                #f
                (begin
                  (generate-source-garbage src-bv frame-count)
                  (apply-light-point-matrix-op src-bv dst-bv)
                  (sdl-update-texture texture 0 dst-ptr (* SCALED_WIDTH 4))
                  (sdl-render-clear renderer)
                  (sdl-render-texture renderer texture 0 0)
                  (sdl-render-present renderer)
                  (loop (+ frame-count 1))))
            (let ([type (foreign-ref 'unsigned-32 event-ptr 0)])
              (if (or (= type SDL_EVENT_QUIT))
                  (poll #f)
                  (poll running?))))))))

;; Main Entry Point
(define (main)
  (let ([init-ok? (sdl-init SDL_INIT_VIDEO)])
    (if (not init-ok?)
        (error 'main "SDL_Init failed" (sdl-get-error))
        (let* ([win-alloc (foreign-alloc 8)]
               [ren-alloc (foreign-alloc 8)]
               [created? (sdl-create-window-and-renderer
                          "LightPointMatrixOp - Chez Scheme"
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
              (let* ([window (foreign-ref 'uptr win-alloc 0)]
                     [renderer (foreign-ref 'uptr ren-alloc 0)])
                (begin
                  (foreign-free win-alloc)
                  (foreign-free ren-alloc)

                  (let ([texture (sdl-create-texture renderer
                                                     SDL_PIXELFORMAT_BGRA8888
                                                     SDL_TEXTUREACCESS_STREAMING
                                                     SCALED_WIDTH
                                                     SCALED_HEIGHT)])
                    (let ([src-bv (make-bytevector SRC_BUFFER_SIZE 0)]
                          [dst-bv (make-bytevector SCALED_BUFFER_SIZE 0)]
                          [event-ptr (foreign-alloc 128)])
                      (begin
                        (lock-object dst-bv)
                        (lock-object src-bv)

                        (let ([dst-ptr (bytevector-data-pointer dst-bv)])
                          (begin
                            (display "Running LightPointMatrixOp pipeline...\n")
                            (run-main-loop renderer texture src-bv dst-bv dst-ptr event-ptr)

                            (unlock-object src-bv)
                            (unlock-object dst-bv)
                            (foreign-free event-ptr)
                            (sdl-destroy-texture texture)
                            (sdl-destroy-renderer renderer)
                            (sdl-destroy-window window)
                            (sdl-quit)))))))))))))

(main)
