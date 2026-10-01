(import
  (scheme)
  (lets)
  (sdl3)
  (syntax)
  (procedure)
  (sdl3-image)
  (mica-sdl3)
  (vstack))

(define PIXEL_FORMAT SDL_PIXELFORMAT_RGBA8888)

;; Dimensions
(define BASE_WIDTH 480)
(define BASE_HEIGHT 256)
(define MATRIX_SIZE 6)

;; Physical Framebuffer Dimensions (2880 x 1536)
(define SCALED_WIDTH (fx*/wraparound BASE_WIDTH MATRIX_SIZE))
(define SCALED_HEIGHT (fx*/wraparound BASE_HEIGHT MATRIX_SIZE))

;; Logical Window Dimensions for 2x Retina Display
(define RETINA_SCALE 2)
(define WINDOW_WIDTH (fxquotient SCALED_WIDTH RETINA_SCALE))
(define WINDOW_HEIGHT (fxquotient SCALED_HEIGHT RETINA_SCALE))

;; Buffer sizes in bytes (4 bytes per u32 pixel)
(define SRC_BUFFER_SIZE (fx*/wraparound BASE_WIDTH (fx*/wraparound BASE_HEIGHT 4)))
(define SCALED_BUFFER_SIZE (fx*/wraparound SCALED_WIDTH (fx*/wraparound SCALED_HEIGHT 4)))

;; Exact float weights converted to 8-bit fixed-point scale (1.0f = 128)
(define light-matrix
  (bytevector
    64 90 102 102 90 64
    90 147 173 173 147 90
    102 173 198 198 173 102
    102 173 198 198 173 102
    90 147 173 173 147 90
    64 90 102 102 90 64))

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
                         [g (fxlogand (fx+/wraparound (fx*/wraparound x 3) (fx+/wraparound (fx*/wraparound y 2) t)) #xFF)]
                         [r (fxlogand (fxlogxor (fx*/wraparound x y) (fx*/wraparound t 5)) #xFF)]
                         [a 255]
                         [pixel (fxlogior (fxsll a 0)
                                          (fxlogior (fxsll b 24)
                                                    (fxlogior (fxsll g 16) (fxsll r 8))))])
                    (bytevector-u32-native-set! src-bv curr-offset pixel)
                    (loop-x (fx+/wraparound x 1) (fx+/wraparound curr-offset 4)))
                  #f))
            (loop-y (fx+/wraparound y 1) (fx+/wraparound src-offset (fx*/wraparound BASE_WIDTH 4))))
          #f))))

;; Pattern Generator - Writing Whole u32 Pixels
(define (clear-bv src-bv)
  (repeat-indexed ($index (* BASE_WIDTH BASE_HEIGHT))
    (bytevector-u32-native-set! src-bv (fxsll $index 2) #x000000ff)))

(define-rule-syntax (color-rgba u32)
  (let
    (($u32 u32))
    (values
      (fxlogand (fxsrl $u32 24) #xff)
      (fxlogand (fxsrl $u32 16) #xff)
      (fxlogand (fxsrl $u32 8) #xff)
      (fxlogand $u32 #xff))))

(define-rule-syntax (rgba-color r g b a)
  (fxlogior
    (fxsll r 24)
    (fxsll g 16)
    (fxsll b 8)
    a))

;; Fully Unrolled 6x6 Light Point Matrix Filter using Inline Arithmetic & fxmin Clamping
(define apply-light-point-matrix-op
  (lambda (src-bv dst-bv mat-bv)
    (let ([scaled-stride (fx*/wraparound SCALED_WIDTH 4)])
      (let loop-y ([y 0] [src-offset 0] [dst-row-base 0])
        (if (fx< y BASE_HEIGHT)
            (begin
              (let loop-x ([x 0] [curr-src src-offset] [dst-pixel-base dst-row-base])
                (if (fx< x BASE_WIDTH)
                    (let-values
                      (((r g b a) (color-rgba (bytevector-u32-native-ref src-bv curr-src))))
                      (let* ([row0 dst-pixel-base]
                             [row1 (fx+/wraparound row0 scaled-stride)]
                             [row2 (fx+/wraparound row1 scaled-stride)]
                             [row3 (fx+/wraparound row2 scaled-stride)]
                             [row4 (fx+/wraparound row3 scaled-stride)]
                             [row5 (fx+/wraparound row4 scaled-stride)])

                        (let-syntax
                          ((make-px
                            (syntax-rules ()
                              ((_ w)
                               (let (($w w))
                                 (rgba-color
                                   (fxmin 255 (fxsrl (fx*/wraparound r $w) 7))
                                   (fxmin 255 (fxsrl (fx*/wraparound g $w) 7))
                                   (fxmin 255 (fxsrl (fx*/wraparound b $w) 7))
                                   a))))))

                          ;; Row 0
                          (bytevector-u32-native-set! dst-bv row0 (make-px (bytevector-u8-ref mat-bv 0)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 4) (make-px (bytevector-u8-ref mat-bv 1)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 8) (make-px (bytevector-u8-ref mat-bv 2)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 12) (make-px (bytevector-u8-ref mat-bv 3)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 16) (make-px (bytevector-u8-ref mat-bv 4)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 20) (make-px (bytevector-u8-ref mat-bv 5)))

                          ;; Row 1
                          (bytevector-u32-native-set! dst-bv row1 (make-px (bytevector-u8-ref mat-bv 6)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 4) (make-px (bytevector-u8-ref mat-bv 7)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 8) (make-px (bytevector-u8-ref mat-bv 8)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 12) (make-px (bytevector-u8-ref mat-bv 9)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 16) (make-px (bytevector-u8-ref mat-bv 10)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 20) (make-px (bytevector-u8-ref mat-bv 11)))

                          ;; Row 2
                          (bytevector-u32-native-set! dst-bv row2 (make-px (bytevector-u8-ref mat-bv 12)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 4) (make-px (bytevector-u8-ref mat-bv 13)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 8) (make-px (bytevector-u8-ref mat-bv 14)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 12) (make-px (bytevector-u8-ref mat-bv 15)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 16) (make-px (bytevector-u8-ref mat-bv 16)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 20) (make-px (bytevector-u8-ref mat-bv 17)))

                          ;; Row 3
                          (bytevector-u32-native-set! dst-bv row3 (make-px (bytevector-u8-ref mat-bv 18)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 4) (make-px (bytevector-u8-ref mat-bv 19)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 8) (make-px (bytevector-u8-ref mat-bv 20)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 12) (make-px (bytevector-u8-ref mat-bv 21)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 16) (make-px (bytevector-u8-ref mat-bv 22)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 20) (make-px (bytevector-u8-ref mat-bv 23)))

                          ;; Row 4
                          (bytevector-u32-native-set! dst-bv row4 (make-px (bytevector-u8-ref mat-bv 24)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 4) (make-px (bytevector-u8-ref mat-bv 25)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 8) (make-px (bytevector-u8-ref mat-bv 26)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 12) (make-px (bytevector-u8-ref mat-bv 27)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 16) (make-px (bytevector-u8-ref mat-bv 28)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 20) (make-px (bytevector-u8-ref mat-bv 29)))

                          ;; Row 5
                          (bytevector-u32-native-set! dst-bv row5 (make-px (bytevector-u8-ref mat-bv 30)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 4) (make-px (bytevector-u8-ref mat-bv 31)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 8) (make-px (bytevector-u8-ref mat-bv 32)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 12) (make-px (bytevector-u8-ref mat-bv 33)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 16) (make-px (bytevector-u8-ref mat-bv 34)))
                          (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 20) (make-px (bytevector-u8-ref mat-bv 35))))

                        (loop-x (fx+/wraparound x 1) (fx+/wraparound curr-src 4) (fx+/wraparound dst-pixel-base 24)))
                    #f)))
              (loop-y (fx+/wraparound y 1)
                      (fx+/wraparound src-offset (fx*/wraparound BASE_WIDTH 4))
                      (fx+/wraparound dst-row-base (fx*/wraparound scaled-stride 6))))
          #f)))))

;; Fully Unrolled 6x6 Nearest Neighbor Expansion
(define apply-direct-6x-scale
  (lambda (src-bv dst-bv)
    (let ([scaled-stride (fx*/wraparound SCALED_WIDTH 4)])
      (let loop-y ([y 0] [src-offset 0] [dst-row-base 0])
        (if (fx< y BASE_HEIGHT)
            (begin
              (let loop-x ([x 0] [curr-src src-offset] [dst-pixel-base dst-row-base])
                (if (fx< x BASE_WIDTH)
                    (let* ([pixel (bytevector-u32-native-ref src-bv curr-src)]
                           [row0 dst-pixel-base]
                           [row1 (fx+/wraparound row0 scaled-stride)]
                           [row2 (fx+/wraparound row1 scaled-stride)]
                           [row3 (fx+/wraparound row2 scaled-stride)]
                           [row4 (fx+/wraparound row3 scaled-stride)]
                           [row5 (fx+/wraparound row4 scaled-stride)])

                      ;; Row 0
                      (bytevector-u32-native-set! dst-bv row0 pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 4) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 8) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 12) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 16) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row0 20) pixel)

                      ;; Row 1
                      (bytevector-u32-native-set! dst-bv row1 pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 4) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 8) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 12) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 16) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row1 20) pixel)

                      ;; Row 2
                      (bytevector-u32-native-set! dst-bv row2 pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 4) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 8) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 12) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 16) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row2 20) pixel)

                      ;; Row 3
                      (bytevector-u32-native-set! dst-bv row3 pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 4) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 8) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 12) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 16) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row3 20) pixel)

                      ;; Row 4
                      (bytevector-u32-native-set! dst-bv row4 pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 4) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 8) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 12) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 16) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row4 20) pixel)

                      ;; Row 5
                      (bytevector-u32-native-set! dst-bv row5 pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 4) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 8) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 12) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 16) pixel)
                      (bytevector-u32-native-set! dst-bv (fx+/wraparound row5 20) pixel)

                      (loop-x (fx+/wraparound x 1) (fx+/wraparound curr-src 4) (fx+/wraparound dst-pixel-base 24)))
                    #f))
              (loop-y (fx+/wraparound y 1)
                      (fx+/wraparound src-offset (fx*/wraparound BASE_WIDTH 4))
                      (fx+/wraparound dst-row-base (fx*/wraparound scaled-stride 6))))
          #f)))))

;; Event Queue Drain Helper (Desugared & Linear)
(define (drain-events $event $keep-running? $filter-state)
  (if (sdl-poll-event $event)
    (lets
      ($type (sdl-event-type $event))
      (cond
        ((fx= $type SDL_EVENT_QUIT)
          (drain-events $event #f $filter-state))
        ((fx= $type SDL_EVENT_KEY_DOWN)
          (lets
            ($repeat? (foreign-ref 'unsigned-8 $event 32))
            ($key (foreign-ref 'unsigned-32 $event 28))
            (if (and (fx= $key SDLK_SPACE) (fx= $repeat? 0))
              (drain-events $event $keep-running? (not $filter-state))
              (drain-events $event $keep-running? $filter-state))))
        (else
          (drain-events $event $keep-running? $filter-state))))
    (values $keep-running? $filter-state)))

;; Main Render Loop
(define run-main-loop
  (lambda (window src-bv dst-bv src-surface dst-surface mat-bv)
    (with-vstack (sp 1024)
      (with-sdl-png-surface ($chicken-surface "/Users/micapolos/git/Tata8/res/micapolos/depressedChicken.png")
        (with-sdl-png-surface ($tilemap-surface "/Users/micapolos/git/Tata8/res/micapolos/tilemap.png")
          (vstack-let sp
            ($event (ftype-sizeof SDL_Event))
            ($src-rect (ftype-sizeof SDL_Rect))
            ($dst-rect (ftype-sizeof SDL_Rect))
              (let loop ([frame-count 0] [filter-enabled? #t])
                (let ([frame-start (sdl-get-ticks)])
                  (let-values ([(keep-running? filter-state) (drain-events $event #t filter-enabled?)])
                    (if (not keep-running?)
                      #f
                      (begin
                        (clear-bv src-bv)
                        (generate-source-garbage src-bv frame-count)
                        (sdl-rect-set-xywh! $src-rect (fx*/wraparound 32 (fxmod (fxdiv frame-count 8) 8)) 0 32 32)
                        (sdl-rect-set-xywh! $dst-rect (fxmod frame-count 448) 0 32 32)
                        (sdl-blit-surface $chicken-surface $src-rect src-surface $dst-rect)
                        (sdl-rect-set-xywh! $src-rect 0 0 112 176)
                        (sdl-rect-set-xywh! $dst-rect (fx- 380 (fxmod frame-count 380)) 27 112 176)
                        (sdl-blit-surface $tilemap-surface $src-rect src-surface $dst-rect)
                        (if filter-state
                          (apply-light-point-matrix-op src-bv dst-bv mat-bv)
                          (apply-direct-6x-scale src-bv dst-bv))

                        ;; Render directly to window surface
                        (let ([win-surface (sdl-get-window-surface window)])
                          (if win-surface
                              (begin
                                (sdl-blit-surface dst-surface 0 win-surface 0)
                                (sdl-update-window-surface window))
                              #f))

                        (let* ([frame-elapsed (- (sdl-get-ticks) frame-start)]
                               [delay-needed (if (< frame-elapsed 16) (- 16 frame-elapsed) 0)])
                          (if (> delay-needed 0)
                              (sdl-delay delay-needed)
                              #f))
                        (loop (fx+/wraparound frame-count 1) filter-state))))))))))))

(with-sdl-init (SDL_INIT_VIDEO)
  (with-sdl-window
    ($window
      "Mica SDL3 sandbox"
      WINDOW_WIDTH
      WINDOW_HEIGHT
      SDL_WINDOW_VISIBLE
      SDL_WINDOW_HIGH_PIXEL_DENSITY)
    (lets
      (src-bv (make-immobile-bytevector SRC_BUFFER_SIZE 0))
      (dst-bv (make-immobile-bytevector SCALED_BUFFER_SIZE 0))
      (with-sdl-surface-from
        ($src-surface
          BASE_WIDTH
          BASE_HEIGHT
          PIXEL_FORMAT
          (object->reference-address src-bv)
          (* BASE_WIDTH 4))
        (with-sdl-surface-from
          ($dst-surface
            SCALED_WIDTH
            SCALED_HEIGHT
            PIXEL_FORMAT
            (object->reference-address dst-bv)
            (* SCALED_WIDTH 4))
          (run-main-loop $window src-bv dst-bv $src-surface $dst-surface light-matrix))))))
