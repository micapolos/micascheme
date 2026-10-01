(import
  (scheme)
  (lets)
  (sdl3)
  (syntax)
  (foreign)
  (system)
  (procedure)
  (sdl3-image)
  (mica-sdl3)
  (vstack)
  (tata color)
  (tata blit))

(define PIXEL_FORMAT SDL_PIXELFORMAT_ARGB8888)

(define BASE_WIDTH 480)
(define BASE_HEIGHT 256)
(define MATRIX_SIZE 6)

(define SCALED_WIDTH (fx*/wraparound BASE_WIDTH MATRIX_SIZE))
(define SCALED_HEIGHT (fx*/wraparound BASE_HEIGHT MATRIX_SIZE))

(define RETINA_SCALE 2)
(define WINDOW_WIDTH (fxquotient SCALED_WIDTH RETINA_SCALE))
(define WINDOW_HEIGHT (fxquotient SCALED_HEIGHT RETINA_SCALE))

(define SRC_BUFFER_SIZE (fx*/wraparound BASE_WIDTH (fx*/wraparound BASE_HEIGHT 4)))
(define SCALED_BUFFER_SIZE (fx*/wraparound SCALED_WIDTH (fx*/wraparound SCALED_HEIGHT 4)))

(define FRAME_INTERVAL_NS 16666667)

(define matrix-light-point
  (bytevector
    64 90 102 102 90 64
    90 147 173 173 147 90
    102 173 198 198 173 102
    102 173 198 198 173 102
    90 147 173 173 147 90
    64 90 102 102 90 64))

(define matrix-scan-point
  (bytevector
    64  90 102 102  90 64
    90 147 173 173 147 90
    102 173 198 198 173 102
    102 173 198 198 173 102
    40 70 90 90 70 40
    20 35 45 45 35 20))

(define matrix-scanlines
  (bytevector
    100 100 100 100 100 100
    160 160 160 160 160 160
    200 200 200 200 200 200
    150 150 150 150 150 150
     80  80  80  80  80  80
     40  40  40  40  40  40))

(define matrix-trinitron
  (bytevector
    110 160 210 210 160 110
    100 150 200 200 150 100
    90 130 180 180 130 90
    70 100 140 140 100 70
    80 120 160 160 120 80
    90 130 180 180 130 90))

(define matrix-shadow-mask
  (bytevector
    80 140 80 80 140 80
    140 210 140 140 210 140
    80 140 80 80 140 80
    70 120 70 70 120 70
    120 190 120 120 190 120
    70 120 70 70 120 70))

(define matrix-lcd-grid
  (bytevector
    200 200 200 200 200 100
    200 200 200 200 200 100
    200 200 200 200 200 100
    200 200 200 200 200 100
    200 200 200 200 200 100
    100 100 100 100 100 50))

(define (blit-garbage $surface $frame-count)
  (lets
    (width (sdl-surface-width $surface))
    (height (sdl-surface-height $surface))
    (src-ptr (sdl-surface-pixels $surface))
    (let loop-y ((y 0) (src-offset 0))
      (if (fx< y height)
          (begin
            (let loop-x ((x 0) (curr-offset src-offset))
              (if (fx< x width)
                  (let* ((t $frame-count)
                         (b (fxlogand (fxlogxor x (fxlogxor y t)) #xFF))
                         (g (fxlogand (fx+/wraparound (fx*/wraparound x 3) (fx+/wraparound (fx*/wraparound y 2) t)) #xFF))
                         (r (fxlogand (fxlogxor (fx*/wraparound x y) (fx*/wraparound t 5)) #xFF))
                         (a 255)
                         (pixel (rgba-color (fxsrl r 2) (fxsrl g 2) (fxsrl b 2) a)))
                    (foreign-set-u32! src-ptr curr-offset pixel)
                    (loop-x (fx+/wraparound x 1)
                            (fx+/wraparound curr-offset 4)))
                  #f))
            (loop-y (fx+/wraparound y 1)
                    (fx+/wraparound src-offset (fx*/wraparound width 4))))
          #f))))

(define (apply-light-point-matrix-op $src-surface $dst-surface $mat-bv)
  (lets
    ($src-width (sdl-surface-width $src-surface))
    ($src-height (sdl-surface-height $src-surface))
    ($src-pixels (sdl-surface-pixels $src-surface))
    ($dst-width (sdl-surface-width $dst-surface))
    ($dst-height (sdl-surface-height $dst-surface))
    ($dst-pixels (sdl-surface-pixels $dst-surface))
    (scaled-stride (fx*/wraparound $dst-width 4))
    (let loop-y ((y 0) (src-offset 0) (dst-row-base 0))
      (and (fx< y $src-height)
        (begin
          (let loop-x ((x 0) (curr-src src-offset) (dst-pixel-base dst-row-base))
            (and (fx< x $src-width)
              (let-values
                (((r g b a) (color-rgba (foreign-u32 $src-pixels curr-src))))
                (lets
                  (row0 dst-pixel-base)
                  (row1 (fx+/wraparound row0 scaled-stride))
                  (row2 (fx+/wraparound row1 scaled-stride))
                  (row3 (fx+/wraparound row2 scaled-stride))
                  (row4 (fx+/wraparound row3 scaled-stride))
                  (row5 (fx+/wraparound row4 scaled-stride))

                  (begin
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

                      (foreign-set-u32! $dst-pixels row0 (make-px (bytevector-u8-ref $mat-bv 0)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row0 4) (make-px (bytevector-u8-ref $mat-bv 1)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row0 8) (make-px (bytevector-u8-ref $mat-bv 2)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row0 12) (make-px (bytevector-u8-ref $mat-bv 3)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row0 16) (make-px (bytevector-u8-ref $mat-bv 4)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row0 20) (make-px (bytevector-u8-ref $mat-bv 5)))

                      (foreign-set-u32! $dst-pixels row1 (make-px (bytevector-u8-ref $mat-bv 6)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row1 4) (make-px (bytevector-u8-ref $mat-bv 7)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row1 8) (make-px (bytevector-u8-ref $mat-bv 8)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row1 12) (make-px (bytevector-u8-ref $mat-bv 9)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row1 16) (make-px (bytevector-u8-ref $mat-bv 10)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row1 20) (make-px (bytevector-u8-ref $mat-bv 11)))

                      (foreign-set-u32! $dst-pixels row2 (make-px (bytevector-u8-ref $mat-bv 12)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row2 4) (make-px (bytevector-u8-ref $mat-bv 13)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row2 8) (make-px (bytevector-u8-ref $mat-bv 14)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row2 12) (make-px (bytevector-u8-ref $mat-bv 15)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row2 16) (make-px (bytevector-u8-ref $mat-bv 16)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row2 20) (make-px (bytevector-u8-ref $mat-bv 17)))

                      (foreign-set-u32! $dst-pixels row3 (make-px (bytevector-u8-ref $mat-bv 18)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row3 4) (make-px (bytevector-u8-ref $mat-bv 19)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row3 8) (make-px (bytevector-u8-ref $mat-bv 20)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row3 12) (make-px (bytevector-u8-ref $mat-bv 21)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row3 16) (make-px (bytevector-u8-ref $mat-bv 22)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row3 20) (make-px (bytevector-u8-ref $mat-bv 23)))

                      (foreign-set-u32! $dst-pixels row4 (make-px (bytevector-u8-ref $mat-bv 24)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row4 4) (make-px (bytevector-u8-ref $mat-bv 25)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row4 8) (make-px (bytevector-u8-ref $mat-bv 26)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row4 12) (make-px (bytevector-u8-ref $mat-bv 27)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row4 16) (make-px (bytevector-u8-ref $mat-bv 28)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row4 20) (make-px (bytevector-u8-ref $mat-bv 29)))

                      (foreign-set-u32! $dst-pixels row5 (make-px (bytevector-u8-ref $mat-bv 30)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row5 4) (make-px (bytevector-u8-ref $mat-bv 31)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row5 8) (make-px (bytevector-u8-ref $mat-bv 32)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row5 12) (make-px (bytevector-u8-ref $mat-bv 33)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row5 16) (make-px (bytevector-u8-ref $mat-bv 34)))
                      (foreign-set-u32! $dst-pixels (fx+/wraparound row5 20) (make-px (bytevector-u8-ref $mat-bv 35))))

                    (loop-x
                      (fx+/wraparound x 1)
                      (fx+/wraparound curr-src 4)
                      (fx+/wraparound dst-pixel-base 24)))))))
          (loop-y
            (fx+/wraparound y 1)
            (fx+/wraparound src-offset (fx*/wraparound $src-width 4))
            (fx+/wraparound dst-row-base (fx*/wraparound scaled-stride 6))))))))

(define (drain-events $event $keep-running? $filter-state)
  (if (sdl-poll-event $event)
    (lets
      ($type (sdl-event-type $event))
      (cond
        ((fx= $type SDL_EVENT_QUIT)
          (drain-events $event #f $filter-state))
        ((fx= $type SDL_EVENT_KEY_DOWN)
          (lets
            ($key (sdl-event-key $event))
            (if (and (fx= $key SDLK_SPACE))
              (drain-events $event $keep-running? (not $filter-state))
              (drain-events $event $keep-running? $filter-state))))
        (else
          (drain-events $event $keep-running? $filter-state))))
    (values $keep-running? $filter-state)))

(define run-main-loop
  (lambda ($window $src-surface $mat-bv)
    (with-vstack (sp 1024)
      (with-sdl-png-surface ($chicken-surface "/Users/micapolos/git/Tata8/res/micapolos/depressedChicken.png")
        (with-sdl-png-surface ($tilemap-surface "/Users/micapolos/git/Tata8/res/micapolos/tilemap.png")
          (vstack-let sp
            ($event (ftype-sizeof SDL_Event))
            ($src-rect (ftype-sizeof SDL_Rect))
            ($dst-rect (ftype-sizeof SDL_Rect))
            (let loop ([$frame-count 0]
                       [filter-enabled? #t]
                       [$next-frame (+ (sdl-get-ticks-ns) FRAME_INTERVAL_NS)])
              (let-values ([(keep-running? filter-state) (drain-events $event #t filter-enabled?)])
                (if (not keep-running?)
                  #f
                  (begin
                    (blit-garbage $src-surface $frame-count)

                    (sdl-rect-set-xywh! $src-rect (fx*/wraparound 32 (fxmod (fxdiv $frame-count 8) 8)) 0 32 32)
                    (sdl-rect-set-xywh! $dst-rect (fxmod $frame-count 448) 0 32 32)
                    (sdl-blit-surface $chicken-surface $src-rect $src-surface $dst-rect)

                    (sdl-rect-set-xywh! $src-rect 0 0 112 176)
                    (sdl-rect-set-xywh! $dst-rect (- 112 (fxmod $frame-count 112)) 27 480 176)
                    (sdl-blit-surface-tiled $tilemap-surface $src-rect $src-surface $dst-rect)

                    (blit-pattern-line
                      #x0103070f
                      32
                      (sdl-surface-pixels $src-surface)
                      (sdl-surface-pitch $src-surface)
                      (rgba-color 255 0 255 255))

                    (vstack-let sp
                      ($pattern 32)
                      (begin
                        (foreign-set-u32! $pattern 0 #x0103070f)
                        (foreign-set-u32! $pattern 4 #x0203070f)
                        (foreign-set-u32! $pattern 8 #x0403070f)
                        (foreign-set-u32! $pattern 12 #x0803070f)
                        (foreign-set-u32! $pattern 16 #x0103070f)
                        (foreign-set-u32! $pattern 20 #x0203070f)
                        (foreign-set-u32! $pattern 24 #x0403070f)
                        (foreign-set-u32! $pattern 28 #x0803070f)
                        (blit-pattern
                          $pattern
                          4
                          8
                          28
                          (fx+/wraparound (sdl-surface-pixels $src-surface) 8)
                          (sdl-surface-pitch $src-surface)
                          (rgba-color 255 0 0 255))))

                    (with-sdl-window-surface ($win-surface $window)
                      (if filter-state
                        (apply-light-point-matrix-op
                          $src-surface
                          $win-surface
                          $mat-bv)
                        (begin
                          (sdl-rect-set-xywh! $src-rect 0 0 BASE_WIDTH BASE_HEIGHT)
                          (sdl-rect-set-xywh! $dst-rect 0 0 SCALED_WIDTH SCALED_HEIGHT)
                          (sdl-blit-surface-scaled $src-surface $src-rect $win-surface $dst-rect SDL_SCALEMODE_NEAREST)))
                      (sdl-update-window-surface $window))

                    (lets
                      ($now (sdl-get-ticks-ns))
                      ($target-frame
                        (if (> $now $next-frame)
                          (+ $now FRAME_INTERVAL_NS)
                          $next-frame))
                      (begin
                        (when (< $now $target-frame)
                          (sdl-delay-ns (- $target-frame $now)))
                        (loop
                          (fx+/wraparound $frame-count 1)
                          filter-state
                          (+ $target-frame FRAME_INTERVAL_NS))))))))))))))

(with-sdl-init (SDL_INIT_VIDEO)
  (with-sdl-window
    ($window
      "Mica SDL3 sandbox"
      WINDOW_WIDTH
      WINDOW_HEIGHT
      SDL_WINDOW_VISIBLE
      SDL_WINDOW_HIGH_PIXEL_DENSITY)
    (with-sdl-surface
      ($src-surface BASE_WIDTH BASE_HEIGHT PIXEL_FORMAT)
      (sdl-set-surface-blend-mode $src-surface SDL_BLENDMODE_NONE)
      (run-main-loop $window $src-surface matrix-scanlines))))
