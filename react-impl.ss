(library (react-impl)
  (export react-syntax)
  (import (micascheme) (mica-sdl) (react-lib) (sequential-syntax))

  (data (initializer syntax))
  (data (updater syntax))
  (data (sampler syntax))
  (data (stream syntax))

  (define-aux-keyword react)

  (define (react-syntax $lookup $syntax)
    (lets
      ($statements (build $lookup $syntax))
      #`(begin
        (sdl-set-main-ready!)
        (run-sdl SDL-INIT-VIDEO SDL-INIT-EVENTS SDL-INIT-AUDIO
          (
            (define $sample-freq 22050)
            (define $window-width 640)
            (define $window-height 480)

            (run-sdl-window
              "Leonardo"
              SDL-WINDOWPOS-UNDEFINED
              SDL-WINDOWPOS-UNDEFINED
              $window-width
              $window-height
              ($window
                ;(SDL_SetWindowFullscreen $window SDL-WINDOW-FULLSCREEN-DESKTOP)

                (run-sdl-renderer
                  $window
                  -1
                  SDL-RENDERER-ACCELERATED
                  ($renderer
                    (define $noise-vector
                      (lets
                        ($vector (make-vector 4096))
                        (do!
                          ((i 0 (+ i 1)))
                          ((= i (vector-length $vector)) (void))
                          (vector-set! $vector i (random 1.0)))
                        $vector))

                    (define $space-pressed? #f)
                    (define $mouse-x 0)
                    (define $mouse-y 0)
                    (define $canvas-width $window-width)
                    (define $canvas-height $window-height)
                    (define $frame-count 0)
                    (define $seconds 0)

                    (define $audio-space-pressed? $space-pressed?)
                    (define $audio-mouse-x $mouse-x)
                    (define $audio-mouse-y $mouse-y)
                    (define $audio-canvas-width $canvas-width)
                    (define $audio-canvas-height $canvas-height)
                    (define $audio-frame-count $frame-count)
                    (define $audio-seconds $seconds)

                    (define $quit? #f)

                    #,@(map initializer-syntax (filter initializer? $statements))

                    (run-sdl-audio-device
                      22050
                      AUDIO-S8
                      1
                      64
                      ($bytevector
                        (do!
                          ((i 0 (+ i 1)))
                          ((>= i (bytevector-length $bytevector)) (void))
                          #,@(map sampler-syntax (filter sampler? $statements))
                          (let*
                            (($index i)
                             ($audio-opt #,(single (map stream-syntax (filter stream? $statements))))
                             ($value
                              (inexact->exact
                                (floor
                                  (-
                                    (* (min 1 (max 0 (or $audio-opt 0.5))) 255)
                                    128)))))
                            (bytevector-s8-set! $bytevector $index $value))))
                      ($audio-device
                        (sdl-pause-audio-device $audio-device #f)

                        (run-sdl-event-loop
                          (cond
                            ((sdl-event-key-up? SDLK-SPACE)
                             (set! $space-pressed? #f))
                            ((sdl-event-key-down? SDLK-SPACE)
                              (set! $space-pressed? #t))
                            ((sdl-event-mouse-motion?)
                              (set! $mouse-x (sdl-event-mouse-motion-x))
                              (set! $mouse-y (sdl-event-mouse-motion-y)))
                            ((sdl-event-none?)
                              (set! $canvas-width (car (sdl-get-renderer-output-size $renderer)))
                              (set! $canvas-height (cadr (sdl-get-renderer-output-size $renderer)))
                              (set! $frame-count (+ $frame-count 1))
                              (set! $seconds (current-seconds))

                              (run-sdl-locked-audio-device $audio-device
                                (
                                  (set! $audio-space-pressed? $space-pressed?)
                                  (set! $audio-mouse-x $mouse-x)
                                  (set! $audio-mouse-y $mouse-y)
                                  (set! $audio-canvas-width $canvas-width)
                                  (set! $audio-canvas-height $canvas-height)
                                  (set! $audio-frame-count $frame-count)
                                  (set! $audio-seconds $seconds)))

                              ; (display "\x1B;[2J")
                              ; (display "\x1B;[0;0H")
                              ; #,@(map (lambda ($syntax) #`(writeln (quote (init #,$syntax))))
                              ;   (map initializer-syntax
                              ;     (filter initializer? $statements)))
                              ; #,@(map (lambda ($syntax) #`(writeln (quote (updater #,$syntax))))
                              ;   (map updater-syntax
                              ;     (filter updater? $statements)))
                              ; #,@(map (lambda ($syntax) #`(writeln (quote (sampler #,$syntax))))
                              ;   (map sampler-syntax
                              ;     (filter sampler? $statements)))
                              ; #,@(map (lambda ($syntax) #`(writeln (quote (audio #,$syntax))))
                              ;   (map stream-syntax
                              ;     (filter stream? $statements)))

                              (sdl-set-render-draw-color! $renderer 0 0 0 255)
                              (sdl-render-clear $renderer)

                              (sdl-set-render-draw-color! $renderer 255 255 255 255)

                              #,@(map updater-syntax (filter updater? $statements))

                              (sdl-render-present $renderer))))

                        (sdl-pause-audio-device $audio-device #t))))))))))))

  (define (build $lookup $syntax)
    (reverse
      (fold-left
        (partial statements+syntax $lookup)
        (stack)
        (syntax->list $syntax))))

  (data (built statements value))

  (define (built-bind $built $fn)
    (lets
      ($fn-built ($fn (built-value $built)))
      (built
        (push-list
          (built-statements $built)
          (reverse (built-statements $fn-built)))
        (built-value $fn-built))))

  (define (statements+syntax $lookup $statements $syntax)
    (syntax-case $syntax (message audio audio2 rect make define)
      ((message $value)
        (built-statements
          (built-bind (built-main-expression $lookup #`$value)
            (lambda ($value)
              (built
                (push $statements
                  (updater #`(displayln #,$value)))
                #`(void))))))
      ((audio $value)
        (lets
          ($built-audio (built-audio-expression $lookup #`$value))
          (push
            (push-all $statements (built-statements $built-audio))
            (stream (built-value $built-audio)))))
      ((rect $x $y $w $h)
        (built-statements
          (built-bind (built-main-expression $lookup #`$x)
            (lambda ($x)
              (built-bind (built-main-expression $lookup #`$y)
                (lambda ($y)
                  (built-bind (built-main-expression $lookup #`$w)
                    (lambda ($w)
                      (built-bind (built-main-expression $lookup #`$h)
                        (lambda ($h)
                          (built
                            (push $statements
                              (updater
                                #`(sdl-render-fill-rect $renderer
                                  (make-sdl-rect
                                    (inexact->exact (round #,$x))
                                    (inexact->exact (round #,$y))
                                    (inexact->exact (round #,$w))
                                    (inexact->exact (round #,$h))))))
                            #`(void))))))))))))
      ($other
        (syntax-error $syntax))))

  (define (built-audio-expression $lookup $syntax)
    (lets
      ($context (empty-context))
      ($context (context-bind $context #`sample-rate (pure-sequential #`$sample-freq)))
      ($context (context-bind $context #`space? (pure-sequential #`$audio-space-pressed?)))
      ($context (context-bind $context #`mouse-x (pure-sequential #`$audio-mouse-x)))
      ($context (context-bind $context #`mouse-y (pure-sequential #`$audio-mouse-y)))
      ($context (context-bind $context #`canvas-width (pure-sequential #`$audio-canvas-width)))
      ($context (context-bind $context #`canvas-height (pure-sequential #`$audio-canvas-height)))
      ($context (context-bind $context #`frames (pure-sequential #`$audio-frame-count)))
      ($context (context-bind $context #`seconds (pure-sequential #`$audio-seconds)))
      ($sequential (syntax-sequential $context $syntax))
      ($deps (sequential-deps $sequential))
      (built
        (append
          (map initializer (deps-declarations $deps))
          (map sampler (deps-updaters $deps)))
        (sequential-value $sequential))))

  (define (built-main-expression $lookup $syntax)
    (lets
      ($context (empty-context))
      ($context (context-bind $context #`sample-rate (pure-sequential #`$sample-freq)))
      ($context (context-bind $context #`space? (pure-sequential #`$space-pressed?)))
      ($context (context-bind $context #`mouse-x (pure-sequential #`$mouse-x)))
      ($context (context-bind $context #`mouse-y (pure-sequential #`$mouse-y)))
      ($context (context-bind $context #`canvas-width (pure-sequential #`$canvas-width)))
      ($context (context-bind $context #`canvas-height (pure-sequential #`$canvas-height)))
      ($context (context-bind $context #`frames (pure-sequential #`$frame-count)))
      ($context (context-bind $context #`seconds (pure-sequential #`$seconds)))
      ($sequential (syntax-sequential $context $syntax))
      ($deps (sequential-deps $sequential))
      (built
        (append
          (map initializer (deps-declarations $deps))
          (map updater (deps-updaters $deps)))
        (sequential-value $sequential))))
)
