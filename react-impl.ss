(library (react-impl)
  (export react-syntax)
  (import (micascheme) (sdl) (react-lib))

  (data (initializer syntax))
  (data (updater syntax))
  (data (sampler syntax))
  (data (stream syntax))

  (define (react-syntax $syntax)
    (lets
      ($statements (build $syntax))
      #`(begin
        (sdl-set-main-ready!)
        (sdl-init SDL-INIT-VIDEO)

        (define $window
          (sdl-create-window "chezscheme"
            SDL-WINDOWPOS-UNDEFINED
            SDL-WINDOWPOS-UNDEFINED
            640
            480))

        (define $renderer
          (sdl-create-renderer $window
            -1
            SDL-RENDERER-ACCELERATED
            SDL-RENDERER-PRESENT-VSYNC))

        (define $audio-spec
          (make-ftype-pointer SDL_AudioSpec
            (foreign-alloc (ftype-sizeof SDL_AudioSpec))))

        (define $phase1 0)
        (define $phase2 0)
        (define $phase3 0)
        (define $osc 0)
        (define $frame-count 0)

        (define $callback
          (lets
            ($callable
              (foreign-callable __collect_safe
                (lambda ($userdata $buffer $len)
                  (do!
                    ((i 0 (+ i 1)))
                    ((>= i $len) (void))
                    #,@(map sampler-syntax (filter sampler? $statements))
                    (set! $phase1 (fract (+ $phase1 (* (/ $mouse-x 60) 0.0025))))
                    (set! $phase2 (fract (+ $phase2 (* (/ $mouse-x 60) 0.001253))))
                    (set! $phase3 (fract (+ $phase3 (* (/ $mouse-x 60) 0.001258))))
                    (cond
                      ($space-pressed?
                        (set! $phase1 (fract (+ $phase1 (* 1 0.0025))))
                        (set! $phase2 (fract (+ $phase2 (* 1 0.001253))))
                        (set! $phase3 (fract (+ $phase3 (* 1 0.001258)))))
                      (else (void)))
                    (set! $osc (fract (+ $osc (* (/ $mouse-y 120) 0.0001))))
                    (let*
                      (($pi2 (* (asin 1) 4))
                       ($index i)
                       ($audio-opt #,(single (map stream-syntax (filter stream? $statements))))
                       ($value
                        (inexact->exact
                          (floor
                            (-
                              (*
                                (fract (or $audio-opt (/ (+ $phase1 $phase2 $phase3) 3)))
                                255)
                              128)))))
                      (foreign-set! `integer-8 $buffer $index $value))))
                (void* void* int)
                void))
            (do (lock-object $callable))
            (foreign-callable-entry-point $callable)))

        (displayln $callback)

        (ftype-set! SDL_AudioSpec (freq) $audio-spec 22050)
        (ftype-set! SDL_AudioSpec (format) $audio-spec AUDIO-S8)
        (ftype-set! SDL_AudioSpec (channels) $audio-spec 1)
        (ftype-set! SDL_AudioSpec (samples) $audio-spec 512)
        (ftype-set! SDL_AudioSpec (callback) $audio-spec $callback)
        (ftype-set! SDL_AudioSpec (userdata) $audio-spec 0)

        (define $open-audio-result
          (SDL_OpenAudio $audio-spec (make-ftype-pointer SDL_AudioSpec 0)))

        (display "$open-audio-result: ")
        (displayln $open-audio-result)

        (foreign-free (ftype-pointer-address $audio-spec))

        (SDL_PauseAudio 0)

        (define $space-pressed? #f)

        (define $mouse-x 0)
        (define $mouse-y 0)

        (define (process-events-and-quit?)
          (sdl-poll-event)
          (cond
            ((sdl-event-none?) #f)
            ((sdl-event-quit?) #t)
            ((sdl-event-key-up? SDLK-SPACE) (set! $space-pressed? #f) #f)
            ((sdl-event-key-down? SDLK-SPACE) (set! $space-pressed? #t) #f)
            ((sdl-event-mouse-motion?)
              (set! $mouse-x (sdl-event-mouse-motion-x))
              (set! $mouse-y (sdl-event-mouse-motion-y))
              #f)
           (else (process-events-and-quit?))))

        #,@(map initializer-syntax (filter initializer? $statements))

        (define (game-loop)
          (display "\x1B;[2J")
          (display "\x1B;[0;0H")
          (displayln "Leonardo, v0.1")
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
          ;(displayln (format "Mouse: ~s ~s" $mouse-x $mouse-y))

          (sdl-set-render-draw-color! $renderer 0 0 0 255)
          (sdl-render-clear $renderer)

          (sdl-set-render-draw-color! $renderer 0 100 0 255)

          #,@(map updater-syntax (filter updater? $statements))

          (sdl-render-present $renderer)

          (set! $frame-count (+ $frame-count 1))

          (if (not (process-events-and-quit?)) (game-loop)))

        (game-loop)
        (displayln "Goodbye.")

        (SDL_PauseAudio 1)
        (SDL_CloseAudio)

        (sdl-destroy-renderer $renderer)
        (sdl-destroy-window $window)
        (sdl-quit))))

  (define (build $syntax)
    (reverse
      (fold-left
        statements+syntax
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

  (define (statements+syntax $statements $syntax)
    (syntax-case $syntax (message audio rect)
      ((message $value)
        (built-statements
          (built-bind (built-updater-expression #`$value)
            (lambda ($value)
              (built
                (push $statements
                  (updater #`(displayln #,$value)))
                #`(void))))))
      ((audio $value)
        (built-statements
          (built-bind (built-sampler-expression #`$value)
            (lambda ($value)
              (built
                (push $statements (stream $value))
                #`(void))))))
      ((rect $x $y $w $h)
        (built-statements
          (built-bind (built-updater-expression #`$x)
            (lambda ($x)
              (built-bind (built-updater-expression #`$y)
                (lambda ($y)
                  (built-bind (built-updater-expression #`$w)
                    (lambda ($w)
                      (built-bind (built-updater-expression #`$h)
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

  (define (built-sampler-expression $syntax)
    (syntax-case $syntax (osc)
      ((osc $freq)
        (built-bind (built-sampler-expression #`$freq)
          (lambda ($freq)
            (lets
              ($osc (car (generate-temporaries `(osc))))
              (built
                (stack
                  (initializer #`(define #,$osc 0.0))
                  (sampler #`(set! #,$osc (fract (+ #,$osc (/ #,$freq 22050.0))))))
                $osc)))))
      ($other
        (built-general-expression $syntax built-sampler-expression))))

  (define (built-updater-expression $syntax)
    (syntax-case $syntax ()
      ($other
        (built-general-expression $syntax built-updater-expression))))

  (define (built-general-expression $syntax $recurse)
    (syntax-case $syntax (if seconds frames mouse-x mouse-y space-pressed?)
      ((if $cond $true $false)
        (built-bind ($recurse #`$cond)
          (lambda ($cond)
            (built-bind ($recurse #`$true)
              (lambda ($true)
                (built-bind ($recurse #`$false)
                  (lambda ($false)
                    (built
                      (stack)
                      #`(if #,$cond #,$true #,$false)))))))))
      (seconds
        (built (stack) #`(seconds)))
      (frames
        (built (stack) #`$frame-count))
      (mouse-x
        (built (stack) #`$mouse-x))
      (mouse-y
        (built (stack) #`$mouse-y))
      (space-pressed?
        (built (stack) #`$space-pressed?))
      (($item ...)
        (lets
          ($builts (map $recurse (syntax->list #`($item ...))))
          (built
            (apply append (map built-statements $builts))
            #`(#,@(map built-value $builts)))))
      ($other
        (built (stack) #`$other))))
)
