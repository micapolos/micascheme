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

        (define $sample-freq 22050)

        (define $window
          (sdl-create-window "Leonardo"
            SDL-WINDOWPOS-UNDEFINED
            SDL-WINDOWPOS-UNDEFINED
            640
            480))

        ;(SDL_SetWindowFullscreen $window SDL-WINDOW-FULLSCREEN-DESKTOP)

        (define $renderer
          (sdl-create-renderer $window
            -1
            SDL-RENDERER-ACCELERATED
            SDL-RENDERER-PRESENT-VSYNC))

        (define $noise-vector (make-vector 4096))
        (do!
          ((i 0 (+ i 1)))
          ((= i (vector-length $noise-vector)) (void))
          (vector-set! $noise-vector i (random 1.0)))
        (define $noise-index 0)

        (define $audio-spec
          (make-ftype-pointer SDL_AudioSpec
            (foreign-alloc (ftype-sizeof SDL_AudioSpec))))

        (define $mutex (make-mutex))

        (define $shared-space? #f)
        (define $shared-mouse-x 0)
        (define $shared-mouse-y 0)
        (define $shared-frame-count 0)
        (define $shared-seconds 0)

        (define $space? #f)
        (define $mouse-x #f)
        (define $mouse-y #f)
        (define $frame-count #f)
        (define $seconds #f)
        (define $quit? #f)

        (define $callback
          (lets
            ($callable
              (foreign-callable __collect_safe
                (lambda ($userdata $buffer $len)
                  (define $space? #f)
                  (define $mouse-x #f)
                  (define $mouse-y #f)
                  (define $frame-count #f)
                  (define $seconds #f)

                  (with-mutex $mutex
                    (set! $space? $shared-space?)
                    (set! $mouse-x $shared-mouse-x)
                    (set! $mouse-y $shared-mouse-y)
                    (set! $frame-count $shared-frame-count)
                    (set! $seconds $shared-seconds))

                  (do!
                    ((i 0 (+ i 1)))
                    ((>= i $len) (void))
                    #,@(map sampler-syntax (filter sampler? $statements))
                    (set! $noise-index (+ $noise-index 1))
                    (if (= $noise-index (vector-length $noise-vector))
                      (set! $noise-index 0))
                    (let*
                      (($index i)
                       ($noise (vector-ref $noise-vector $noise-index))
                       ($audio-opt #,(single (map stream-syntax (filter stream? $statements))))
                       ($value
                        (inexact->exact
                          (floor
                            (-
                              (* (min 1 (max 0 (or $audio-opt 0.5))) 255)
                              128)))))
                      (foreign-set! `integer-8 $buffer $index $value))))
                (void* void* int)
                void))
            (do (lock-object $callable))
            (foreign-callable-entry-point $callable)))

        (ftype-set! SDL_AudioSpec (freq) $audio-spec $sample-freq)
        (ftype-set! SDL_AudioSpec (format) $audio-spec AUDIO-S8)
        (ftype-set! SDL_AudioSpec (channels) $audio-spec 1)
        (ftype-set! SDL_AudioSpec (samples) $audio-spec 256)
        (ftype-set! SDL_AudioSpec (callback) $audio-spec $callback)
        (ftype-set! SDL_AudioSpec (userdata) $audio-spec 0)

        (define $open-audio-result
          (SDL_OpenAudio $audio-spec (make-ftype-pointer SDL_AudioSpec 0)))

        (foreign-free (ftype-pointer-address $audio-spec))

        (SDL_PauseAudio 0)

        (define (process-events-and-quit?)
          (sdl-poll-event)
          (cond
            ((sdl-event-none?) #f)
            ((sdl-event-quit?) #t)
            ((sdl-event-key-up? SDLK-SPACE) (set! $shared-space? #f) #f)
            ((sdl-event-key-down? SDLK-SPACE) (set! $shared-space? #t) #f)
            ((sdl-event-mouse-motion?)
              (set! $shared-mouse-x (sdl-event-mouse-motion-x))
              (set! $shared-mouse-y (sdl-event-mouse-motion-y))
              #f)
           (else (process-events-and-quit?))))

        #,@(map initializer-syntax (filter initializer? $statements))

        (define (game-loop)
          (define $space? #f)
          (define $mouse-x #f)
          (define $mouse-y #f)
          (define $frame-count #f)
          (define $seconds #f)

          (display "\x1B;[2J")
          (display "\x1B;[0;0H")

          (set! $quit?
            (with-mutex $mutex
              (set! $shared-frame-count (+ $shared-frame-count 1))
              (set! $shared-seconds (seconds))

              (set! $space? $shared-space?)
              (set! $mouse-x $shared-mouse-x)
              (set! $mouse-y $shared-mouse-y)
              (set! $frame-count $shared-frame-count)
              (set! $seconds $shared-seconds)

              (process-events-and-quit?)))

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

          (sdl-render-present $renderer)

          (if (not $quit?) (game-loop)))

        (game-loop)

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
    (syntax-case $syntax (message audio rect make)
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
    (syntax-case $syntax (osc noise)
      ((osc $freq)
        (built-bind (built-sampler-expression #`$freq)
          (lambda ($freq)
            (lets
              ($osc (car (generate-temporaries `(osc))))
              (built
                (stack
                  (initializer #`(define #,$osc 0.0))
                  (sampler #`(set! #,$osc (fract (+ #,$osc (/ #,$freq $sample-freq))))))
                $osc)))))
      (noise
        (lets
          ($noise (car (generate-temporaries `(noise))))
          (built (stack) #`$noise)))
      ($other
        (built-general-expression $syntax built-sampler-expression))))

  (define (built-updater-expression $syntax)
    (syntax-case $syntax ()
      ($other
        (built-general-expression $syntax built-updater-expression))))

  (define (built-general-expression $syntax $recurse)
    (syntax-case $syntax (if seconds frames mouse-x mouse-y space?)
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
        (built (stack) #`$seconds))
      (frames
        (built (stack) #`$frame-count))
      (mouse-x
        (built (stack) #`$mouse-x))
      (mouse-y
        (built (stack) #`$mouse-y))
      (space?
        (built (stack) #`$space?))
      (($item ...)
        (lets
          ($builts (map $recurse (syntax->list #`($item ...))))
          (built
            (apply append (map built-statements $builts))
            #`(#,@(map built-value $builts)))))
      ($other
        (built (stack) #`$other))))
)
