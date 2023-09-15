(library (react-impl)
  (export react-syntax)
  (import (micascheme) (sdl) (react-lib))

  (data (initializer syntax))
  (data (updater syntax))

  (define (react-syntax $syntax)
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

      (define $callback
        (lets
          ($callable
            (foreign-callable __collect_safe
              (lambda ($userdata $buffer $len)
                (do!
                  ((i 0 (+ i 1)))
                  ((>= i $len) (void))
                  (set! $phase1 (fract (+ $phase1 (* 2 0.0025))))
                  (set! $phase2 (fract (+ $phase2 (* 2 0.001253))))
                  (set! $phase3 (fract (+ $phase3 (* 2 0.001258))))
                  (cond
                    ($space-pressed
                      (set! $phase1 (fract (+ $phase1 (* 1 0.0025))))
                      (set! $phase2 (fract (+ $phase2 (* 1 0.001253))))
                      (set! $phase3 (fract (+ $phase3 (* 1 0.001258)))))
                    (else (void)))
                  (set! $osc (fract (+ $osc 0.0001)))
                  (let*
                    (($pi2 (* (asin 1) 4))
                     ($index i)
                     ($value
                      (inexact->exact
                        (round
                          (* (sin (* $pi2 $osc)) 127 0.33
                            (+
                              $phase1
                              $phase2
                              $phase3))))))
                    (foreign-set! `integer-8 $buffer $index $value)
                    )))
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

      (define $space-pressed #f)

      (define (process-events-and-quit?)
        (sdl-poll-event)
        (cond
         ((sdl-event-none?) #f)
         ((sdl-event-quit?) #t)
         ((sdl-event-key-up? SDLK-SPACE) (set! $space-pressed #f) #f)
         ((sdl-event-key-down? SDLK-SPACE) (set! $space-pressed #t) #f)
         (else (process-events-and-quit?))))

      (define (game-loop)
        (display "\x1B;[2J")
        (display "\x1B;[0;0H")
        (displayln "Leonardo, v0.1")

        #,@(build $syntax)

        (sdl-set-render-draw-color! $renderer 0 0 0 255)
        (sdl-render-clear $renderer)

        (sdl-set-render-draw-color! $renderer 0 100 0 255)
        (sdl-render-fill-rects $renderer
          (list
            (make-sdl-rect   0   0 50 50)
            (make-sdl-rect 100   0 50 50)
            (make-sdl-rect   0 100 50 50)))

        (sdl-render-present $renderer)

        (if (not (process-events-and-quit?)) (game-loop)))

      (game-loop)
      (displayln "Goodbye.")

      (SDL_PauseAudio 1)
      (SDL_CloseAudio)

      (sdl-destroy-renderer $renderer)
      (sdl-destroy-window $window)
      (sdl-quit)))

  (define (build $syntax)
    (reverse
      (fold-left
        builder+syntax
        (stack)
        (syntax->list $syntax))))

  (define (builder+syntax $builder $syntax)
    (syntax-case $syntax (message)
      ((message $message)
        (push $builder
          #`(lets
            ($time (current-time `time-monotonic))
            (displayln
              #,(syntax-case #`$message (time)
                (time
                  #`(string-append
                    "Time: "
                    (number->string
                      (+
                        (* (time-second $time) 1000)
                        (div (time-nanosecond $time) 1000000)))
                    "ms"))
                ($other #`(quote $other)))))))
      ($other
        (syntax-error $syntax))))
)
