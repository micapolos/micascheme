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

      (define (process-events-and-quit?)
        (sdl-poll-event)
        (cond
         ((sdl-event-none?) #f)
         ((sdl-event-quit?) #t)
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
