(library (react-impl)
  (export react-syntax)
  (import (micascheme) (sdl) (react-lib) (sequential-syntax))

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
                    (let*
                      (($index i)
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
        (ftype-set! SDL_AudioSpec (samples) $audio-spec 64)
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

          #,@(map (lambda ($syntax) #`(writeln (quote (init #,$syntax))))
            (map initializer-syntax
              (filter initializer? $statements)))
          #,@(map (lambda ($syntax) #`(writeln (quote (updater #,$syntax))))
            (map updater-syntax
              (filter updater? $statements)))
          #,@(map (lambda ($syntax) #`(writeln (quote (sampler #,$syntax))))
            (map sampler-syntax
              (filter sampler? $statements)))
          #,@(map (lambda ($syntax) #`(writeln (quote (audio #,$syntax))))
            (map stream-syntax
              (filter stream? $statements)))

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
          (built-bind (built-updater-expression $lookup #`$value)
            (lambda ($value)
              (built
                (push $statements
                  (updater #`(displayln #,$value)))
                #`(void))))))
      ((audio $value)
        (lets
          ($context (empty-context))
          ($context (context-bind $context #`space? (pure-sequential #`$space?)))
          ($context (context-bind $context #`mouse-x (pure-sequential #`$mouse-x)))
          ($context (context-bind $context #`mouse-y (pure-sequential #`$mouse-y)))
          ($sequential (syntax-sequential $context #`$value))
          ($deps (sequential-deps $sequential))
          (append
            (stack (stream (sequential-value $sequential)))
            (map initializer (deps-declarations $deps))
            (map sampler (deps-updaters $deps)))))
      ((rect $x $y $w $h)
        (built-statements
          (built-bind (built-updater-expression $lookup #`$x)
            (lambda ($x)
              (built-bind (built-updater-expression $lookup #`$y)
                (lambda ($y)
                  (built-bind (built-updater-expression $lookup #`$w)
                    (lambda ($w)
                      (built-bind (built-updater-expression $lookup #`$h)
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

  (define (built-updater-expression $lookup $syntax)
    (syntax-case $syntax ()
      ($other
        (built-general-expression $lookup $syntax built-updater-expression))))

  (define (built-general-expression $lookup $syntax $recurse)
    (syntax-case $syntax (if seconds frames mouse-x mouse-y space? vector lets)
      ((lets $body)
        (built-general-expression $lookup #`$body $recurse))
      ((lets ($var $expr) $rest ...)
        (lets
          ($built-expr ($recurse $lookup #`$expr))
          ($built-rest ($recurse $lookup #`(lets $rest ...)))
          (built
            (append
              (built-statements $built-rest)
              (stack
                (initializer #`(define $var #f))
                (sampler #`(set! $var #,(built-value $built-expr))))
              (built-statements $built-expr))
            (built-value $built-rest))))
      ((if $cond $true $false)
        (built-bind ($recurse $lookup #`$cond)
          (lambda ($cond)
            (built-bind ($recurse $lookup #`$true)
              (lambda ($true)
                (built-bind ($recurse $lookup #`$false)
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
      ((vector $item ...)
        (lets
          ($vector (car (generate-temporaries `(vector))))
          (built
            (stack
              (initializer
                #`(define #,$vector (vector $item ...))))
            $vector)))
      (($item ...)
        (lets
          ($builts (map (partial $recurse $lookup) (syntax->list #`($item ...))))
          (built
            (apply append (map built-statements $builts))
            #`(#,@(map built-value $builts)))))
      ($id (identifier? #`$id)
        (switch ($lookup #`$id #`react)
          ((false? _)
            (built (stack) #`$id))
          ((else $property)
            (built (car $property) (cdr $property)))))
      ($other
        (built (stack) #`$other))))
)
