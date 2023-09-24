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
                             (set! $space-pressed? #f)
                             #f)
                            ((sdl-event-key-down? SDLK-SPACE)
                              (set! $space-pressed? #t)
                              #f)
                            ((sdl-event-mouse-motion?)
                              (set! $mouse-x (sdl-event-mouse-motion-x))
                              (set! $mouse-y (sdl-event-mouse-motion-y))
                              #f)
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
          (built-bind (built-updater-expression $lookup #`$value)
            (lambda ($value)
              (built
                (push $statements
                  (updater #`(displayln #,$value)))
                #`(void))))))
      ((audio $value)
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
          ($sequential (syntax-sequential $context #`$value))
          ($deps (sequential-deps $sequential))
          (append
            (stack (stream (sequential-value $sequential)))
            (map initializer (deps-declarations $deps))
            (map sampler (deps-updaters $deps))
            $statements)))
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
      (canvas-width
        (built (stack) #`$canvas-width))
      (canvas-height
        (built (stack) #`$canvas-height))
      (space?
        (built (stack) #`$space-pressed?))
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
