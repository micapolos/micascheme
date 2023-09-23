(library (mica-sdl)
  (export
    run-sdl
    run-sdl-window
    run-sdl-renderer
    run-sdl-audio
    run-sdl-event-loop)

  (import (micascheme) (sdl))

  (export (import (sdl)))

  (define (sdl-error)
    (error `sdl (sdl-get-error)))

  (define-syntax-rule (run-sdl $flag $flags ... ($body ...))
    (case (sdl-init $flag $flags ...)
      ((0)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-quit))))
      (else (sdl-error))))

  (define-syntax-rule (run-sdl-window $title $x $y $w $h $flag ... ($window $body ...))
    (switch (sdl-create-window $title $x $y $w $h $flag ...)
      ((zero? _) (sdl-error))
      ((else $window)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-window $window))))))

  (define-syntax-rule (run-sdl-renderer $window $index $flag ... ($renderer $body ...))
    (switch (sdl-create-renderer $window $index $flag ...)
      ((zero? _) (sdl-error))
      ((else $renderer)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-renderer $renderer))))))

  (define-syntax-rule (run-sdl-audio $freq $format $channels $samples ($bytevector $callback ...) ($body ...))
    (begin
      (define $callable
        (foreign-callable __collect_safe
          (lambda ($userdata $buffer $len)
            (lets
              ($tmp-bytevector (make-bytevector $len))
              (do ((lambda ($bytevector) $callback ...) $tmp-bytevector))
              (do!
                (($index 0 (+ $index 1)))
                ((>= $index $len) (void))
                (foreign-set! `unsigned-8 $buffer $index
                  (bytevector-u8-ref $tmp-bytevector $index)))))
          (void* void* int)
          void))
      (lock-object $callable)
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (define $audio-spec
            (make-ftype-pointer SDL_AudioSpec
              (foreign-alloc (ftype-sizeof SDL_AudioSpec))))
          (ftype-set! SDL_AudioSpec (freq) $audio-spec $freq)
          (ftype-set! SDL_AudioSpec (format) $audio-spec $format)
          (ftype-set! SDL_AudioSpec (channels) $audio-spec $channels)
          (ftype-set! SDL_AudioSpec (samples) $audio-spec $samples)
          (ftype-set! SDL_AudioSpec (callback) $audio-spec (foreign-callable-entry-point $callable))
          (ftype-set! SDL_AudioSpec (userdata) $audio-spec 0)

          (switch (SDL_OpenAudio $audio-spec (make-ftype-pointer SDL_AudioSpec 0))
            ((zero? _)
              (dynamic-wind
                (lambda () #f)
                (lambda () $body ...)
                (lambda () (SDL_CloseAudio))))
            ((else _) (sdl-error))))
        (lambda () (unlock-object $callable)))))

  (define-syntax-rule (run-sdl-event-loop $body ...)
    (do!
      (($event (sdl-poll-event) (sdl-poll-event)))
      ((sdl-event-quit?) (void))
      $body ...))
)
