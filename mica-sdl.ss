(library (mica-sdl)
  (export
    run-sdl
    run-sdl-window
    run-sdl-renderer
    run-sdl-rgb-surface-with-format
    run-sdl-texture
    run-sdl-texture-from-surface
    run-sdl-event-loop
    run-sdl-audio-device
    run-sdl-locked-audio-device
    sdl-surface-pixels
    sdl-surface-pitch
    sdl-pause-audio-device
    sdl-queued-audio-size
    sdl-queue-audio)

  (import (micascheme) (sdl))

  (export (import (sdl)))

  (define (sdl-error)
    (error `sdl (sdl-get-error)))

  (define $audio-mutex (make-mutex))

  (define-rule-syntax (run-sdl ($flag $flags ...) $body ...)
    (case (sdl-init $flag $flags ...)
      ((0)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-quit))))
      (else (sdl-error))))

  (define-rule-syntax (run-sdl-window ($window $title $x $y $w $h $flag ...) $body ...)
    (switch (sdl-create-window $title $x $y $w $h $flag ...)
      ((zero? _) (sdl-error))
      ((else $window)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-window $window))))))

  (define-rule-syntax (run-sdl-renderer ($renderer $window $index $flag ...) $body ...)
    (switch (sdl-create-renderer $window $index $flag ...)
      ((zero? _) (sdl-error))
      ((else $renderer)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-renderer $renderer))))))

  (define-rule-syntax (run-sdl-rgb-surface-with-format ($surface $flags $width $height $bits-per-pixel $pixel-format) $body ...)
    (switch (sdl-create-rgb-surface-with-format $flags $width $height $bits-per-pixel $pixel-format)
      ((ftype-pointer-null? _) (sdl-error))
      ((else $surface)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-free-surface $surface))))))

  (define-rule-syntax (run-sdl-texture ($texture $renderer $format $access $width $height) $body ...)
    (switch (sdl-create-texture $renderer $format $access $width $height)
      ((zero? _) (sdl-error))
      ((else $texture)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-texture $texture))))))

  (define-rule-syntax (run-sdl-texture-from-surface ($texture $renderer $surface) $body ...)
    (switch (sdl-create-texture-from-surface $renderer $surface)
      ((zero? _) (sdl-error))
      ((else $texture)
        (dynamic-wind
          (lambda () #f)
          (lambda () $body ...)
          (lambda () (sdl-destroy-texture $texture))))))

  (define-rule-syntax (sdl-surface-pixels $surface)
    (ftype-ref SDL_Surface (pixels) $surface))

  (define-rule-syntax (sdl-surface-pitch $surface)
    (ftype-ref SDL_Surface (pitch) $surface))

  (define-rule-syntax
    (run-sdl-audio-device
      ($audio-device $freq $format $channels $samples ($bytevector $callback ...))
      $body ...)
    (begin
      (define $callable
        (foreign-callable __collect_safe
          (lambda ($userdata $buffer $len)
            ;(displayln "Audio callback...")
            (with-mutex $audio-mutex
              (let (($tmp-bytevector (make-bytevector $len)))
                (app
                  (lambda ($bytevector) $callback ...)
                  $tmp-bytevector)
                (do
                  (($index 0 (+ $index 1)))
                  ((>= $index $len) (void))
                  (foreign-set! `unsigned-8 $buffer $index
                    (bytevector-u8-ref $tmp-bytevector $index))))))
          (void* void* int)
          void))
      (define $desired-audio-spec-address (foreign-alloc (ftype-sizeof SDL_AudioSpec)))
      (define $obtained-audio-spec-address (foreign-alloc (ftype-sizeof SDL_AudioSpec)))
      (lock-object $callable)
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (define $audio-spec
            (make-ftype-pointer SDL_AudioSpec
              $desired-audio-spec-address))
          (define $obtained-audio-spec
            (make-ftype-pointer SDL_AudioSpec
              $obtained-audio-spec-address))

          (ftype-set! SDL_AudioSpec (freq) $audio-spec $freq)
          (ftype-set! SDL_AudioSpec (format) $audio-spec $format)
          (ftype-set! SDL_AudioSpec (channels) $audio-spec $channels)
          (ftype-set! SDL_AudioSpec (samples) $audio-spec $samples)
          (ftype-set! SDL_AudioSpec (callback) $audio-spec (foreign-callable-entry-point $callable))
          (ftype-set! SDL_AudioSpec (userdata) $audio-spec 0)

          (switch
            (SDL_OpenAudioDevice #f 0 $audio-spec $obtained-audio-spec 0)
            ((zero? _) (sdl-error))
            ((else $audio-device)
              (dynamic-wind
                (lambda () #f)
                (lambda ()
                  (define $buffer-size (ftype-ref SDL_AudioSpec (size) $obtained-audio-spec))
                  ;(displayln (format "Audio buffer size: ~a" $buffer-size))
                  $body ...)
                (lambda () (SDL_CloseAudioDevice $audio-device))))))
        (lambda ()
          (foreign-free $obtained-audio-spec-address)
          (foreign-free $desired-audio-spec-address)
          (unlock-object $callable)))))

  (define-rule-syntax (run-sdl-event-loop $body ...)
    (do
      (($event (sdl-poll-event) (sdl-poll-event)))
      ((sdl-event-quit?) (void))
      $body ...))

  (define-rule-syntax (sdl-pause-audio-device $audio-device $pause?)
    (SDL_PauseAudioDevice $audio-device (if $pause? 1 0)))

  (define-rule-syntax (sdl-queue-audio $audio-device $bytevector)
    (SDL_QueueAudio $audio-device $bytevector (bytevector-length $bytevector)))

  (define-rule-syntax (sdl-queued-audio-size $audio-device)
    (SDL_GetQueuedAudioSize $audio-device))

  ; TODO: SDL_LockAudioDevice dead-locks. Fix it!!!
  (define-rule-syntax (run-sdl-locked-audio-device $audio-device ($body ...))
    (with-mutex $audio-mutex $body ...))
    ; (dynamic-wind
    ;   (lambda ()
    ;     (displayln "Locking audio...")
    ;     (SDL_LockAudioDevice $audio-device)
    ;     (void))
    ;   (lambda () $body ...)
    ;   (lambda ()
    ;     (displayln "Unlocking audio...")
    ;     (SDL_UnlockAudioDevice $audio-device)
    ;     (void))))
)
