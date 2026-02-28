(library (micac lib emu)
  (export
    emu video
    width height video-x video-y
    mouse-x mouse-y mouse-pressed?
    audio-req? audio-left audio-right
    red green blue
    update
    file
    run-emu)
  (import
    (micac)
    (micac lib std)
    (micac lib sdl)
    (syntax)
    (identifier))

  (micac
    (externs
      video width height video-x video-y
      mouse-x mouse-y mouse-pressed?
      audio-req? audio-left audio-right
      red green blue
      update)

    (macro (file data data-size filename)
      (sdl-file-data data data-size filename))

    (macro
      (emu
        (video width-expr height-expr h-blank-expr v-blank-expr cycles-per-pixel-expr)
        body ...
        (update update-body ...))
      (keywords video init update)
      (macro width width-expr)
      (macro height height-expr)
      (macro h-blank h-blank-expr)
      (macro v-blank v-blank-expr)
      (macro h-size (+ width h-blank))
      (macro v-size (+ height v-blank))
      (macro cycles-per-pixel cycles-per-pixel-expr)
      (macro frame-cycles (* h-size v-size cycles-per-pixel))
      (macro window-scale 2)

      (var int video-x 0)
      (var int video-y 0)
      (var int pixel-cycle-counter 0)

      (var uint8_t red #x00)
      (var uint8_t green #x00)
      (var uint8_t blue #x00)

      (sdl-init)

      (sdl-window window "Emu" (* width window-scale) (* height window-scale))
      (sdl-renderer renderer window)
      (sdl-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING width height)

      (macro pixel-count (* width height))
      (macro bits-per-pixel 4)
      (macro pixels-size (* pixel-count bits-per-pixel))
      (macro pixels-pitch (* width bits-per-pixel))
      (alloc pixels uint8_t pixels-size)
      (var uint8_t (* pixel-ref) pixels)

      (const int audio-samples 256)

      (var SDL_AudioSpec audio-spec)
      (set (audio-spec freq) 22050)
      (set (audio-spec format) AUDIO_U8)
      (set (audio-spec channels) 2)
      (set (audio-spec samples) audio-samples)
      (set (audio-spec callback) 0)

      (sdl-audio-device audio-device (&ref audio-spec))

      (const int sample-buffer-size (* 2 audio-samples))
      (alloc sample-buffer uint8_t sample-buffer-size)
      (var uint8_t (* sample-buffer-ref) sample-buffer)
      (var int sample-counter 0)

      (const float frame-samples (div (cast float 22050) 60))
      (const float sample-cycles (div frame-cycles frame-samples))
      (printf "Cycles per frame: %i\\n" frame-cycles)
      (printf "Samples per frame: %f\\n" frame-samples)
      (printf "Cycles per sample: %f\\n" sample-cycles)

      (var int sample-cycle-counter 0)

      (var bool audio-req? #f)
      (var uint8_t audio-left 128)
      (var uint8_t audio-right 128)

      (sdl-pause-audio-device audio-device #f)

      (var int mouse-x 0)
      (var int mouse-y 0)
      (var bool mouse-pressed? #f)

      body ...
      (sdl-event-loop
        (var int sdl-mouse-x)
        (var int sdl-mouse-y)
        (const uint32_t sdl-mouse-state (sdl-get-mouse-state sdl-mouse-x sdl-mouse-y))
        (set mouse-x (div sdl-mouse-x window-scale))
        (set mouse-y (div sdl-mouse-y window-scale))
        (set mouse-pressed? (not (zero? (bitwise-and sdl-mouse-state #b1))))

        (repeat frame-cycles
          (set audio-req? (= sample-cycle-counter 0))

          update-body ...

          (when (zero? pixel-cycle-counter)
            (const bool h-video? (< video-x width))
            (const bool v-video? (< video-y height))
            (const bool video? (and h-video? v-video?))

            (when video?
              (set (pixel-ref *) #xff) ; alpha
              (inc pixel-ref)

              (set (pixel-ref *) red)
              (inc pixel-ref)

              (set (pixel-ref *) green)
              (inc pixel-ref)

              (set (pixel-ref *) blue)
              (inc pixel-ref)))

          (when (zero? pixel-cycle-counter)
            (set pixel-cycle-counter cycles-per-pixel)

            (inc video-x)
            (when (= video-x h-size)
              (set video-x 0)

              (inc video-y)
              (when (= video-y v-size)
                (set video-y 0)
                (set pixel-ref pixels))))
          (dec pixel-cycle-counter)

          (when audio-req?
            (set sample-cycle-counter + sample-cycles)

            (set (sample-buffer-ref *) audio-left)
            (inc sample-buffer-ref)
            (set (sample-buffer-ref *) audio-right)
            (inc sample-buffer-ref)

            (when (zero? sample-counter)
              (set sample-counter audio-samples)
              (set sample-buffer-ref sample-buffer)

              (const int queued-audio-size (sdl-get-queued-audio-size audio-device))
              (var int queue-audio-count 1)
              (cond
                ((= queued-audio-size 0)
                  (set queue-audio-count 2)
                  (printf "Audio queue underflow.\\n"))
                ((>= queued-audio-size (* 4 sample-buffer-size))
                  (set queue-audio-count 0)
                  (printf "Audio queue overflow.\\n")))
              (repeat queue-audio-count
                (sdl-queue-audio audio-device sample-buffer sample-buffer-size)))
            (dec sample-counter))
          (set sample-cycle-counter - 1)
        )

        (sdl-update-texture texture 0 pixels pixels-pitch)
        (sdl-render-copy renderer texture 0 0)
        (sdl-render-present renderer))))

  (define-rule-syntax (run-emu body ...)
    (micac (run (emu body ...))))
)
