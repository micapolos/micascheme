(import
  (micac)
  (c run)
  (micac lib std)
  (micac lib sdl)
  (only (micascheme) parameterize))

(parameterize ((c-run-echo? #t))
  (micac
    (run
      (macro width 352)
      (macro height 288)
      (macro window-scale 2)
      (macro pixel-count (* width height))
      (macro bits-per-pixel 4)
      (macro pixels-size (* pixel-count bits-per-pixel))
      (macro pixels-pitch (* width bits-per-pixel))
      (macro sample-count 4096)

      (sdl-init)

      (var SDL_AudioSpec audio-spec)

      (set (audio-spec freq) 22050)
      (set (audio-spec format) AUDIO_U8)
      (set (audio-spec channels) 1)
      (set (audio-spec samples) 256)
      (set (audio-spec callback) 0)
      (sdl-audio-device audio-device (&ref audio-spec))

      (alloc samples uint8_t sample-count)

      (sdl-pause-audio-device audio-device #f)

      (sdl-window window "My window" (* width window-scale) (* height window-scale))
      (sdl-renderer renderer window)
      (sdl-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING width height)

      (alloc pixels uint8_t pixels-size)
      (var int frame-counter 0)

      (var uint16_t sample-1 0)
      (var uint16_t sample-2 0)

      (sdl-event-loop
        (begin
          (var uint8_t (* pixel-ref) pixels)
          (var uint8_t value (* frame-counter 8))
          (repeat pixels-size
            (set (pixel-ref *) value)
            (set pixel-ref + 1)
            (set value + 1)))
        (set frame-counter + 1)
        (sdl-update-texture texture 0 pixels pixels-pitch)
        (sdl-render-copy renderer texture 0 0)
        (sdl-render-present renderer)

        (when (< (sdl-get-queued-audio-size audio-device) sample-count)
          (var uint8_t (* samples-ref) samples)
          (for-each (i sample-count)
            (set sample-1 + 257)
            (set sample-2 + 258)
            (set (samples-ref *) (bitwise-and (>> (+ (>> sample-1 8) (>> sample-2 8)) 1) #xff))
            (inc samples-ref))

          (sdl-queue-audio audio-device samples sample-count))))))
