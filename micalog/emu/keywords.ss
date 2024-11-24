(library (micalog emu keywords)
  (export
    video-x video-y
    video-red video-green video-blue
    mouse-x mouse-y mouse-pressed?)
  (import (micascheme))

  (define-aux-keywords
    video-x video-y
    video-red video-green video-blue
    mouse-x mouse-y mouse-pressed?)
)
