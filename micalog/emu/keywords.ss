(library (micalog emu keywords)
  (export
    clock reset?
    video-x video-y
    video-red video-green video-blue
    mouse-x mouse-y mouse-pressed?)
  (import (micascheme))

  (define-aux-keywords
    clock reset?
    video-x video-y
    video-red video-green video-blue
    mouse-x mouse-y mouse-pressed?)
)
