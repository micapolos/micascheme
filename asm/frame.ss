(library (asm frame)
  (export
    frame frame? frame-parameters frame-program)
  (import (micascheme) (asm program) (asm fragment) (asm parameters) (asm block))

  (data (frame parameters program))

  (define (frame+fragment $frame $fragment)
    (frame
      (parameters-append
        (frame-parameters $frame)
        (fragment-parameters $fragment))
      (lets
        ($program (frame-program $frame))
        (program-with-block $program
          (block-append
            (program-block $program)
            (fragment-block $fragment))))))
)
