(library (asm frame)
  (export
    frame frame? frame-parameters frame-program
    frame+fragment
    frame+label
    frame->syntax)
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

  (define (frame+label $frame $label)
    (frame-with-program $frame
      (program+label (frame-program $frame) $label)))

  (define (frame->syntax $org $frame)
    #`(lambda
      #,(parameters->syntax (frame-parameters $frame))
      #,(program->syntax $org (frame-program $frame))))
)
