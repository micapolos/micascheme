(library (asm frame)
  (export
    frame frame? frame-parameters frame-program
    empty-frame
    frame+fragment
    frame+label
    frame->syntax
    frame+syntax
    label)
  (import (micascheme) (asm program) (asm fragment) (asm parameters) (asm block))

  (define-keywords label)

  (data (frame parameters program))

  (define (empty-frame)
    (frame (empty-parameters) (empty-program)))

  (define (frame+fragment $frame $fragment)
    (frame
      ; TODO: Don't add parameters which match labels
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
    (frame
      (parameters-remove (frame-parameters $frame) $label)
      (program+label (frame-program $frame) $label)))

  (define (frame->syntax $org $frame)
    #`(lambda
      #,(parameters->syntax (frame-parameters $frame))
      #,(program->syntax $org (frame-program $frame))))

  (define (frame+syntax $frame $syntax)
    (syntax-case $syntax (label)
      ((label x)
        (identifier? #'x)
        (frame+label $frame #'x))
      (else
        (frame+fragment $frame (syntax->fragment $syntax)))))
)
