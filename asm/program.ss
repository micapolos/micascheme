(library (asm program)
  (export
    program program? program-labels program-block
    label->program
    fragment->program
    program->syntax)
  (import (micascheme) (syntax lookup) (asm block) (asm fragment))

  (data (program labels block))

  (define (empty-program)
    (program '() (empty-block)))

  (define (label-ref $labels $id)
    (lets
      ($ass? (assid $id $labels))
      (and $ass? (cdr $ass?))))

  (define (program+label $lookup $program $label)
    (lets
      ($labels (program-labels $program))
      (cond
        ((label-ref $labels $label) $program)
        (else
          (lets
            ($program-block (program-block $program))
            ($program-size (block-size $program-block))
            ($fragment (lookup-ref $lookup $label))
            ($fragment-block (fragment-block $fragment))
            ($program
              (program
                (push $labels (cons $label $program-size))
                (block-append $program-block $fragment-block)))
            (fold-left
              (partial program+label $lookup)
              $program
              (fragment-parameters $fragment)))))))

  (define (label->program $lookup $label)
    (program+label
      $lookup
      (empty-program)
      $label))

  (define (fragment->program $lookup $fragment)
    (fold-left
      (partial program+label $lookup)
      (program (stack) (fragment-block $fragment))
      (fragment-parameters $fragment)))

  (define (label->syntax $org $label)
    #`(
      #,(car $label)
      #,(literal->syntax (+ $org (cdr $label)))))

  (define (program->syntax $org $program)
    (lets
      ($labels (program-labels $program))
      ($block (program-block $program))
      #`(lets
        #,@(map (partial label->syntax $org) (reverse $labels))
        #,(block->syntax $block))))
)
