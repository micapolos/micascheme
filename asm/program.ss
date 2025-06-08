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

  (define (program+label $program $label)
    (or
      (program+label? $program $label)
      (syntax-error $label "already defined")))

  (define (program+label? $program $label)
    (lets
      ($labels (program-labels $program))
      (and (not (label-ref $labels $label))
        (program-with-labels $program
          (push $labels
            (cons $label
              (block-size (program-block $program))))))))

  (define (program+import $lookup $program $label)
    (switch (program+label? $program $label)
      ((false? _) $program)
      ((else $program)
        (lets
          ($program-block (program-block $program))
          ($fragment (lookup-ref $lookup $label))
          ($fragment-block (fragment-block $fragment))
          ($program
            (program-with-block $program
              (block-append $program-block $fragment-block)))
          (fold-left
            (partial program+import $lookup)
            $program
            (fragment-parameters $fragment))))))

  (define (label->program $lookup $label)
    (program+import
      $lookup
      (empty-program)
      $label))

  (define (fragment->program $lookup $fragment)
    (fold-left
      (partial program+import $lookup)
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
