(library (asm fragment)
  (export
    fragment fragment? fragment-parameters fragment-block
    program program? program-labels program-org program-block
    org-label->put-proc-syntax)
  (import (micascheme) (syntax lookup) (asm block))

  (data (fragment parameters block))
  (data (program labels org block))

  (define (label-ref $labels $id)
    (lets
      ($ass? (assid $id $labels))
      (and $ass? (cdr $ass?))))

  (define (empty-program)
    (program '() 0 (empty-block)))

  (define (program+label $fragment-lookup $program $label)
    (lets
      ($labels (program-labels $program))
      (cond
        ((label-ref $labels $label) $program)
        (else
          (lets
            ($fragment (lookup-ref $fragment-lookup $label))
            ($program
              (fold-left
                (partial program+label $fragment-lookup)
                $program
                (fragment-parameters $fragment)))
            ($org (program-org $program))
            ($labels (program-labels $program))
            ($program-block (program-block $program))
            ($fragment-block (fragment-block $fragment))
            (program
              (push $labels (cons $label $org))
              (+ $org (block-size $fragment-block))
              (block-append $program-block $fragment-block)))))))

  (define (org-label->put-proc-syntax $fragment-lookup $label)
    (lets
      ((program $labels _ $block)
        (program+label
          $fragment-lookup
          (empty-program)
          $label))
      #`(lambda ($port $org)
        (lets
          #,@(map-with
            ($label (reverse $labels))
            #`(
              #,(car $label)
              (+ $org #,(literal->syntax (cdr $label)))))
          (run
            #,@(block->put-syntaxes $block #'$port))))))
)
