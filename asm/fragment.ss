(library (asm fragment)
  (export
    fragment fragment? fragment-parameters fragment-block
    program program? program-labels program-org program-block
    label->program
    program->syntax
    fragment->datum
    syntax->fragment)
  (import (micascheme) (syntax lookup) (asm block) (asm expression))

  (data (fragment parameters block))
  (data (program labels org block))

  (define (fragment->datum $fragment)
    `(fragment
      (,@(map syntax->datum (fragment-parameters $fragment)))
      ,(block->datum (fragment-block $fragment))))

  (define (label-ref $labels $id)
    (lets
      ($ass? (assid $id $labels))
      (and $ass? (cdr $ass?))))

  (define (empty-program $org)
    (program '() $org (empty-block)))

  (define (program+label $lookup $program $label)
    (lets
      ($labels (program-labels $program))
      (cond
        ((label-ref $labels $label) $program)
        (else
          (lets
            ($org (program-org $program))
            ($program-block (program-block $program))
            ($fragment (lookup-ref $lookup $label))
            ($fragment-block (fragment-block $fragment))
            ($program
              (program
                (push $labels (cons $label $org))
                (+ $org (block-size $fragment-block))
                (block-append $program-block $fragment-block)))
            (fold-left
              (partial program+label $lookup)
              $program
              (fragment-parameters $fragment)))))))

  (define (label->program $lookup $org $label)
    (program+label
      $lookup
      (empty-program $org)
      $label))

  (define (label->syntax $label)
    #`(
      #,(car $label)
      #,(literal->syntax (cdr $label))))

  (define (program->syntax $program)
    (lets
      ($labels (program-labels $program))
      ($block (program-block $program))
      #`(lets
        #,@(map label->syntax (reverse $labels))
        #,(block->syntax $block))))

  (define (syntax->fragment $syntax)
    (syntax-case $syntax (begin db dw call ret)
      ((db arg ...)
        (lets
          ($expressions (map (partial syntax->expression '()) #'(arg ...)))
          (fragment
            (apply append (map expression-parameters $expressions))
            (block
              (length $expressions)
              (lambda ($port-identifier)
                (map-with ($expression (reverse $expressions))
                  #`(put-u8 #,$port-identifier
                    #,(expression-syntax $expression))))))))))
)
