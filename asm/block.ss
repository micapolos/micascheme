(library (asm block)
  (export
    block block?
    block-size block-labels block-relocable-binary-syntaxes block-import-base block-imports
    block-with-size block-with-labels block-with-relocable-binary-syntaxes block-with-import-base block-with-imports
    empty-block
    block-relocable-binary-syntax
    block+label
    block+relocable-binary-syntax
    block+zeroes
    block-bind
    block+import
    list->block
    block-append
    block->map-string)
  (import
    (micascheme)
    (asm binary)
    (asm-2 relocable)
    (code))

  (data (block size labels relocable-binary-syntaxes import-base imports))

  (define (empty-block)
    (block 0 (stack) (stack) #'() (stack)))

  (define (block+label $block $label)
    (block-with-labels $block
      (push
        (block-labels $block)
        (cons $label (block-size $block)))))

  (define (block+relocable-binary-syntax $block $size $relocable-binary-syntax)
    (block-with-relocable-binary-syntaxes
      (block-with-size $block (+ (block-size $block) $size))
      (push
        (block-relocable-binary-syntaxes $block)
        (relocable+offset $relocable-binary-syntax (block-size $block)))))

  (define (block+import $block $import $proc)
    (cond
      ((member $import (block-imports $block)) $block)
      (else
        (fluent $block
          (block-with-imports (push (block-imports $block) $import))
          (block-apply $proc)))))

  (define (block-apply $block $proc)
    ($proc $block))

  (define (offset-label $offset $label)
    (cons (car $label) (+ (cdr $label) $offset)))

  (define (block+ $block-a $block-b)
    (lets
      ($size-a (block-size $block-a))
      (block
        (+ $size-a (block-size $block-b))
        (push-all
          (block-labels $block-a)
          (map (partial offset-label $size-a) (block-labels $block-b)))
        (push-all
          (block-relocable-binary-syntaxes $block-a)
          (map (partial offset-relocable $size-a) (block-relocable-binary-syntaxes $block-b)))
        (block-import-base $block-a)
        (push-all
          (block-imports $block-a)
          (block-imports $block-b)))))

  (define-list->/append (block $blocks)
    (fold-left block+ (empty-block) $blocks))

  (define (localize-block $block)
    (fluent $block
      (block-with-labels (stack))
      (block-with-relocable-binary-syntaxes
        (stack (block-relocable-binary-syntax $block)))))

  (define (block-bind $block $proc)
    (block+ $block (localize-block ($proc (empty-block)))))

  (define (block+zeroes $block $size)
    (block+relocable-binary-syntax $block $size
      (relocable-with
        #`(zero-binary #,(literal->syntax $size)))))

  (define (block-align $block $alignment)
    (block+zeroes $block
      (bitwise-align (block-size $block) $alignment)))

  (define (block-relocable-binary-syntax $block)
    (relocable-with ($org)
      #`(let
        (#,@(map-with ($label (reverse (block-labels $block)))
          #`(
            #,(car $label)
            #,(literal->syntax (+ $org (cdr $label))))))
        #,(relocable-ref
          (relocable-map
            (lambda ($binary-syntaxes)
              (or
                (single $binary-syntaxes)
                #`(binary-append #,@$binary-syntaxes)))
            (list->relocable (reverse (block-relocable-binary-syntaxes $block))))
          $org))))

  (define (block->map-string $block)
    (code-string
      (list->code
        (map-with ($label (reverse (block-labels $block)))
          (code
            (string-code (format "~8,'0X" (cdr $label)))
            #\space
            (string-code (format "~8,'0X" (cdr $label)))
            #\space
            "00"
            #\space
            (string-code (symbol->string (syntax->datum (car $label))))
            #\newline)))))
)
