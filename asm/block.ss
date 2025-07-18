(library (asm block)
  (export
    block block?

    block-size
    block-labels
    block-lookable-relocable-binary-syntaxes
    block-import-base
    block-imports

    block-with-size
    block-with-labels
    block-with-lookable-relocable-binary-syntaxes
    block-with-import-base
    block-with-imports

    empty-block
    block-lookable-relocable-binary-syntax
    block+label
    block+lookable-relocable-binary-syntax
    block+zeroes
    block-bind
    block+import
    block->map-string
    block->datum
    check-block)
  (import
    (micascheme)
    (asm binary)
    (asm-2 relocable)
    (asm lookable)
    (code))

  (data (block size labels lookable-relocable-binary-syntaxes import-base imports))

  (define (empty-block)
    (block 0 (stack) (stack) #'() (stack)))

  (define (block+label $block $label)
    (block-with-labels $block
      (push
        (block-labels $block)
        (cons $label (block-size $block)))))

  (define (block+lookable-relocable-binary-syntax $block $size $lookable-relocable-binary-syntax)
    (block-with-lookable-relocable-binary-syntaxes
      (block-with-size $block (+ (block-size $block) $size))
      (push
        (block-lookable-relocable-binary-syntaxes $block)
        (lookable-map
          (lambda ($relocable-binary-syntax)
            (relocable+offset $relocable-binary-syntax (block-size $block)))
          $lookable-relocable-binary-syntax))))

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

  (define (localize-block $block)
    (fluent $block
      (block-with-labels (stack))
      (block-with-lookable-relocable-binary-syntaxes
        (stack (block-lookable-relocable-binary-syntax $block)))))

  (define (block-bind $block $proc)
    (lets
      ($local-block ($proc (empty-block)))
      (fluent $block
        (block-with-size (+ (block-size $block) (block-size $local-block)))
        (block-with-lookable-relocable-binary-syntaxes
          (push
            (block-lookable-relocable-binary-syntaxes $block)
            (lookable-map
              (lambda ($local-relocable-binary-syntax)
                (relocable+offset $local-relocable-binary-syntax (block-size $block)))
              (block-lookable-relocable-binary-syntax $local-block)))))))

  (define (block+zeroes $block $size)
    (block+lookable-relocable-binary-syntax $block $size
      (lookable
        (relocable-with
          #`(zero-binary #,(literal->syntax $size))))))

  (define (block-align $block $alignment)
    (block+zeroes $block
      (bitwise-align (block-size $block) $alignment)))

  (define (block-lookable-relocable-binary-syntax $block)
    (lookable ($lookup)
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
              (list->relocable
                (reverse
                  (lookable-ref
                    (list->lookable (block-lookable-relocable-binary-syntaxes $block))
                    $lookup))))
            $org)))))

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

  (define (block->datum $lookup $org $block)
    `(block
      ,(syntax->datum (block-import-base $block))
      (import ,@(map syntax->datum (reverse (block-imports $block))))
      ,(syntax->datum
        (relocable-ref
          (lookable-ref (block-lookable-relocable-binary-syntax $block) $lookup)
          $org))))

  (define-rules-syntax
    ((check-block lookup org block stx)
      (check (equal? (block->datum lookup org block) 'stx))))
)
