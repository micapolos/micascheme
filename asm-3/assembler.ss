(library (asm-3 assembler)
  (export
    assembler? assembler-alignment assembler-size assembler-deps

    empty-assembler
    assembler+dep
    assembler+label
    assembler+define
    assembler+binary
    assembler+zeros
    assembler-align
    assembler->syntax
    assembler->datum
    check-assembler)
  (import (micascheme) (syntax lookup))

  (define-keywords $org $port)

  (data (assembler-transformer proc))
  (data (assembler alignment size deps label-syntaxes define-syntaxes run-syntaxes))

  (define (empty-assembler)
    (assembler 1 0 (stack) (stack) (stack) (stack)))

  (define (assembler+dep $assembler $dep)
    (lets
      ($deps (assembler-deps $assembler))
      (assembler-with-deps $assembler
        (cond
          ((memp (partial free-identifier=? $dep) $deps) $deps)
          (else (push $deps $dep))))))

  (define (assembler+label $assembler $label)
    (assembler-with-label-syntaxes $assembler
      (push
        (assembler-label-syntaxes $assembler)
        #`(#,$label (+ org #,(literal->syntax (assembler-size $assembler)))))))

  (define (assembler+define $assembler $identifier $syntax)
    (assembler-with-define-syntaxes $assembler
      (push
        (assembler-define-syntaxes $assembler)
        #`(#,$identifier #,$syntax))))

  (define (assembler+binary $assembler $size $syntax)
    (fluent $assembler
      (assembler-with-run-syntaxes
        (push
          (assembler-run-syntaxes $assembler)
          #`(put-binary $port #,$syntax)))
      (assembler-with-size
        (+ (assembler-size $assembler) $size))))

  (define (assembler+zeros $assembler $size)
    (if (zero? $size)
      $assembler
      (assembler+binary $assembler $size
        #`(zero-binary #,(literal->syntax $size)))))

  (define (assembler-align $assembler $alignment)
    (lets
      ($size (assembler-size $assembler))
      (fluent $assembler
        (assembler-with-alignment (max (assembler-alignment $assembler) $alignment))
        (assembler+zeros (- (bitwise-align $size $alignment) $size)))))

  (define (assembler->syntax $assembler)
    #`(lambda ($org)
      (lambda ($port)
        (lets
          #,@(reverse (assembler-label-syntaxes $assembler))
          #,@(reverse (assembler-define-syntaxes $assembler))
          (run #,@(reverse (assembler-run-syntaxes $assembler)))))))

  (define (assembler->datum $assembler)
    `(assembler
      (alignment ,(assembler-alignment $assembler))
      (size ,(assembler-size $assembler))
      (deps ,@(reverse (map syntax->datum (assembler-deps $assembler))))
      ,@(reverse (map syntax->datum (assembler-label-syntaxes $assembler)))
      ,@(reverse (map syntax->datum (assembler-define-syntaxes $assembler)))
      ,@(reverse (map syntax->datum (assembler-run-syntaxes $assembler)))))

  (define-rule-syntax (check-assembler in out)
    (check (equal? (assembler->datum in) 'out)))
)
