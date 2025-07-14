(library (asm-2 assembler)
  (export
    assembler pure-assembler assemble
    assembler-bind
    assembler-map
    assembler-bind-with
    list->assembler
    assembler-append
    ref-assembler
    define-assembler)
  (import (micascheme) (syntax lookup))

  (define-rule-syntax (assembler ($lookup) lookup/value)
    (lambda ($lookup) lookup/value))

  (define (pure-assembler $value)
    (assembler ($lookup)
      (values $lookup $value)))

  (define assemble
    (case-lambda
      (($assembler $lookup)
        ($assembler $lookup))
      (($assembler)
        (lets
          ((values $lookup $value) (assemble $assembler (empty-lookup)))
          $value))))

  (define (assembler-bind $proc $assembler)
    (assembler ($lookup)
      (lets
        ((values $lookup $value) (assemble $assembler $lookup))
        (assemble ($proc $value) $lookup))))

  (define (ref-assembler $label)
    (assembler ($lookup)
      (values $lookup (lookup-ref $lookup $label))))

  (define (define-assembler $label $value)
    (assembler ($lookup)
      (values (lookup+ $lookup $label $value) (void))))

  (define (assembler-map $proc $assembler)
    (assembler-bind pure-assembler $assembler))

  (define-rules-syntax
    ((assembler-bind-with body) body)
    ((assembler-bind-with (var expr) xs ...)
      (assembler-bind
        (lambda (var) (assembler-bind-with xs ...))
        expr)))

  (define (list->assembler $assemblers)
    (fold-left
      (lambda ($list-assembler $assembler)
        (assembler-bind-with
          ($value $assembler)
          ($list $list-assembler)
          (pure-assembler (cons $value $list))))
      (pure-assembler (list))
      (reverse $assemblers)))

  (define (assembler-append . $assemblers)
    (list->assembler $assemblers))
)
