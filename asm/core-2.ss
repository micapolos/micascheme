(library (asm core-2)
  (export asm org emit flush)
  (import (micascheme))

  (define-record-type builder
    (fields (mutable put-procs) (mutable org))
    (protocol
      (lambda (new)
        (lambda ()
          (new (stack) 0)))))

  (define current-builder (make-thread-parameter (make-builder)))

  (define-rule-syntax (with-current-builder ($builder) body ...)
    (lets
      ($builder (current-builder))
      (run-void body ...)))

  (define (org $org)
    (with-current-builder ($builder)
      (builder-org-set! $builder $org)))

  (define-syntax (asm $syntax)
    (begin
      (define $labels (stack))
      (define $definitions (stack))
      (define $instructions (stack))
      (define (parse $op)
        (syntax-case $op (label)
          (x
            (identifier? #'x)
            (begin
              (push! $labels #'(x 0))
              (push! $instructions #'(set! x (builder-org (current-builder))))))
          (instruction
            (push! $instructions #'instruction))))
      (syntax-case $syntax ()
        ((_ body body* ...)
          (begin
            (for-each parse #'(body body* ...))
            #`(let
              (#,@(reverse $labels))
              #,@(reverse $instructions)))))))

  (define-rule-syntax (emit ($port $size) body ...)
    (with-current-builder ($builder)
      (builder-put-procs-set! $builder
        (push (builder-put-procs $builder) (lambda ($port) body ...)))
      (builder-org-set! $builder (+ (builder-org $builder) $size))))

  (define (flush $port)
    (with-current-builder ($builder)
      (for-each
        (lambda ($put-proc) ($put-proc $port))
        (reverse (builder-put-procs $builder)))
      (builder-put-procs-set! $builder (stack))
      (builder-org-set! $builder 0)))
)
