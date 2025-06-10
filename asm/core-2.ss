(library (asm core-2)
  (export asm org emit flush)
  (import (micascheme))

  (define put-procs-parameter (make-thread-parameter (stack)))
  (define org-parameter (make-thread-parameter 0))

  (define-rule-syntax (org value)
    (org-parameter value))

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
              (push! $instructions #'(set! x (org-parameter)))))
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
    (begin
      (put-procs-parameter
        (push (put-procs-parameter)
          (lambda ($port) body ...)))
      (org-parameter (+ (org-parameter) $size))))

  (define (flush $port)
    (run
      (for-each
        (lambda ($put-proc) ($put-proc $port))
        (reverse (put-procs-parameter)))
      (put-procs-parameter (stack))
      (org-parameter 0)))
)
