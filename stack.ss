(library (stack)
  (export
    stack
    push push-list push-all
    top pop)
  (import
    (scheme))

  (define-syntax stack
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $item ...)
          #`(list #,@(reverse (syntax->list #`($item ...))))))))

  (define (push $stack $item) (cons $item $stack))
  (define (push-list $stack $list) (fold-left push $stack $list))
  (define (push-all $stack $stack2) (append $stack2 $stack))
  (define (top $stack) (car $stack))
  (define (pop $stack) (cdr $stack))
)
