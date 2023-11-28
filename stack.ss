(library (stack)
  (export
    stack
    push push-list push-all
    top pop)
  (import
    (scheme))

  (define (stack . $items) (reverse $items))

  (define (push $stack $item) (cons $item $stack))
  (define (push-list $stack $list) (fold-left push $stack $list))
  (define (push-all $stack $stack2) (append $stack2 $stack))
  (define (top $stack) (car $stack))
  (define (pop $stack) (cdr $stack))
)
