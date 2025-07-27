(import (asm base) (asm block) (asm syntax-expression) (asm syntax-block) (syntax lookup))

(define lookup
  (lookup-with
    (db
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ x ...)
              (apply db-block
                (map (partial syntax->expression $lookup) #'(x ...))))))))))

(check-syntax->block lookup
  foo
  (block 1 0 (stack (foo 0)) (stack)))

(check-syntax->block lookup
  (align 4)
  (block 4 0 (stack) (stack)))

(check-syntax->block lookup
  (db 10 foo bar foo)
  (block 1 4
    (stack)
    (stack
      (dependent (foo bar)
        (db-binary 10 foo bar foo)))))

(check-syntax->block lookup
  (begin
    start
    (db 10 start end)
    (db foo bar foo)
    end)
  (block 1 6
    (stack
      (start 0)
      (end 6))
    (stack
      (dependent (start end) (db-binary 10 start end))
      (dependent (foo bar) (db-binary foo bar foo)))))
