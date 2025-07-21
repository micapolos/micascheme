(import (asm-3 base) (asm-3 syntax-block) (syntax lookup) (asm-3 org))

(check-syntax->block #xc000
  (empty-lookup)
  (db 10 20 30)
  (dependent
    (aligned 1
      (sized 3
        (environmental
          (block
            (binary 10)
            (binary 20)
            (binary 30)))))))

(check-syntax->block #xc000
  (empty-lookup)
  (dw #x1234 #x5678)
  (dependent
    (aligned 1
      (sized 4
        (environmental
          (block
            (binary #x34 #x12)
            (binary #x78 #x56)))))))

(check-syntax->block #xc000
  (empty-lookup)
  (dw org)
  (dependent
    (aligned 1
      (sized 2
        (environmental
          (block (binary #x00 #xc0)))))))

(check-syntax->block #xc000
  (empty-lookup)
  (align 4)
  (dependent (aligned 4 (sized 0 (environmental (block))))))

(check-syntax->block #xc000
  (empty-lookup)
  foo
  (dependent (aligned 1 (sized 0 (environmental (foo #xc000) (block))))))

(check-syntax->block #xc000
  (empty-lookup)
  (begin
    (db 10)
    (db 20))
  (dependent
    (aligned 1
      (sized 2
        (environmental
          (block
            (binary 10)
            (binary 20)))))))
