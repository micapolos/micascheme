(import (asm-3 base) (asm-3 expression) (asm-3 fragment) (asm-3 dependent) (syntax lookup))

; db

(check-fragment #xc000
  (empty-lookup)
  (db)
  (dependent (aligned 1 (sized 0 (binary)))))

(check-fragment #xc000
  (empty-lookup)
  (db 10 20 30)
  (dependent (aligned 1 (sized 3 (binary 10 20 30)))))

(check-fragment 100
  (empty-lookup)
  (db org)
  (dependent (aligned 1 (sized 1 (binary 100)))))

(check-fragment #xc000
  (lookup-with (foo 10))
  (db foo)
  (dependent (foo) (aligned 1 (sized 1 (binary 10)))))

(check-fragment #xc000
  (lookup-with (+ +) (foo 10) (bar 20))
  (db (+ foo bar))
  (dependent (+ foo bar) (aligned 1 (sized 1 (binary 30)))))

; dw

(check-fragment #xc000
  (empty-lookup)
  (dw)
  (dependent (aligned 1 (sized 0 (binary)))))

(check-fragment #xc000
  (empty-lookup)
  (dw #x0123)
  (dependent (aligned 1 (sized 2 (binary #x23 #x01)))))

(check-fragment #xc000
  (empty-lookup)
  (dw org)
  (dependent (aligned 1 (sized 2 (binary #x00 #xc0)))))

(check-fragment #xc000
  (lookup-with (foo #x1234))
  (dw foo)
  (dependent (foo) (aligned 1 (sized 2 (binary #x34 #x12)))))
