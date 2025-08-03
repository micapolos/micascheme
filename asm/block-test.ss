(import (asm base) (asm block) (asm expression) (syntax lookup))

(check-block
  (empty-block)
  (block 1 0 (stack) (stack)))

(check-block
  (align-block 8)
  (block 8 0 (stack) (stack)))

(check-block
  (identifier-block #'foo)
  (block 1 0 (stack (foo 0)) (stack)))

(check-block
  (size-blob-block 10 (pure-expression #'binary-10))
  (block 1 10 (stack) (stack (dependent binary-10))))

(check-block
  (db-block
    (pure-expression #'10)
    (identifier-expression #'foo)
    (identifier-expression #'bar)
    (identifier-expression #'foo))
  (block 1 4
    (stack)
    (stack
      (dependent (foo bar)
        (db-binary 10 foo bar foo)))))

(check-block
  (dw-block
    (pure-expression #'#x1234)
    (identifier-expression #'foo)
    (identifier-expression #'bar)
    (identifier-expression #'foo))
  (block 1 8
    (stack)
    (stack
      (dependent (foo bar)
        (dw-binary #x1234 foo bar foo)))))

(check-block
  (offset-block 10 (identifier-block #'foo))
  (block 1 0
    (stack (foo 10))
    (stack)))

(check-block
  (offset-block 10 (db-block (identifier-expression #'foo)))
  (block 1 1 (stack) (stack (dependent (foo) (db-binary foo)))))

(check-block
  (block-append)
  (block 1 0 (stack) (stack)))

(check-block
  (block-append
    (identifier-block #'start)
    (db-block (identifier-expression #'foo))
    (align-block 4)
    (db-block (identifier-expression #'bar))
    (align-block 2)
    (dw-block (identifier-expression #'end))
    (identifier-block #'end))
   (block 4 8
    (stack
      (start 0)
      (end 8))
    (stack
      (dependent (foo) (db-binary foo))
      (dependent (zero-binary 3))
      (dependent (bar) (db-binary bar))
      (dependent (zero-binary 1))
      (dependent (end) (dw-binary end)))))

(check-block
  (bytevector-block (bytevector 1 2 3))
  (block 1 3 (stack) (stack (dependent (db-binary 1 2 3)))))

(check-block
  (dz-block "foo")
  (block 1 4 (stack)
    (stack
      (dependent (db-binary 102 111 111))
      (dependent (db-binary 0)))))

(check-block
  (utf8-block "\x0;\x10;\x20;" "\x1234;")
  (block 1 6 (stack)
    (stack
      (dependent (db-binary #x00 #x10 #x20))
      (dependent (db-binary #xe1 #x88 #xb4)))))

(check-block
  (ascii-block "\x0;\x10;\x20;" "\xff;\x1234;")
  (block 1 5 (stack)
    (stack
      (dependent (db-binary #x00 #x10 #x20))
      (dependent (db-binary #xff #x34)))))
