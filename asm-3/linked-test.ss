(import
  (asm-3 base)
  (asm-3 identified)
  (asm-3 fragment)
  (asm-2 aligned)
  (asm-3 sized)
  (asm-3 linked))

(check-list->linked
  (identified #'const-1 #'expr-1)
  (identified #'proc-1 (aligned 1 (sized 3 #'binary-1)))
  (identified #'const-2 #'expr-2)
  (identified #'proc-2 (aligned 4 (sized 1 #'binary-2)))
  (identified #'const-3 #'expr-3)
  (identified #'proc-3 (aligned 2 (sized 5 #'binary-3)))
  (linked
    (proc-2 0)
    (proc-3 1)
    (proc-1 6)
    (lets
      (proc-2 0)
      (proc-3 1)
      (proc-1 6)
      (const-1 expr-1)
      (const-2 expr-2)
      (const-3 expr-3)
      (map-relocable list->binary
        (relocable-append
          (offset-relocable 0 binary-2)
          (offset-relocable 1 binary-3)
          (offset-relocable 6 binary-1))))))
