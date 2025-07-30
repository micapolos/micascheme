(import
  (asm base)
  (asm identified)
  (asm fragment)
  (asm aligned)
  (asm sized)
  (asm linked))

(check-list->linked
  (identified #'const-1 #'expr-1)
  (identified #'proc-1 (aligned 1 (sized 3 #'binary-1)))
  (identified #'const-2 #'expr-2)
  (identified #'proc-2 (aligned 4 (sized 1 #'binary-2)))
  (identified #'const-3 #'expr-3)
  (identified #'proc-3 (aligned 2 (sized 5 #'binary-3)))
  (linked
    (proc-2 0)
    (proc-3 2)
    (proc-1 7)
    (relocable-with ($org)
      (lets
        (proc-2 (+ $org 0))
        (proc-3 (+ $org 2))
        (proc-1 (+ $org 7))
        (const-1 expr-1)
        (const-2 expr-2)
        (const-3 expr-3)
        (relocable-ref
          (map-relocable list->binary
            (relocable-append
              (offset-relocable 0
                (relocable-map binary-2
                  (lambda ($binary)
                    (binary-append $binary (zero-binary 1)))))
              (offset-relocable 2 binary-3)
              (offset-relocable 7 binary-1))) $org)))))



