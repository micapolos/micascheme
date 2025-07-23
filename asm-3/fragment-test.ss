(import
  (asm-3 base)
  (asm-3 expression)
  (asm-2 aligned)
  (asm-3 sized)
  (asm lookable)
  (asm-3 fragment)
  (asm-3 dependent)
  (asm-2 relocable))

(check-fragment 100
  (lookup-with (foo 10))
  (dependent-with (foo)
    (aligned 2
      (sized 4
        (relocable-with ($org)
          (lookable ($lookup)
            (u8-binary (+ $org ($lookup #'foo))))))))
  (dependent (foo) (aligned 2 (sized 4 (binary 110)))))
