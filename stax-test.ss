(import
  (except (micascheme) push pop)
  (stax))

(run
  (push 1)
  (push 2)
  (check (equal? (pop) 2))
  (check (equal? (pop) 1)))
