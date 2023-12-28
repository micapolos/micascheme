(import
  (except (micascheme) push pop load)
  (stax))

(run
  (push 1)
  (push 2)
  (check (equal? (pop) 2))
  (check (equal? (pop) 1)))
