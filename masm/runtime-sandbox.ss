(import (masm runtime))

(push 3)
(switch
  (push 0)
  (push 10)
  (push 20)
  (do
    (push 15)
    (dup)
    (add))
  (push 40))
(out)
