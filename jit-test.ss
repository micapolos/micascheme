(import (scheme) (jit) (check) (lets))

(lets
  ($jit (jit #x10000))
  (begin
    (check (= (jit-size $jit) #x10000))
    (check (not (= (jit-address $jit) -1)))
    (jit-exec! $jit)
    (jit-edit! $jit)))
