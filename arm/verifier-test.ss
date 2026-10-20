(import
  (scheme)
  (check)
  (arm va64)
  (arm encode)
  (arm verifier))

(let (($verifier (make-verifier)))
  (check (verifier-state-start? (verifier-state $verifier)))
  (check (raises (verifier-emit-a64 $verifier #x123))))

(let (($verifier (make-verifier)))
  (check (raises (verifier-emit-va64 $verifier (fn 1 0)))))

(let (($verifier (make-verifier)))
  (check (raises (verifier-emit-va64 $verifier (fn 0 1)))))

(let (($verifier (make-verifier)))
  (verifier-emit-va64 $verifier (fn 0 0))
  (check (verifier-state-function? (verifier-state $verifier))))

(let (($verifier (make-verifier)))
  (verifier-emit $verifier
    (va64 (fn 0 0))
    (a64 (nop)))
  (check (verifier-state-function? (verifier-state $verifier)))
  (check (fx= (verifier-pc $verifier) 1)))
