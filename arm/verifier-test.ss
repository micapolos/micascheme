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
  (verifier-emit-va64 $verifier (fn 0 0)))
