(import
  (scheme)
  (check)
  (arm64 va64)
  (arm64 encode)
  (arm64 verifier))

(let (($verifier (make-verifier)))
  (check (verifier-state-start? (verifier-state $verifier)))
  (check (raises (verifier-emit-a64 $verifier #x123))))

(let (($verifier (make-verifier)))
  (verifier-emit-va64 $verifier (fn 0 0)))
