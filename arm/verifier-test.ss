(import
  (scheme)
  (check)
  (arm va64)
  (arm encode)
  (arm verifier))

(let (($verifier (make-verifier)))
  (check (verifier-state-start? (verifier-state $verifier)))
  (check (raises (verifier-emit-a64 $verifier #x123))))

(check-verifier-error (va64 (fn 1 0)))
(check-verifier-error (va64 (fn 0 1)))

(with-verifier $verifier
  (va64 (fn 0 0))
  (check (verifier-state-function? (verifier-state $verifier))))

(with-verifier $verifier
  (va64 (fn 0 0))
  (a64 (nop))
  (begin
    (check (verifier-state-function? (verifier-state $verifier)))
    (check (fx= (verifier-pc $verifier) 1))))
