(library (leo3 load)
  (export
    leo-load
    leo-load-program)
  (import
    (scheme)
    (prefix (leo3 read) leo-)
    (leo3 path)
    (leo3 environment))

  (define leo-load
    (case-lambda
      (($path)
        (leo-load $path (current-eval)))
      (($path $eval)
        (if (path-leo? $path)
          (parameterize ((interaction-environment leo-interaction-environment))
            (for-each $eval (leo-read-file $path)))
          (parameterize ((interaction-environment scheme-interaction-environment))
            (load $path $eval))))))

  (define leo-load-program
    (case-lambda
      (($path)
        (leo-load-program $path (current-eval)))
      (($path $eval)
        (if (path-leo? $path)
          (parameterize ((interaction-environment leo-interaction-environment))
            ($eval `(top-level-program ,@(leo-read-file $path))))
          (parameterize ((interaction-environment scheme-interaction-environment))
            (load-program $path $eval))))))
)
