(library (leo3 load)
  (export
    leo-load
    leo-load-program)
  (import
    (scheme)
    (prefix (leo3 read) leo-)
    (leo3 rewriter)
    (leo3 path))

  (define leo-load
    (case-lambda
      (($path)
        (leo-load $path (current-eval)))
      (($path $eval)
        (if (path-leo? $path)
          (for-each $eval (leo-read-file $path))
          (load $path $eval)))))

  (define leo-load-program
    (case-lambda
      (($path)
        (leo-load-program $path (current-eval)))
      (($path $eval)
        (if (path-leo? $path)
          ($eval
            `(top-level-program
              ,@(syntax-case (leo-read-file $path) ()
                ((import body ...)
                  `(
                    ,(rewrite-import #'import)
                    ,@#'(body ...))))))
          (load-program $path $eval)))))
)
