(import
  (scheme)
  (leo3 load)
  (leo3 path)
  (lets))

(library-extensions (cons '(".leo" . ".so") (library-extensions)))
(compile-imported-libraries #t)
(compile-library-handler
  (lets
    ($compile-library (compile-library-handler))
    (lambda ($src-path $obj-path)
      (if (path-leo? $src-path)
        (begin
          (when (compile-file-message)
            (printf "compilins ~a with output to ~a\n" $src-path $obj-path))
          (compile-to-file $src-path $obj-path))
        ($compile-library $src-path $obj-path)))))
(define-top-level-value 'load leo-load (interaction-environment))
(define-top-level-value 'load-program leo-load-program (interaction-environment))
(scheme-program
  (lets
    ($scheme-program (scheme-program))
    (lambda ($fn . $fns)
      (if (path-leo? $fn)
        (begin
          (command-line (cons $fn $fns))
          (command-line-arguments $fns)
          (leo-load-program $fn))
        (apply $scheme-program $fn $fns)))))
(scheme-script
  (lets
    ($scheme-script (scheme-script))
    (lambda ($fn . $fns)
      (if (path-leo? $fn)
        (begin
          (command-line (cons $fn $fns))
          (command-line-arguments $fns)
          (leo-load $fn))
        (apply $scheme-script $fn $fns)))))
(scheme-start
  (lets
    ($scheme-start (scheme-start))
    (lambda $fns
      (for-each leo-load $fns)
      ($scheme-start))))
