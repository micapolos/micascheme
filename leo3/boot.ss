(import
  (scheme)
  (leo3 load)
  (leo3 path)
  (leo3 read)
  (leo3 rewriter)
  (lets))

(library-extensions (cons '(".leo" . ".so") (library-extensions)))
(library-search-handler
  (lets
    ($library-search (library-search-handler))
    (lambda ($who $lib $dirs $exts)
      (lets
        ((values $src-path $obj-path $obj-found?)
          ($library-search $who $lib $dirs $exts))
        (if
          (and
            $src-path
            (not $obj-found?)
            (not (compile-imported-libraries))
            (path-leo? $src-path))
          (begin
            (leo-load $src-path)
            (values "/dev/null" #f #f))
          (values $src-path $obj-path $obj-found?))))))
(compile-library-handler
  (lets
    ($compile-library (compile-library-handler))
    (lambda ($src-path $obj-path)
      (if (path-leo? $src-path)
        (begin
          (when (compile-file-message)
            (printf "compiling ~a with output to ~a\n" $src-path $obj-path))
          (compile-to-file
            (map rewrite-library (read-file $src-path))
            $obj-path))
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
