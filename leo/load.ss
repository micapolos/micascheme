(library (leo load)
  (export
    load-leo
    load-leo-program)
  (import
    (micascheme)
    (getter)
    (leo leo)
    (leo read)
    (leo expand)
    (leo getter))

  (define (load-leo $path)
    (leo (load $path)))

  ; FIXIT
  (define (load-leo-program $path)
    (lets
      ($sfd (path-source-file-descriptor $path))
      (call-with-port (open-source-file $sfd)
        (lambda ($port)
          (lets
            ($read (make-leo-read $port $sfd 0))
            (let $loop (($list '()))
              (switch ($read)
                ((eof? _)
                  (eval
                    (cons leo-expand `(top-level-program ,@(reverse $list)))))
                ((else $line)
                  ($loop (cons $line $list))))))))))
)
