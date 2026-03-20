(library (leo read)
  (export make-leo-read)
  (import
    (micascheme)
    (getter)
    (leo getter)
    (leo path)
    (leo expand)
    (leo source-file-descriptor))

  (define (make-leo-read $port $sfd $bfp)
    (lets
      ($line 0)
      ($column 0)
      ; Don't know why, but extracting this variable outside
      ; of lambda helped to avoid call-with-values warning
      ($getter
        (or-eof-getter
          (skip-until-getter
            char-newline?
            line-annotation-getter)))
      (lambda ()
        (lets
          ((values $value $new-bfp $new-line $new-column)
            (getter-get!
              $getter
              $port
              $sfd
              0 ; indent
              $bfp
              $line
              $column))
          (run
            (set! $bfp $new-bfp)
            (set! $line $new-line)
            (set! $column $new-column))
          $value))))
)
