(library (leo read)
  (export
    leo-read
    make-leo-read)
  (import
    (micascheme)
    (getter)
    (leo getter)
    (leo path)
    (only (mica reader) read-port)
    (only (leo mica reader single-line) single-line-annotation)
    (leo expand)
    (leo source-file-descriptor))

  (define (leo-read $port $sfd $bfp)
    (read-port single-line-annotation $port $sfd $bfp))

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
