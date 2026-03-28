(library (leo read)
  (export
    leo-read-annotation
    leo-read
    make-leo-read)
  (import
    (micascheme)
    (getter)
    (leo getter)
    (leo path)
    (prefix (mica reader) %)
    (prefix (leo mica reader single-line) %)
    (leo expand)
    (leo source-file-descriptor))

  (define (leo-read-annotation $port $sfd $bfp)
    (%read-port-bfp (%or-eof %single-line-annotation) $port $sfd $bfp))

  (define (leo-read $port)
    (lets
      ((values $datum/eof $bfp)
        (leo-read-annotation $port (source-file-descriptor "test.leo" 0) 0))
      (datum/annotation-stripped $datum/eof)))

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
