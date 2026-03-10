(library (leo read)
  (export leo-read)
  (import
    (micascheme)
    (getter)
    (leo getter)
    ; Force loading (leo lang), so it's available when reading leo files.
    (only (leo lang)))

  (define (leo-read $port $sfd? $ann? $bfp?)
    (lets
      ((values $value $bfp $line $column)
        (getter-get!
          (or-eof-getter (if $ann? line-annotation-getter line-getter))
          $port
          ; TODO: Make it work without it.
          (or $sfd? (source-file-descriptor "noname" 0))
          0
          (or $bfp? 0)
          0
          0))
      (values $value $bfp)))
)
