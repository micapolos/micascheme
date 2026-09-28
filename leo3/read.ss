(library (leo3 read)
  (export read read-file)
  (import
    (except (scheme) read)
    (lets)
    (prefix (mica reader) %)
    (prefix (leo3 reader line) %))

  (define read
    (case-lambda
      (()
        (read (current-input-port)))
      (($port)
        (lets
          ((values $value $bfp)
            (%read-port-bfp
              (%one-of
                (%map %line-annotation annotation-stripped)
                %eof)
              $port
              (source-file-descriptor (port-name $port) 0)
              0))
          $value))))

  (define (read-file $path)
    (%read-file (%inline?-line-annotations #f) $path))
)
