(library (leo3 source-file-descriptor)
  (export source-file-descriptor-leo?)
  (import
    (scheme)
    (source-file-descriptor)
    (leo3 path))

  (define (source-file-descriptor-leo? $sfd)
    (path-leo? (source-file-descriptor-path $sfd)))
)
