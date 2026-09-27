(library (leo3 source-object)
  (export source-object-leo?)
  (import
    (scheme)
    (leo3 source-file-descriptor))

  (define (source-object-leo? $source)
    (source-file-descriptor-leo? (source-object-sfd $source)))
)
