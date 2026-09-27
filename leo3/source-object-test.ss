(import
  (scheme)
  (check)
  (boolean)
  (leo3 source-file-descriptor)
  (leo3 source-object))

(check (source-object-leo? (make-source-object (source-file-descriptor "foo.leo" 0) 0 0)))
(check (not (source-object-leo? (make-source-object (source-file-descriptor "foo.ss" 0) 0 0))))
