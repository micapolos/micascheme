(import
  (scheme)
  (check)
  (boolean)
  (leo3 source-file-descriptor))

(check (source-file-descriptor-leo? (source-file-descriptor "foo.leo" 0)))
(check (not (source-file-descriptor-leo? (source-file-descriptor "foo.ss" 0))))
