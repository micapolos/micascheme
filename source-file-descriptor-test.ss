(import
  (chezscheme)
  (check)
  (source-file-descriptor))

(check
  (source-file-descriptor=?
    (source-file-descriptor "foo" 3)
    (source-file-descriptor "foo" 3)))

(check
  (not
    (source-file-descriptor=?
      (source-file-descriptor "foo" 3)
      (source-file-descriptor "bar" 3))))

(check
  (not
    (source-file-descriptor=?
      (source-file-descriptor "foo" 3)
      (source-file-descriptor "foo" 5))))

(check
  (source-file-descriptor?
    (path->source-file-descriptor "source-file-descriptor.ss")))
