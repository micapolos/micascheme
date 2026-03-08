(import (chezscheme) (check) (source-object))

(check
  (source-object=?
    (make-source-object
      (source-file-descriptor "foo" 3)
      10 20)
    (make-source-object
      (source-file-descriptor "foo" 3)
      10 20)))

(check
  (not
    (source-object=?
      (make-source-object
        (source-file-descriptor "bar" 3)
        10 20)
      (make-source-object
        (source-file-descriptor "foo" 3)
        10 20))))

(check
  (not
    (source-object=?
      (make-source-object
        (source-file-descriptor "foo" 3)
        10 20)
      (make-source-object
        (source-file-descriptor "foo" 3)
        15 20))))

(check
  (not
    (source-object=?
      (make-source-object
        (source-file-descriptor "foo" 3)
        10 20)
      (make-source-object
        (source-file-descriptor "foo" 3)
        10 30))))

(check
  (source-object=?
    (append-source-object
      (make-source-object
        (source-file-descriptor "foo" 3)
        10 20)
      (make-source-object
        (source-file-descriptor "foo" 3)
        30 40)
      (make-source-object
        (source-file-descriptor "foo" 3)
        5 30))
    (make-source-object
        (source-file-descriptor "foo" 3)
        5 40)))
