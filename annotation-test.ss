(import (scheme) (check) (annotation))

(check (annotation? (datum/annotation 123)))

(check
  (equal?
    (annotation-stripped (datum/annotation (foo bar)))
    '(foo bar)))

(check
  (annotation?
    (append-annotation
      (datum/annotation 10)
      (datum/annotation 20)
      (datum/annotation 30))))
