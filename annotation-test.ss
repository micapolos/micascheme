(import (scheme) (check) (annotation))

(check (annotation? (datum/annotation 123)))

(check
  (equal?
    (annotation-stripped (datum/annotation (foo bar)))
    '(foo bar)))
