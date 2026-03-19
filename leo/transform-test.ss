(import
  (except (micascheme) with from)
  (leo transform))

(check
  (equal?
    (syntax->datum (transform-name #'foo))
    (syntax->datum '(foo))))

(check
  (equal?
    (syntax->datum (transform-name #'(foo bar)))
    (syntax->datum '(foo bar))))

(check
  (equal?
    (syntax->datum (transform-name #'(foo (bar goo))))
    (syntax->datum '(foo bar goo))))

(check
  (equal?
    (syntax->datum (transform-import-spec #'foo))
    (syntax->datum '(foo))))

(check
  (equal?
    (syntax->datum (transform-import-spec #'(foo bar)))
    (syntax->datum '(foo bar))))

(check
  (equal?
    (syntax->datum (transform-import-spec #'(foo (bar goo))))
    (syntax->datum '(foo bar goo))))

(check
  (equal?
    (syntax->datum
      (transform-import-spec
        #'(rename
          (a %a)
          (b %b)
          (from (foo (bar goo))))))
    (syntax->datum
      '(rename
        (foo bar goo)
        (a %a)
        (b %b)))))

(check
  (equal?
    (syntax->datum
      (transform-import-spec
        #'(alias
          (a %a)
          (b %b)
          (from (foo (bar goo))))))
    (syntax->datum
      '(alias
        (foo bar goo)
        (a %a)
        (b %b)))))

(check
  (equal?
    (syntax->datum
      (transform-import-spec
        #'(only a b (from (foo (bar goo))))))
    (syntax->datum
      '(only (foo bar goo) a b))))

(check
  (equal?
    (syntax->datum
      (transform-import-spec
        #'(except a b (from (foo (bar goo))))))
    (syntax->datum
      '(except (foo bar goo) a b))))

(check
  (equal?
    (syntax->datum
      (transform-import-spec
        #'(prefix (% (from (foo (bar goo)))))))
    (syntax->datum
      '(prefix (foo bar goo) %))))

(check
  (equal?
    (syntax->datum
      (transform-import-spec
        #'(add-prefix (% (from (foo (bar goo)))))))
    (syntax->datum
      '(add-prefix (foo bar goo) %))))

(check
  (equal?
    (syntax->datum
      (transform-import-spec
        #'(drop-prefix (% (from (foo (bar goo)))))))
    (syntax->datum
      '(drop-prefix (foo bar goo) %))))

(check
  (equal?
    (syntax->datum
      (transform-export #'(export foo bar)))
    (syntax->datum
      '(export foo bar))))

(check
  (equal?
    (syntax->datum
      (transform-export
        #'(export
          (rename (foo bar) (goo gar)))))
    (syntax->datum
      '(export
        (rename
          (foo bar)
          (goo gar))))))

(check
  (equal?
    (syntax->datum
      (transform-export
        #'(export
          (import (foo (bar goo))))))
    (syntax->datum
      '(export
        (import
          (foo bar goo))))))

(check
  (equal?
    (syntax->datum
      (transform-import
        #'(import foo (foo bar) (foo (bar goo)))))
    (syntax->datum
      '(import (foo) (foo bar) (foo bar goo)))))

(check
  (equal?
    (syntax->datum
      (transform-library
        #'(library (foo (bar goo))
          (export foo bar)
          (import (zoo (zar zoo)))
          a b c)))
    (syntax->datum
      '(library
        (foo bar goo)
        (export foo bar)
        (import (zoo zar zoo))
        a b c))))
