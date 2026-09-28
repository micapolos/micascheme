(import (scheme) (check) (leo3 rewriter))

(check (equal? (rewrite-identifier 'foo) 'foo))
(check (raises (rewrite-identifier 123)))

(check (equal? (rewrite-identifier-pair '(foo bar)) '(foo bar)))
(check (raises (rewrite-identifier-pair '(foo 123))))

(check (equal? (rewrite-name '(name foo bar)) '(foo bar)))
(check (raises (rewrite-name '(nazwa foo bar))))
(check (raises (rewrite-name '(name foo 123))))

(check (equal? (rewrite-import-spec '(from foo bar)) '(foo bar)))
(check (equal? (rewrite-import-spec '(only x y (from foo bar))) '(only (foo bar) x y)))
(check (equal? (rewrite-import-spec '(except x y (from foo bar))) '(except (foo bar) x y)))
(check (equal? (rewrite-import-spec '(prefix foo-bar (from foo bar))) '(prefix (foo bar) foo-bar-)))
(check (equal? (rewrite-import-spec '(rename (a b) (c d) (from foo bar))) '(rename (foo bar) (a b) (c d))))

(check
  (equal?
    (rewrite-import '(import (from foo bar) (except x y (from zoo zar))))
    '(import
      (foo bar)
      (except (zoo zar) x y))))

(check
  (equal?
    (rewrite-export '(export x y))
    '(export x y)))
