(import (scheme) (check) (leo3 symbolizer))

(check (symbol=? (symbolize-dashed '(foo bar)) 'foo-bar))

(check (symbol=? (symbolize-as '(foo as bar)) 'foo->bar))
(check (symbol=? (symbolize-as '(foo goo as bar gar)) 'foo-goo->bar-gar))

(check (symbol=? (symbolize '(foo goo as bar gar)) 'foo-goo->bar-gar))
