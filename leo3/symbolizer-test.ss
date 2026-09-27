(import (scheme) (check) (leo3 symbolizer))

(check (symbol=? (symbolize-dashed '(foo bar)) 'foo-bar))

(check (symbol=? (symbolize-as '(foo as bar)) 'foo->bar))
(check (symbol=? (symbolize-as '(foo goo as bar gar)) 'foo-goo->bar-gar))

(check (symbol=? (symbolize-is '(is foo)) 'foo?))
(check (symbol=? (symbolize-is '(is foo bar)) 'foo-bar?))
(check (symbol=? (symbolize-is '(is is foo bar)) 'foo-bar??))

(check (symbol=? (symbolize '(is foo goo as is bar gar)) 'foo-goo?->bar-gar?))
