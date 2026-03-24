(import (micascheme) (leo word))

(check (equal? (symbol->word 'quasiquote) "<<"))
(check (equal? (symbol->word 'unquote) ">>"))
(check (equal? (symbol->word 'unquote-splicing) ">>..."))
(check (equal? (symbol->word 'quote) "quote"))
(check (equal? (symbol->word 'other) "other"))
