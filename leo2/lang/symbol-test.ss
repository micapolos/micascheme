(import
  (leo2 base)
  (leo2 lang symbol))

(check (equal? (symbol->index? '$1) 0))
(check (equal? (symbol->index? '$2) 1))
(check (equal? (symbol->index? '$123) 122))

(check (equal? (symbol->index? '$0) #f))
(check (equal? (symbol->index? 'foo) #f))
(check (equal? (symbol->index? '$-1) #f))
(check (equal? (symbol->index? '$3.14) #f))

(check (equal? (index->symbol 0) '$1))
(check (equal? (index->symbol 1) '$2))
(check (equal? (index->symbol 122) '$123))
