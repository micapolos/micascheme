(import
  (leo2 base)
  (leo2 symbol))

(check (equal? (symbol->index? '$1) 0))
(check (equal? (symbol->index? '$2) 1))
(check (equal? (symbol->index? '$123) 122))

(check (equal? (symbol->index? '$0) #f))
(check (equal? (symbol->index? 'foo) #f))
(check (equal? (symbol->index? '$-1) #f))
(check (equal? (symbol->index? '$3.14) #f))
