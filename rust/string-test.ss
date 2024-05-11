(import (micascheme) (rust string))

(check (equal? (rust-expr-string #`(u8 10)) "10"))
(check (equal? (rust-expr-string #`(u8+1 (u8 10))) "10.wrapping_inc()"))
(check (equal? (rust-expr-string #`(u8-1 (u8 10))) "10.wrapping_dec()"))
(check (equal? (rust-expr-string #`(u8+ (u8 10) (u8 20))) "10.wrapping_add(20)"))
(check (equal? (rust-expr-string #`(u8- (u8 10) (u8 20))) "10.wrapping_sub(20)"))
