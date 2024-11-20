(import
  (micascheme)
  (micalog model)
  (prefix (micalog keywords) %))

(check (syntax=? (type-size #'6) #'6))

(check (syntax=? (expr-type #'(%expr foo _)) #'foo))
(check (syntax=? (expr-value #'(%expr _ foo)) #'foo))

(check (syntax=? (reg-type #'(%reg 6)) #'6))

(check (syntax=? (process-edge #'(%posedge _ _)) #'%posedge))
(check (syntax=? (process-edge #'(%negedge _ _)) #'%negedge))

(check (opposite-edges? #'%posedge #'%negedge))
(check (opposite-edges? #'%negedge #'%posedge))
