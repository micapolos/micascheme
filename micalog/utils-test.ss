(import
  (micascheme)
  (micalog utils)
  (prefix (micalog keywords) %))

(check (syntax=? (type-size #'6) #'6))

(check (syntax=? (expr-type #'(%expr foo _)) #'foo))
(check (syntax=? (expr-value #'(%expr _ foo)) #'foo))

(check (syntax=? (reg-type #'(%reg 6)) #'6))

(check (syntax=? (process-edge #'(%posedge _ _)) #'%posedge))
(check (syntax=? (process-edge #'(%negedge _ _)) #'%negedge))

(check (opposite-edges? #'%posedge #'%negedge))
(check (opposite-edges? #'%negedge #'%posedge))

(check (raises (opposite-edges? #'%negedge #'%negedge)))
(check (raises (opposite-edges? #'%posedge #'%posedge)))

(check (opposite-processes? #'(%posedge _ _) #'(%negedge _ _)))
(check (opposite-processes? #'(%negedge _ _) #'(%posedge _ _)))
(check (raises (opposite-processes? #'(%posedge _ _) #'(%posedge _ _))))
(check (raises (opposite-processes? #'(%negedge _ _) #'(%negedge _ _))))

; === edge ===

(check (syntax=? (edge-identifier #'%posedge) #'%posedge))
(check (syntax=? (edge-identifier #'%negedge) #'%negedge))
(check (raises (edge-identifier #'foo)))

(check (edge=? #'%posedge #'%posedge))
(check (not (edge=? #'%posedge #'%negedge)))

(check (syntax=? (edge+? #'%posedge #'%posedge) #'%posedge))
(check (syntax=? (edge+? #'%negedge #'%negedge) #'%negedge))
(check (not (edge+? #'%posedge #'%negedge)))
(check (not (edge+? #'%negedge #'%posedge)))

; === edges+ ===

(check-datum=? (edges+ #'%posedge #'%posedge) #'%posedge)
(check-datum=? (edges+ #'%negedge #'%negedge) #'%negedge)
(check-datum=? (edges+ #'%posedge #'%negedge) #'%edge)
(check-datum=? (edges+ #'%negedge #'%posedge) #'%edge)
(check-datum=? (edges+ #'%edge #'%posedge) #'%edge)
(check-datum=? (edges+ #'%edge #'%negedge) #'%edge)
(check-datum=? (edges+ #'%posedge #'%edge) #'%edge)
(check-datum=? (edges+ #'%negedge #'%edge) #'%edge)

; === event+? ===

(check-datum=? (event+? #'(%posedge clock) #'(%posedge clock)) #'(%posedge clock))
(check-datum=? (event+? #'(%posedge clock) #'(%negedge clock)) #'(%edge clock))
(check (not (event+? #'(%posedge clock-1) #'(%posedge clock-2))))

; === domain+ ===

(check-datum=? (domain+ #'() #'()) #'())
(check-datum=? (domain+ #'() #'((%posedge clock))) #'())
(check-datum=? (domain+ #'((%posedge clock)) #'()) #'())

(check-datum=? (domain+ #'((%posedge clock)) #'((%posedge clock))) #'((%posedge clock)))
(check-datum=? (domain+ #'((%posedge clock)) #'((%negedge clock))) #'((%edge clock)))
(check-datum=? (domain+ #'((%posedge clock-1)) #'((%negedge clock-2))) #'())

(check-datum=?
  (domain+
    #'((%posedge clock-1) (%posedge clock-2))
    #'((%posedge clock-1)))
  #'((%posedge clock-1)))

(check-datum=?
  (domain+
    #'((%posedge clock-1) (%posedge clock-2))
    #'((%posedge clock-1) (%posedge clock-2)))
  #'((%posedge clock-1) (%posedge clock-2)))

(check-datum=?
  (domain+
    #'((%posedge clock-1) (%posedge clock-2))
    #'((%negedge clock-1) (%posedge clock-2)))
  #'((%edge clock-1)))
