(import
  (micascheme)
  (micalog domain)
  (micalog utils)
  (prefix (micalog keywords) %))

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
