(import
  (only (micascheme) check equal? quote procedure? let)
  (micalang runtime-term))

(check (equal? bool (native 'bool)))
(check (equal? int (native 'int)))

(check (equal? (zero? 0) #t))
(check (equal? (zero? 1) #f))
(check
  (equal?
    (zero? (variable 1))
    (application (native zero?) (variable 1))))

(check (equal? (inc 1) 2))
(check
  (equal?
    (inc (variable 1))
    (application (native inc) (variable 1))))

(check (equal? (dec 2) 1))
(check
  (equal?
    (dec (variable 1))
    (application (native dec) (variable 1))))

(check (equal? ((= 2) 2) #t))
(check (equal? ((= 2) 3) #f))
(check
  (equal?
    ((= (variable 1)) 3)
    (application (application (native =) (variable 1)) 3)))
(check
  (equal?
    ((= 2) (variable 1))
    (application (application (native =) 2) (variable 1))))

(check (equal? ((+ 2) 3) 5))
(check
  (equal?
    ((+ (variable 1)) 3)
    (application (application (native +) (variable 1)) 3)))
(check
  (equal?
    ((+ 2) (variable 1))
    (application (application (native +) 2) (variable 1))))

(check (equal? ((- 5) 3) 2))
(check
  (equal?
    ((- (variable 1)) 3)
    (application (application (native -) (variable 1)) 3)))
(check
  (equal?
    ((- 2) (variable 1))
    (application (application (native -) 2) (variable 1))))

(check (equal? ((< 2) 3) #t))
(check
  (equal?
    ((< (variable 1)) 3)
    (application (application (native <) (variable 1)) 3)))
(check
  (equal?
    ((< 2) (variable 1))
    (application (application (native <) 2) (variable 1))))

(check (equal? (pi-param (pi (_ int) bool)) int))
(check (equal? ((pi (_ int) bool) '()) bool))

(check (equal? (pi-param (pi (x type) x)) type))
(check (equal? ((pi (x type) x) int) int))

(check (equal? (pi-param (pi (x type) (list x))) type))
(check (equal? ((pi (x type) (list x)) int) (list int)))
