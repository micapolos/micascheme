(import
  (micascheme)
  (micalog verilog)
  (prefix (micalog keywords) %)
  (prefix (verilog keywords) %%))

(define-case-syntax (check-verilog (name micalog) verilog)
  #`(check
    (equal?
      (syntax->datum
        (
          #,(identifier-append #'name #'name #'->verilog)
          #'micalog))
      'verilog)))

(check-verilog (name foo) foo)

(check-verilog (value foo) foo)
(check-verilog (value 128) 128)

(check-verilog (edge %posedge) %%posedge)
(check-verilog (edge %negedge) %%negedge)

(check-verilog (expr 10) 10)
(check-verilog (expr foo) foo)
(check-verilog (expr (%append 2 a 4 b)) (%%append a b))
(check-verilog (expr (%slice 3 a 5)) (%%ref a (7 %%to 5)))

(check-verilog (expr (%= 16 a b)) (%%= a b))
(check-verilog (expr (%!= 16 a b)) (%%!= a b))
(check-verilog (expr (%< 16 a b)) (%%< a b))
(check-verilog (expr (%<= 16 a b)) (%%<= a b))
(check-verilog (expr (%> 16 a b)) (%%> a b))
(check-verilog (expr (%>= 16 a b)) (%%>= a b))

(check-verilog (expr (%not 16 a)) (%%not a))
(check-verilog (expr (%and 16 a b)) (%%and a b))
(check-verilog (expr (%or 16 a b)) (%%or a b))
(check-verilog (expr (%xor 16 a b)) (%%xor a b))
(check-verilog (expr (%nand 16 a b)) (%%nand a b))
(check-verilog (expr (%nor 16 a b)) (%%nor a b))
(check-verilog (expr (%xnor 16 a b)) (%%xnor a b))

(check-verilog (expr (%+ 16 a b)) (%%+ a b))
(check-verilog (expr (%- 16 a b)) (%%- a b))
(check-verilog (expr (%- 16 a)) (%%- a))

(check-verilog (expr (%if 16 foo? a b)) (%%if foo? a b))

(check-verilog
  (expr (%if 16 (%not 1 foo?) (%and 16 a b) (%or 16 c (%not 16 d))))
  (%%if (%%not foo?) (%%and a b) (%%or c (%%not d))))

(check-verilog (input (%input 1 foo)) (%%input foo))
(check-verilog (input (%input 16 foo)) (%%input (15 %%to 0) foo))

(check-verilog (output (%output 1 foo)) (%%output foo))
(check-verilog (output (%output 16 foo)) (%%output (15 %%to 0) foo))

(check-verilog (declaration (%wire 1 foo)) (%%wire foo))
(check-verilog (declaration (%wire 8 foo)) (%%wire (7 %%to 0) foo))

(check-verilog (declaration (%register 1 foo)) (%%reg foo))
(check-verilog (declaration (%register 8 foo)) (%%reg (7 %%to 0) foo))

(check-verilog (declaration (%assign 1 foo bar)) (%%assign foo bar))
(check-verilog (declaration (%assign 8 foo bar)) (%%assign foo bar))

(check-verilog
  (declaration
    (%on clock
      (%posedge
        (%set 1 foo bar)
        (%set 2 goo gar))))
  (%%always (%%posedge clock)
    (%%set! foo bar)
    (%%set! goo gar)))

(check-verilog
  (instr (%set 1 foo bar))
  (%%set! foo bar))

(check-verilog
  (module
    (%module my-mod
      (%input 8 in1)
      (%input 8 in2)))
  (%%module
    (my-mod
      (%%input (7 %%to 0) in1)
      (%%input (7 %%to 0) in2))))

(check-verilog
  (module
    (%module my-mod
      (%wire 8 bar)
      (%assign 8 bar 12)))
  (%%module
    (my-mod)
    (%%wire (7 %%to 0) bar)
    (%%assign bar 12)))

(check-verilog
  (module
    (%module my-mod
      (%register 16 init-1)
      (%register 16 init-2)
      (%wire 16 update-1)
      (%assign 16 update-1 3)
      (%wire 16 update-2)
      (%assign 16 update-2 4)
      (%on clock
        (%posedge
          (%set 16 init-1 5)
          (%set 16 init-2 6)))))
  (%%module (my-mod)
    (%%reg (15 %%to 0) init-1)
    (%%reg (15 %%to 0) init-2)
    (%%wire (15 %%to 0) update-1)
    (%%assign update-1 3)
    (%%wire (15 %%to 0) update-2)
    (%%assign update-2 4)
    (%%always (%%posedge clock)
      (%%set! init-1 5)
      (%%set! init-2 6))))

(check-verilog
  (module
    (%module my-mod
      (%output 8 out-1)
      (%assign 8 out-1 value-1)
      (%output 8 out-2)
      (%assign 8 out-2 value-2)))
  (%%module
    (my-mod
      (%%output (7 %%to 0) out-1)
      (%%output (7 %%to 0) out-2))
    (%%assign out-1 value-1)
    (%%assign out-2 value-2)))
