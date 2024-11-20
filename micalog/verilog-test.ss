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

(check-verilog (event (%posedge foo)) (%%posedge foo))
(check-verilog (event (%negedge 123)) (%%negedge 123))

(check-verilog (expr 10) 10)
(check-verilog (expr foo) foo)
(check-verilog (expr (%append 0)) (%%append))
(check-verilog (expr (%append 16 a b c)) (%%append a b c))
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

(check-verilog (expr (%add 16 a b)) (%%+ a b))
(check-verilog (expr (%sub 16 a b)) (%%- a b))
(check-verilog (expr (%neg 16 a)) (%%- a))

(check-verilog (expr (%if 16 foo? a b)) (%%if foo? a b))

(check-verilog (input (foo 1)) (%%input foo))
(check-verilog (input (foo 16)) (%%input (15 %%to 0) foo))

(check-verilog (output (foo 1 value)) (%%output foo))
(check-verilog (output (foo 16 value)) (%%output (15 %%to 0) foo))

(check-verilog (init (%register 1 foo)) (%%reg foo))
(check-verilog (init (%register 8 foo)) (%%reg (7 %%to 0) foo))

(check-verilog
  (declaration-declarations
    (%on clock
      (%posedge
        (%init
          (%register 1 pos-init-1)
          (%register 1 pos-init-2))
        (%update
          (%wire 1 pos-update 3)))
      (%negedge
        (%init
          (%register 1 neg-init-1)
          (%register 1 neg-init-2))
        (%update
          (%wire 1 neg-update 6)
          (%on sub-clock
            (%posedge
              (%init
                (%register 1 sub-init))
              (%update
                (%wire 1 sub-update 8))))))))
  (
    (%%reg pos-init-1)
    (%%reg pos-init-2)
    (%%wire pos-update)
    (%%assign pos-update 3)
    (%%reg neg-init-1)
    (%%reg neg-init-2)
    (%%wire neg-update)
    (%%assign neg-update 6)
    (%%reg sub-init)
    (%%wire sub-update)
    (%%assign sub-update 8)))

(check-verilog
  (declaration-instrs
    (%on clock
      (%posedge
        (%init
          (%register 1 pos-1)
          (%register 1 pos-2))
        (%update
          (%set 1 pos-1 3)
          (%set 1 pos-1 4)))
      (%negedge
        (%init
          (%register 1 neg-1)
          (%register 1 neg-2))
        (%update
          (%set 1 neg-1 7)
          (%set 1 neg-2 8)
          (%on sub-clock
            (%posedge
              (%init
                (%register 1 sub))
              (%update
                (%set 1 sub 10))))))))
  (
    (%%always (%%posedge clock)
      (%%set! pos-1 3)
      (%%set! pos-1 4))
    (%%always (%%negedge clock)
      (%%set! neg-1 7)
      (%%set! neg-2 8))))

(check-verilog
  (module
    (%module my-mod
      (%input
        (in1 8)
        (in2 8))
      (%internal)
      (%output)))
  (%%module
    (my-mod
      (%%input (7 %%to 0) in1)
      (%%input (7 %%to 0) in2))))

(check-verilog
  (module
    (%module my-mod
      (%input)
      (%internal (%wire 8 bar 12))
      (%output)))
  (%%module
    (my-mod)
    (%%wire (7 %%to 0) bar)
    (%%assign bar 12)))

(check-verilog
  (module
    (%module my-mod
      (%input)
      (%internal (%wire 8 foo 0))
      (%output)))
  (%%module
    (my-mod)
    (%%wire (7 %%to 0) foo)
    (%%assign foo 0)))

(check-verilog
  (module
    (%module my-mod
      (%input)
      (%internal
        (%on clock
          (%posedge
            (%init
              (%register 16 init-1)
              (%register 16 init-2))
            (%update
              (%wire 16 update-1 3)
              (%wire 16 update-2 4)
              (%set 16 init-1 5)
              (%set 16 init-2 6)))))
      (%output)))
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
      (%input)
      (%internal)
      (%output
        (out-1 8 value-1)
        (out-2 8 value-2))))
  (%%module
    (my-mod
      (%%output (7 %%to 0) out-1)
      (%%output (7 %%to 0) out-2))
    (%%assign out-1 value-1)
    (%%assign out-2 value-2)))
