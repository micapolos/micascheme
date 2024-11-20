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

(check-verilog (init (%register foo 1)) (%%reg foo))
(check-verilog (init (%register foo 8)) (%%reg (7 %%to 0) foo))

(check-verilog
  (init-names
    (clock
      (%on
        (%posedge
          (%init
            (%register pos-init-1 1)
            (%register pos-init-2 1))
          (%update
            (pos-update 1 0)))
        (%negedge
          (%init
            (%register neg-init-1 1)
            (%register neg-init-2 1))
          (%update
            (neg-update 1 0)
            (sub-clock
              (%on
                (%posedge
                  (%init
                    (%register sub-init 1))
                  (%update
                    (sub-update 1 0))))))))))
  (pos-init-1 pos-init-2 neg-init-1 neg-init-2 sub-init))

(check-verilog
  (declaration-declarations
    (clock
      (%on
        (%posedge
          (%init
            (%register pos-init-1 1)
            (%register pos-init-2 1))
          (%update
            (pos-update 1 3)))
        (%negedge
          (%init
            (%register neg-init-1 1)
            (%register neg-init-2 1))
          (%update
            (neg-update 1 6)
            (sub-clock
              (%on
                (%posedge
                  (%init
                    (%register sub-init 1))
                  (%update
                    (sub-update 1 8))))))))))
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
    (clock
      (%on
        (%posedge
          (%init
            (%register pos-1 1)
            (%register pos-2 1))
          (%update
            (pos-1 1 3)
            (pos-1 1 4)))
        (%negedge
          (%init
            (%register neg-1 1)
            (%register neg-2 1))
          (%update
            (neg-1 1 7)
            (neg-2 1 8)
            (sub-clock
              (%on
                (%posedge
                  (%init
                    (%register sub 1))
                  (%update
                    (sub 1 10))))))))))
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
      (%internal (bar 8 12))
      (%output)))
  (%%module
    (my-mod)
    (%%wire (7 %%to 0) bar)
    (%%assign bar 12)))

(check-verilog
  (module
    (%module my-mod
      (%input)
      (%internal (foo 8 0))
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
        (clock
          (%on
            (%posedge
              (%init
                (%register init-1 16)
                (%register init-2 16))
              (%update
                (update-1 16 3)
                (update-2 16 4)
                (init-1 16 5)
                (init-2 16 6))))))
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
