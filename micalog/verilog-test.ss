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

(check-verilog (parameter (%input 1 foo)) (%%input foo))
(check-verilog (parameter (%input 16 foo)) (%%input (15 %%to 0) foo))

(check-verilog (parameter (%output 1 foo value)) (%%output foo))
(check-verilog (parameter (%output 16 foo value)) (%%output (15 %%to 0) foo))

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

(check-verilog (expr (%not 16 a)) (%%inv a))
(check-verilog (expr (%and 16 a b)) (%%and a b))
(check-verilog (expr (%or 16 a b)) (%%or a b))
(check-verilog (expr (%xor 16 a b)) (%%xor a b))
(check-verilog (expr (%nand 16 a b)) (%%nand a b))
(check-verilog (expr (%nor 16 a b)) (%%nor a b))
(check-verilog (expr (%xnor 16 a b)) (%%xnor a b))

(check-verilog (expr (%add 16 a b)) (%%+ a b))
(check-verilog (expr (%sub 16 a b)) (%%- a b))
(check-verilog (expr (%neg 16 a)) (%%- a))

(check-verilog
  (module
    (%module
      (%input 8 in1)
      (%input 8 in2)))
  (module
    (micalog
      (%%input (7 %%to 0) in1)
      (%%input (7 %%to 0) in2))))

(check-verilog
  (module
    (%module
      (%output 8 out-1 value-1)
      (%output 8 out-2 value-2)))
  (module
    (micalog
      (%%output (7 %%to 0) out-1)
      (%%output (7 %%to 0) out-2))
    (%%assign out-1 value-1)
    (%%assign out-2 value-2)))

(check-verilog
  (module
    (%module
      (%internal 8 bar 12)))
  (module
    (micalog)
    (%%wire (7 %%to 0) bar)
    (%%assign bar 12)))

(check-verilog
  (module
    (%module
      (%internal 8 foo (%register 16 (%init) (%on (%posedge clock) bar)))))
  (module
    (micalog)
    (%%reg (7 %%to 0) foo)
    (%%always (%%posedge clock) (%%set! foo bar))))

(check-verilog
  (module
    (%module
      (%internal 8 foo (%register 16 (%init 15) (%on (%posedge clock) bar)))))
  (module
    (micalog)
    (%%reg (7 %%to 0) foo 15)
    (%%always (%%posedge clock) (%%set! foo bar))))
