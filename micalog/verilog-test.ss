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

(check-verilog (edge 0) %%negedge)
(check-verilog (edge 1) %%posedge)

(check-verilog (parameter (%input 1 foo)) (%%input foo))
(check-verilog (parameter (%input 16 foo)) (%%input (15 %%to 0) foo))

(check-verilog (parameter (%output 1 foo value)) (%%output foo))
(check-verilog (parameter (%output 16 foo value)) (%%output (15 %%to 0) foo))

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
    (%%always %%* (%%assign out-1 value-1))
    (%%always %%* (%%assign out-2 value-2))))

(check-verilog
  (module
    (%module
      (%internal 8 bar 12)))
  (module
    (micalog)
    (%%wire (7 %%to 0) bar)
    (%%always %%* (%%assign bar 12))))

(check-verilog
  (module
    (%module
      (%internal 8 foo (%register 16 15 clock 1 bar))))
  (module
    (micalog)
    (%%reg (7 %%to 0) foo 15)
    (%%always (%%posedge clock) (%%set! foo bar))))
