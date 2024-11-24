(import
  (micascheme)
  (micalog emu inits-updates)
  (prefix (micalog keywords) %))

(define-check-datum-> inits-updates-syntax)

(check-inits-updates-syntax
  (statement (%input 8 foo))
  (begin (%input 8 foo)))

(check-inits-updates-syntax
  (statement (%output 8 foo bar))
  (begin (%output 8 foo bar)))

(check-inits-updates-syntax
  (statement (%wire 8 foo bar))
  (begin (%wire 8 foo bar)))

(check-inits-updates-syntax
  (statement (%register 8 foo))
  (begin (%register 8 foo)))

(check-inits-updates-syntax
  (statement (%set 8 foo bar))
  (begin (%set 8 foo bar)))

(check-inits-updates-syntax
  (statement
    (%cond
      (foo
        (%register 8 foo-reg-8)
        (%wire 8 foo-wire-8))
      (%else
        (%register 8 else-reg-8)
        (%wire 8 else-wire-8))))
   (begin
     (%register 8 foo-reg-8)
     (%register 8 else-reg-8)
     (%cond
      (foo (%wire 8 foo-wire-8))
      (%else (%wire 8 else-wire-8)))))

(check-inits-updates-syntax
  (statement
    (%cond
      (foo
        (%register 8 foo-reg-8)
        (%wire 8 foo-wire-8))
      (bar
        (%register 8 bar-reg-8)
        (%wire 8 bar-wire-8))))
   (begin
     (%register 8 foo-reg-8)
     (%register 8 bar-reg-8)
     (%cond
      (foo (%wire 8 foo-wire-8))
      (bar (%wire 8 bar-wire-8)))))

(check-inits-updates-syntax
  (statement
    (%on clock
      (%posedge
        (%register 8 reg-8)
        (%wire 8 wire-8))))
   (begin
     (%register 8 reg-8)
     (%on clock
      (%posedge (%wire 8 wire-8)))))

(check-inits-updates-syntax
  (statement
    (%on clock
      (%posedge
        (%register 8 reg-8)
        (%wire 8 wire-8))
      (%else
        (%register 8 else-reg-8)
        (%wire 8 else-wire-8))))
   (begin
     (%register 8 reg-8)
     (%register 8 else-reg-8)
     (%on clock
      (%posedge (%wire 8 wire-8))
      (%else (%wire 8 else-wire-8)))))

(check-inits-updates-syntax
  (module
    (%module mod
      (%on clock
        (%posedge
          (%register 8 reg-8)
          foo bar))))
  (%module mod
    (%register 8 reg-8)
    (%on clock
      (%posedge foo bar))))
