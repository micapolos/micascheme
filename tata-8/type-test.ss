(import
  (scheme)
  (tata-8 type))

(check-tata
  10
  "Integer.Constant(10)")

(check-tata
  (+ 10 20)
  "Integer.Apply2(Integer.Op2.ADD, Integer.Constant(10), Integer.Constant(20))")

(check-tata
  empty-drawing
  "Drawing.Empty")

(check-tata
  (filled-rectangle 10 20 30 40)
  "Drawing.Rect(Integer.Constant(10), Integer.Constant(20), Integer.Constant(30), Integer.Constant(40))")

(display (tata-program-string (filled-rectangle 10 20 30 40)))
