(import
  (scheme)
  (string)
  (tata-8 expander))

(check-expands
  10
  "Integer.Constant(10)")

(check-expands
  "foo"
  "Text.Constant(\"foo\")")

(check-expands
  (image "quote.png")
  "Image.Resource(\"quote.png\")")

(check-expands
  (+ 10 20)
  "Integer.Apply2(Integer.Op2.ADD, Integer.Constant(10), Integer.Constant(20))")

(check-expands
  empty-drawing
  "Drawing.Empty")

(check-expands
  (filled-rectangle
    (position (x 10) (y 20))
    (size (width 30) (height 40)))
  "Drawing.Rect("
  "  Integer.Constant(10),"
  "  Integer.Constant(20),"
  "  Integer.Constant(30),"
  "  Integer.Constant(40))")
