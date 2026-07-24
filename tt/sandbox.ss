(typed
  (lambda? number? ... number?)
  +)

(typed
  (case-lambda?
    ((explicit element?) (list? element?))
    ((implicit element?) element? element? ... (list? element?)))
  list)

(typed (lambda? number number point?) point)
(typed (lambda? point? number) point-x)
(typed (lambda? point? number) point-y)

(typed (lambda? number number rectangle?) rectangle)
(typed (lambda? rectangle? number) rectangle-width)
(typed (lambda? rectangle? number) rectangle-height)

(typed (lambda? number circle?) circle)
(typed (lambda? circle? number) circle-radius)

(typed (lambda? rectangle? shape?) rectangle-shape)
(typed (lambda? circle? shape?) circle-shape)
(typed
  (lambda?
    (implicit result?)
    shape?
    (lambda rectangle? result?)
    (lambda circle? result?))
  shape-switch)

(shape-switch
  (lambda ($rectangle rectangle?)
    `(rectangle
      (width ,(rectangle-width $rectangle))
      (height ,(rectangle-height $rectangle))))
  (lambda ($circle circle?)
    `(circle
      (radius
        ,(circle-radius $circle)))))
