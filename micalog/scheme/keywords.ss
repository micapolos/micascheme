(library (micalog scheme keywords)
  (export clock reset? exit?)
  (import (micascheme))

  (define clock 0)
  (define reset? 0)
  (define exit? 0)
)
