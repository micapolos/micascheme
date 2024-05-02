(library (emu math)
  (export
    fx->u8 fx->u16
    u8+ u8- u8+1 u8-1
    u16+ u16- u16+1 u16-1
    u8-233 u16-88
    u16-h u16-l)
  (import (scheme) (syntaxes))

  (define-rules-syntaxes
    ((fx->u8 $fx) (fxand $fx #xff))
    ((fx->u16 $fx) (fxand $fx #xffff))
    ((u8+ $a $b) (fx->u8 (fx+/wraparound $a $b)))
    ((u8- $a $b) (fx->u8 (fx-/wraparound $a $b)))
    ((u8+1 $x) (u8+ $x 1))
    ((u8-1 $x) (u8- $x 1))
    ((u16+ $a $b) (fx->u16 (fx+/wraparound $a $b)))
    ((u16- $a $b) (fx->u16 (fx-/wraparound $a $b)))
    ((u16+1 $x) (u16+ $x 1))
    ((u16-1 $x) (u16- $x 1))
    ((u16-h $u16) (fxsrl $u16 8))
    ((u16-l $u16) (fxand #xff))
    ((u8-233 $a $b $c) (fxior (fxsll $a 6) (fxsll $b 3) $c))
    ((u16-88 $h $l) (fxior (fxsll $h 8) $l)))
)
