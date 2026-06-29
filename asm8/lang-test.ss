(import (scheme) (check) (asm8 lang))

(check
  (equal?
    (asm8 (u8.push 10))
    10))

(check
  (equal?
    (asm8
      (u8.push 10)
      u8.inc)
    11))

(check
  (equal?
    (asm8
      (u8.push 10)
      u8.dec)
    9))

(check
  (equal?
    (asm8
      (u8.push 10)
      (u8.push 20)
      u8.add)
    30))

(check
  (equal?
    (asm8
      (u8.push 30)
      (u8.push 20)
      u8.sub)
    10))

(check
  (equal?
    (asm8
      (u8.push 10)
      (u8.push 20)
      u8.mul)
    200))

