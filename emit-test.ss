(import (scheme) (emit))

(check-emits
  (emit-u8 #x12)
  (#x12))

(check-emits
  (emit-u16-le #x1234)
  (#x34 #x12))

(check-emits
  (emit-u32-le #x12345678)
  (#x78 #x56 #x34 #x12))
