(import (zasm keywords))

(check-zasm-bytevector (unreachable) (#x00))
(check-zasm-bytevector (nop) (#x01))
(check-zasm-bytevector (u8.const #x12) (#x02 #x12))

