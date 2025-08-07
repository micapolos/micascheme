(import (asm base) (asm identified))

(check (identified-identifier=? #'a (identified-with a 10)))
(check (not (identified-identifier=? #'a (identified-with b 10))))
(check (not (identified-identifier=? #'a (identified-with b #'a))))
