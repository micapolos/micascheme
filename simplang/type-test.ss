(import (micascheme) (simplang type))

(check (type=? #'boolean #'boolean))
(check (type=? #'integer #'integer))
(check (type=? #'char #'char))
(check (type=? #'string #'string))

(check (not (type=? #'integer #'char)))

(check (type=? #'(arrow (integer string) boolean) #'(arrow (integer string) boolean)))
(check (not (type=? #'(arrow (integer string) boolean) #'(arrow (integer) boolean))))
(check (not (type=? #'(arrow (integer string) boolean) #'(arrow (integer string) char))))

(check (type=? #`(macro #,+) #`(macro #,+)))
(check (not (type=? #`(macro #,+) #`(macro #,-))))
