(import (micascheme) (leo char))

(check (char-letter? #\a))
(check (char-letter? #\z))
(check (char-letter? #\A))
(check (char-letter? #\Z))
(check (not (char-letter? #\1)))

(check (char-digit? #\0))
(check (char-digit? #\9))
(check (not (char-digit? #\a)))

(check (char-constituent? #\a))
(check (char-constituent? #\z))
(check (char-constituent? #\A))
(check (char-constituent? #\Z))
(check (not (char-constituent? #\space)))
(check (not (char-constituent? #\:)))
