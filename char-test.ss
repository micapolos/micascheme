(import (scheme) (check) (char))

(check (equal? (char->ascii #\space) #x20))
(check (equal? (char->ascii (integer->char #xff)) #xff))
(check (equal? (char->ascii (integer->char #x1234)) #x34))

(check (char-newline? #\newline))
(check (not (char-newline? #\a)))

(check (char-space? #\space))
(check (not (char-space? #\a)))

(check (char=? (char colon) #\:))
(check (char=? (char dot) #\.))
(check (char=? (char at) #\@))
(check (char=? (char space) #\space))
(check (char=? (char a) #\a))
(check (char=? (char 0) #\0))
(check (char=? (char :) #\:))
