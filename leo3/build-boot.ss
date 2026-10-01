(import (scheme))
(compile-imported-libraries #t)
(make-boot-file "bin/leo3.boot" '("scheme") "leo3/boot.ss")
