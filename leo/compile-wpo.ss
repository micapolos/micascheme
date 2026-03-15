(import (chezscheme))

(optimize-level 3)
(compile-imported-libraries #t)
(generate-wpo-files #t)

(pretty-print (compile-program "leo/main.ss" "dist/lib/leo.so"))
(pretty-print (compile-whole-program "dist/lib/leo.wpo" "dist/lib/leo" #t))
