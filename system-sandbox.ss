(import (micascheme))

(display-current
  (lets
    ((current $seconds) current-seconds)
    ((current $file-string) (current-file-string "system-sandbox.ss"))
    (current
      (format
        "File content at ~,2f seconds...\n\n~a"
        $seconds
        $file-string))))
