(import (micascheme))

(display-current
  (lets
    (in current
      ($seconds current-seconds)
      ($file-string (current-file-string "system-sandbox.ss"))
      (current
        (format
          "File content at ~,2f seconds...\n\n~a"
          $seconds
          $file-string)))))
