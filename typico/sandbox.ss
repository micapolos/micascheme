(import (only (scheme) parameterize current-expand) (typico lang) (typico environment))

(pretty-print (typico-expand '(+ "foo" "bar") (typico-environment)))
