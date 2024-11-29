(import (micascheme) (micalog core scope))

(check (raises (scope-value (empty-scope) #'foo)))

(lets
  ($scope
    (fluent (empty-scope)
      (scope+ #'zero 0)
      (scope+ #'one 1)))
  (run
    (check-equal? (scope-value $scope #'zero) 0)
    (check-equal? (scope-value $scope #'one) 1)
    (check (raises (scope-value $scope #'two)))
    (check (raises (scope+ #'one 12))))
  ((pair $scope $gen-zero)
    (scope+gen $scope #'zero "zero"))
  (run
    (check (equal? (scope-value $scope #'zero) "zero"))
    (check (equal? (scope-value $scope $gen-zero) "zero"))))
