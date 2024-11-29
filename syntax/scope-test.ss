(import (micascheme) (syntax scope))

(check (false? (scope-ref (empty-scope) #'zero)))

(lets
  ($scope
    (fluent (empty-scope)
      (scope+ #'zero #'10)
      (scope+ #'one #'1)
      (scope+ #'zero #'0)))
  (run
    (check-datum=? (scope-ref $scope #'zero) #'0)
    (check-datum=? (scope-ref $scope #'one) #'1)
    (check (false? (scope-ref $scope #'two)))

    (check-datum=? (scope-item $scope #'zero) #'0)
    (check-datum=? (scope-item $scope #'one) #'1)
    (check (raises (scope-item $scope #'two)))

    (check (raises (scope+undefined $scope #'one 11)))))

(lets
  ($scope (scope-with (zero 0) (one 1)))
  (run
    (check-datum=? (scope-ref $scope #'zero) #'0)
    (check-datum=? (scope-ref $scope #'one) #'1)
    (check (false? (scope-ref $scope #'two)))))

(lets
  ($scope
    (scope-append
      (scope-with (zero 0) (one 11))
      (scope-with (two 2) (one 1))))
  (run
    (check-datum=? (scope-ref $scope #'zero) 0)))
