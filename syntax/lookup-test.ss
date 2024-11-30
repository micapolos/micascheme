(import (micascheme) (syntax lookup))

(check (false? (lookup-ref (empty-lookup) #'zero)))

(lets
  ($scope
    (fluent (empty-lookup)
      (lookup+ #'zero #'10)
      (lookup+ #'one #'1)
      (lookup+ #'zero #'0)))
  (run
    (check-datum=? (lookup-ref $scope #'zero) #'0)
    (check-datum=? (lookup-ref $scope #'one) #'1)
    (check (false? (lookup-ref $scope #'two)))

    (check-datum=? (lookup-value $scope #'zero) #'0)
    (check-datum=? (lookup-value $scope #'one) #'1)
    (check (raises (lookup-value $scope #'two)))

    (check (raises (lookup+undefined $scope #'one 11)))))

(lets
  ($scope (lookup-with (zero 0) (one 1)))
  (run
    (check-datum=? (lookup-ref $scope #'zero) #'0)
    (check-datum=? (lookup-ref $scope #'one) #'1)
    (check (false? (lookup-ref $scope #'two)))))

(lets
  ($scope
    (lookup-append
      (lookup-with (zero 0) (one 11))
      (lookup-with (two 2) (one 1))))
  (run
    (check-datum=? (lookup-ref $scope #'zero) 0)))
