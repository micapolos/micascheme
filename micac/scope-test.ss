(import (micascheme) (micac scope))

(lets
  ($scope (scope))
  ($scope (scope-alloc $scope #'foo))
  ($scope (scope-alloc $scope #'bar))
  (run
    (check
      (free-identifier=?
        (scope-transform $scope #'foo #'foo)
        (scope-transform $scope #'foo #'foo)))

    (check
      (not
        (free-identifier=?
          (scope-transform $scope #'foo #'foo)
          #'foo)))

    (check
      (not
        (free-identifier=?
          (scope-transform $scope #'foo #'foo)
          (scope-transform $scope #'bar #'bar))))))
