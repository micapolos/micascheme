(import (micascheme) (micalog core scope))

(parameterize ((gensym-count 0))
  (define (ref-equal? $a $b)
    (and
      (free-identifier=? (car $a) (car $b))
      (equal? (cdr $a) (cdr $b))))

  (define-rule-syntax (check-scope-ref-equal? scope id (id2 value))
    (check (ref-equal? (scope-ref scope id) (pair id2 value))))

  (check (raises (scope-ref (empty-scope) #'foo)))

  (lets
    ($scope
      (fluent (empty-scope)
        (scope+ #'zero "zero")
        (scope+ #'one "one")))
    (run
      (check-scope-ref-equal? $scope #'zero (#'zero "zero"))
      (check-scope-ref-equal? $scope #'one (#'one "one"))
      (check (raises (scope-ref $scope #'two)))
      (check (raises (scope+ #'zero "new zero")))
      (check (raises (scope+ #'one "new one"))))
    ($scope
      (fluent $scope
        (scope+gen #'zero "gen zero")
        (scope+gen #'two "gen two")))
    (run
      (check-scope-ref-equal? $scope #'zero (#'zero_0 "gen zero"))
      (check-scope-ref-equal? $scope #'one (#'one "one"))
      (check-scope-ref-equal? $scope #'two (#'two_1 "gen two"))
      (check (raises (scope+ $scope #'zero "new zero")))
      (check (raises (scope+ $scope #'one "new one")))
      (check (raises (scope+ $scope #'two "new two"))))
    ($scope (scope+ $scope #'three "three"))
    (run
      (check-scope-ref-equal? $scope #'zero (#'zero_0 "gen zero"))
      (check-scope-ref-equal? $scope #'one (#'one "one"))
      (check-scope-ref-equal? $scope #'two (#'two_1 "gen two"))
      (check-scope-ref-equal? $scope #'three (#'three "three")))
    ($scope (scope-commit $scope))
    (run
      (check (raises (scope+ $scope #'zero "new zero")))
      (check (raises (scope+ $scope #'one "new one")))
      (check (raises (scope+ $scope #'two "new two_1")))
      (check (raises (scope+ $scope #'three "new three"))))
    ($scope
      (fluent $scope
        (scope+gen #'zero "new gen zero")
        (scope+gen #'three "new gen three")
        (scope+ #'four "four")))
    (run
      (check-scope-ref-equal? $scope #'zero (#'zero_2 "new gen zero"))
      (check-scope-ref-equal? $scope #'one (#'one "one"))
      (check-scope-ref-equal? $scope #'two (#'two_1 "gen two"))
      (check-scope-ref-equal? $scope #'three (#'three_3 "new gen three"))
      (check-scope-ref-equal? $scope #'four (#'four "four")))))

