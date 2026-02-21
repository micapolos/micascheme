(library (union)
  (export union)
  (import
    (scheme)
    (lets)
    (identifier)
    (switch)
    (throw)
    (list-syntax))

  (define-syntax (union $syntax)
    (syntax-case $syntax ()
      ((_ (name ids ...))
        (lets
          ($ids
            #'(ids ...))
          ($predicate
            (identifier-append #'name #'name #'?))
          ($id-predicates
            (map-with ($id $ids)
              (identifier-append #'name $id #'?)))
          ($switch (identifier-append #'name #'name #'- #'switch))
          ($case-ids
            (map-with ($id $ids)
              (identifier-append #'name #'$ $id)))
          ($case-bodies
            (map-with ($id $ids)
              (identifier-append #'name $id #'- #'body)))
          ($not-name (identifier-append #'name #'not- #'name))
          #`(begin
            (define (#,$predicate $union)
              (or
                #,@(map-with ($id-predicate $id-predicates)
                  #`(#,$id-predicate $union))))
            (define-syntax #,$switch
              (syntax-rules (#,@$id-predicates)
                ((_ x
                  #,@(map-with
                    ($id-predicate $id-predicates)
                    ($case-id $case-ids)
                    ($case-body $case-bodies)
                    #`((#,$id-predicate #,$case-id) #,$case-body)))
                  (switch x
                    #,@(map-with
                      ($id-predicate $id-predicates)
                      ($case-id $case-ids)
                      ($case-body $case-bodies)
                      #`((#,$id-predicate #,$case-id) #,$case-body))
                    ((else $other)
                      (throw #,$not-name $other)))))))))))
)
