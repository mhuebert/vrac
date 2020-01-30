(ns vrac.data-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.data :as vd]))

(deftest normalizer-entity-test
  (let [normalize (vd/normalizer #{:user/id
                                   :item/id
                                   :tag/id})]
    (are [value result]
      (= (normalize value) result)

      ; Values that should stay unchanged.
      "hello"
      [[nil nil "hello"]]

      [5 "hello"]
      [[nil nil [5 "hello"]]]

      {5 "hello"}
      [[nil nil {5 "hello"}]]

      ; One to one entity relation
      {:user/id 1
       :user/name "Johanna"
       :user/known-for #:tag{:id 1
                             :name "Clojure Skills"}}
      [[:tag/id 1 #:tag{:id 1
                        :name "Clojure Skills"}]
       [:user/id 1 #:user{:id 1
                          :name "Johanna"
                          :known-for [:tag/id 1]}]]

      ; One to many entity relation via a vector
      {:user/id 1
       :user/name "Johanna"
       :user/belongings [#:item{:id 1
                                :name "MacBook Air"}
                         #:item{:id 2
                                :name "Umbrella"}]}
      [[:item/id 1 #:item{:id 1
                          :name "MacBook Air"}]
       [:item/id 2 #:item{:id 2
                          :name "Umbrella"}]
       [:user/id 1 #:user{:belongings [[:item/id 1]
                                       [:item/id 2]]
                          :id 1
                          :name "Johanna"}]]

      ; One to many entity relation via a set
      {:user/id 1
       :user/name "Johanna"
       :user/belongings #{#:item{:id 1
                                 :name "MacBook Air"}}}
      [[:item/id 1 #:item{:id 1
                          :name "MacBook Air"}]
       [:user/id 1 #:user{:belongings #{[:item/id 1]}
                          :id 1
                          :name "Johanna"}]]

      ; Vector of mixed stuffs
      [{:user/id 1
        :user/name "Johanna"}
       3]
      [[:user/id 1 #:user{:id 1
                          :name "Johanna"}]
       [nil nil [[:user/id 1] 3]]]

      ; map of mixed stuffs
      {:a {:user/id 1
           :user/name "Johanna"}
       :b 7}
      [[:user/id 1 #:user{:id 1
                          :name "Johanna"}]
       [nil nil {:a [:user/id 1], :b 7}]])))

(comment
  (deftest db-assoc-test
    (are [db-before elements db-after]
      (= (reduce (fn [db [table id entity]]
                   (vd/db-assoc db [table id] entity))
                 db-before
                 elements)
         db-after)

      {}
      [[:cow/id 1 #:cow{:id 1, :name "la noire", :age 2}]
       [:cow/id 2 #:cow{:id 2, :name "bella", :age 3}]]
      {:cow/id {1 #:cow{:id 1, :name "la noire", :age 2}
                2 #:cow{:id 2, :name "bella", :age 3}}}

      {:cow/id {1 #:cow{:id 1, :name "la noire", :age 2}
                2 #:cow{:id 2, :name "bella", :age 3}}}
      [[:cow/id 1 #:cow{:id 1, :name "la noiraude", :age 2}]]
      {:cow/id {1 #:cow{:id 1, :name "la noiraude", :age 2}
                2 #:cow{:id 2, :name "bella", :age 3}}}))

  (deftest db-update-test
    (are [db-before idents db-after]
      (= (reduce (fn [db ident]
                   (vd/db-update db ident update :cow/age + 5))
                 db-before
                 idents)
         db-after)

      ;; Update things in the DB, add 5 years to the specified cows.
      {:cow/id {1 #:cow{:id 1, :name "la noire", :age 2}
                2 #:cow{:id 2, :name "bella", :age 3}
                3 #:cow{:id 3, :name "tulipe", :age 4}}}
      [[:cow/id 1]
       [:cow/id 2]]
      {:cow/id {1 #:cow{:id 1, :name "la noire", :age 7}
                2 #:cow{:id 2, :name "bella", :age 8}
                3 #:cow{:id 3, :name "tulipe", :age 4}}})))

(deftest ident?-test
  (is (vd/ident? ^:ident [:cow/id 1]))
  (is (not (vd/ident? [:cow/id 1]))))

(deftest denormalize-entity-test
  (are [db ident result]
    (= (vd/denormalize-entity db (get-in db ident))
       result)

    {:user/id {1 #:user{:id 1
                        :name "Johanna"
                        :known-for ^:ident [:tag/id 1]
                        :belongings [^:ident [:item/id 1]
                                     ^:ident [:item/id 2]]}}
     :item/id {1 #:item{:id 1
                        :name "MacBook Air"}
               2 #:item{:id 2
                        :name "Umbrella"}}
     :tag/id {1 #:tag{:id 1
                      :name "Clojure Skills"}}}
    [:user/id 1]
    {:user/id 1
     :user/name "Johanna"
     :user/known-for #:tag{:id 1
                           :name "Clojure Skills"}
     :user/belongings [#:item{:id 1
                              :name "MacBook Air"}
                       #:item{:id 2
                              :name "Umbrella"}]}))
