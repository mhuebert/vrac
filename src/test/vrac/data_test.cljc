(ns vrac.data-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.data :as vd]))

(deftest normalized-elements-test
  (are [value result]
    (= (vd/normalized-elements #{:user/id
                                 :item/id
                                 :tag/id}
                               value)
       result)

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
     :user/known-for {:tag/id 1
                      :tag/name "Clojure Skills"}}
    [[:tag/id 1 #:tag{:id 1
                      :name "Clojure Skills"}]
     [:user/id 1 #:user{:id 1
                        :name "Johanna"
                        :known-for [:tag/id 1]}]]

    ; One to many entity relation via a vector
    {:user/id 1
     :user/name "Johanna"
     :user/belongings [{:item/id 1
                        :item/name "MacBook Air"}
                       {:item/id 2
                        :item/name "Umbrella"}]}
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
     :user/belongings #{{:item/id 1
                         :item/name "MacBook Air"}}}
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
     [nil nil {:a [:user/id 1], :b 7}]]))

(deftest assoc-db-test
  (are [db-before elements db-after]
    (= (reduce vd/assoc-db db-before elements) db-after)

    ;; Insert things in the DB
    {}
    [[:cow/id 1 #:cow {:id 1, :name "la noire", :age 2}]
     [:cow/id 2 #:cow {:id 2, :name "bella", :age 3}]]
    {:cow/id {1 #:cow {:id 1, :name "la noire", :age 2}
              2 #:cow {:id 2, :name "bella", :age 3}}}

    ;; Replace things in the DB
    {:cow/id {1 #:cow {:id 1, :name "la noire", :age 2}
              2 #:cow {:id 2, :name "bella", :age 3}}}
    [[:cow/id 1 #:cow {:id 1, :name "la noiraude"}]]
    {:cow/id {1 #:cow {:id 1, :name "la noiraude"}
              2 #:cow {:id 2, :name "bella", :age 3}}}))

(deftest dissoc-db-test
  (are [db-before elements db-after]
    (= (reduce vd/dissoc-db db-before elements) db-after)

    ;; Remove things in the DB
    {:cow/id {1 #:cow {:id 1, :name "la noire", :age 2}
              2 #:cow {:id 2, :name "bella", :age 3}
              3 #:cow {:id 3, :name "tulipe", :age 4}}}
    [[:cow/id 1 #:cow {:id 1, :name "la noire", :age 2}] ; the entity value is ignored
     [:cow/id 2]] ; it can be omitted
    {:cow/id {3 #:cow {:id 3, :name "tulipe", :age 4}}}

    ;; Remove remaining entities in the DB
    {:cow/id {3 #:cow {:id 3, :name "tulipe", :age 4}}}
    [[:cow/id 3]]
    {:cow/id {}}))

(deftest update-db-test
  ;; With the default merge function ...
  (are [db-before elements db-after]
    (= (reduce vd/update-db db-before elements) db-after)

    ;; Insert things in the DB
    {}
    [[:cow/id 1 #:cow {:id 1, :name "la noire", :age 2}]
     [:cow/id 2 #:cow {:id 2, :name "bella", :age 3}]]
    {:cow/id {1 #:cow {:id 1, :name "la noire", :age 2}
              2 #:cow {:id 2, :name "bella", :age 3}}}

    ;; Update things in the DB
    {:cow/id {1 #:cow {:id 1, :name "la noire", :age 2}
              2 #:cow {:id 2, :name "bella", :age 3}}}
    [[:cow/id 1 #:cow {:id 1, :name "la noiraude"}]]
    {:cow/id {1 #:cow {:id 1, :name "la noiraude", :age 2}
              2 #:cow {:id 2, :name "bella", :age 3}}})

  ;; With a custom merge function ...
  (are [db-before f-merge elements db-after]
    (= (reduce (partial vd/update-db f-merge) db-before elements) db-after)

    ;; Insert things in the DB
    {}
    (fn [cow new-cow]
      (select-keys new-cow [:cow/id :cow/name]))
    [[:cow/id 1 #:cow {:id 1, :name "la noire", :age 2}]
     [:cow/id 2 #:cow {:id 2, :name "bella", :age 3}]]
    {:cow/id {1 #:cow {:id 1, :name "la noire"}
              2 #:cow {:id 2, :name "bella"}}}

    ;; Replace things in the DB
    {:cow/id {1 #:cow {:id 1, :name "la noire", :age 2}
              2 #:cow {:id 2, :name "bella", :age 3}}}
    (fn [cow new-cow]
      (-> (merge cow new-cow)
          (assoc :cow/prev-name (:cow/name cow))))
    [[:cow/id 1 #:cow {:id 1, :name "la noiraude"}]]
    {:cow/id {1 #:cow {:id 1, :name "la noiraude", :age 2, :prev-name "la noire"}
              2 #:cow {:id 2, :name "bella", :age 3}}}))
