(ns vrac.util-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.util :as vu]))

(deftest tag-id-class-test
  (are [kw result]
    (= (#'vu/tag-id-class kw) result)

    :. {:tag :div
        :id nil
        :class []}

    :.class1 {:tag :div
              :id nil
              :class ["class1"]}

    :#the-one {:tag :div
               :id "the-one"
               :class []}

    :h1.class1#the-one.class2 {:tag :h1
                               :id "the-one"
                               :class ["class1" "class2"]}

    :todo/list.class1#the-one.class2 {:tag :todo/list
                                      :id "the-one"
                                      :class ["class1" "class2"]}

    :my.todo/list.class1#the-one.class2 {:tag :my.todo/list
                                         :id "the-one"
                                         :class ["class1" "class2"]}))

(deftest merge-props-test
  (are [id class props result]
    (= (#'vu/merge-props id class props) result)

    nil [] {}
    {:id nil
     :className nil
     :style nil}

    "app" [] {}
    {:id "app"
     :className nil
     :style nil}

    "app" [] {:id "the-one"}
    {:id "the-one"
     :className nil
     :style nil}

    "app" ["green" "big"] {:class "red"}
    {:id "app"
     :className "green big red"
     :style nil}

    "app" ["green" "big"] {:class ["red" "yellow"]}
    {:id "app"
     :className "green big red yellow"
     :style nil}

    "app" [] {:style {:background-color "blue"}}
    {:id "app"
     :className nil
     :style {:backgroundColor "blue"}}))
