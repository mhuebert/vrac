(ns vrac.core-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.core :as v :refer [template->ast
                                     get-template-props
                                     get-queries
                                     render]]))

(deftest get-template-props-test
  (are [template props]
    (= (get-template-props (template->ast template)) props)

    nil
    #{}

    'a
    '#{a}

    '(:a nil)
    '#{}

    '[:p a]
    '#{a}

    '(if a b c)
    '#{a b c}

    '(when a b)
    '#{a b}

    '(let [a b, c d] e)
    '#{b d e}

    '(for [a b, c d] e)
    '#{b d e}

    '(let [a b, c a] [:p a b c d])
    '#{b d}))

(deftest get-deps-test
  (are [template deps]
    (= (#'v/get-deps {} {} (template->ast template)) deps)

    nil
    [[]]

    'ctx
    '[[ctx]]

    '(:1 nil)
    '[[:1]]

    '(:1 ctx)
    '[[ctx :1]]

    '(:2 (:1 ctx))
    '[[ctx :1 :2]]

    '(:3 (:2 (:1 ctx)))
    '[[ctx :1 :2 :3]]

    '(if (:1 ctx1)
       [:p 1]
       [:p (:2 ctx1)])
    '[[ctx1 :1]
      [ctx1 :2]]

    '(if (:1 ctx1)
       (:2 ctx1)
       (:3 ctx1))
    '[[ctx1 :1]
      [ctx1 :2]
      [ctx1 :3]]

    '[:p (:1 ctx1) (:2 ctx1)]
    '[[ctx1 :1]
      [ctx1 :2]]

    '[:p (:1 ctx1) (:2 ctx2)]
    '[[ctx1 :1]
      [ctx2 :2]]

    '(let [var1 (:1 ctx1)
           var2 (:2 ctx2)
           var3 (:3 var1)]
       [:p (:4 var1) (:5 var2) (:6 var3)])
    '[[ctx1 :1 :4]
      [ctx2 :2 :5]
      [ctx1 :1 :3 :6]])

  (let [components {::item {:parsed-template
                            (template->ast '[:li (:item/name item)])}
                    ::list {:parsed-template
                            (template->ast '[:ul
                                             (for [item (:list/items list)]
                                               [::item {:item item}])])}}
        env {:components components}]
    (are [component-id deps]
      (= (#'v/get-deps env {} (-> env :components component-id :parsed-template)) deps)

      ::item '[[item :item/name]]
      ::list '[[list :list/items :item/name]])))

(deftest deps->eql-test
  (are [deps eql-queries]
    (= (#'v/deps->eql deps) eql-queries)

    []
    []

    [[]]
    []

    [[] [:1] []]
    [:1]

    [[:1]]
    [:1]

    '[[ctx]]
    '[ctx]

    '[[ctx :1]]
    '[{ctx [:1]}]

    '[[ctx :1 :2]]
    '[{ctx [{:1 [:2]}]}]

    '[[ctx :1 :2 :3]]
    '[{ctx [{:1 [{:2 [:3]}]}]}]

    '[[ctx :1]
      [ctx :2]
      [ctx :3 :4]]
    '[{ctx [:1 :2 {:3 [:4]}]}]

    '[[ctx0 :0 :2 :3]
      [ctx1 :1 :2 :3]]
    '[{ctx0 [{:0 [{:2 [:3]}]}]}
      {ctx1 [{:1 [{:2 [:3]}]}]}]

    '[[ctx1 :1 :4]
      [ctx2 :2 :5]
      [ctx1 :1 :3 :6]]
    '[{ctx1 [{:1 [:4
                  {:3 [:6]}]}]}
      {ctx2 [{:2 [:5]}]}]))


(deftest get-queries-test
  (are [template eql-queries]
    (let [env {:components {:comp {:parsed-template (template->ast template)}}}]
      (= (get-queries env :comp) eql-queries))

    nil
    []

    '(:1 nil)
    '[:1]

    '(:1 ctx)
    '[{ctx [:1]}]

    '(:2 (:1 nil))
    '[{:1 [:2]}]

    '(:2 (:1 ctx))
    '[{ctx [{:1 [:2]}]}]

    '(:3 (:2 (:1 ctx)))
    '[{ctx [{:1 [{:2 [:3]}]}]}]

    '(if (:1 ctx1)
       [:p 1]
       [:p (:2 ctx1)])
    '[{ctx1 [:1 :2]}]

    '(if (:1 ctx1)
       (:2 ctx1)
       (:3 ctx1))
    '[{ctx1 [:1 :2 :3]}]

    '[:p (:1 ctx1) (:2 ctx1)]
    '[{ctx1 [:1 :2]}]

    '[:p (:1 ctx1) (:2 ctx2)]
    '[{ctx1 [:1]}
      {ctx2 [:2]}]

    '(let [var1 (:1 ctx1)
           var2 (:2 ctx2)
           var3 (:3 var1)]
       [:p (:4 var1) (:5 var2) (:6 var3)])
    '[{ctx1 [{:1 [:4 {:3 [:6]}]}]}
      {ctx2 [{:2 [:5]}]}]))


(deftest render-test
  (let [default-props {'user {:name "John"
                              :male? true
                              :age 45
                              :friends [{:name "Alice"
                                         :alias ["Al" "Ali"]}
                                        {:name "Bob"
                                         :alias ["Bobo" "Boby"]}
                                        {:name "Cat"
                                         :alias ["MiaoMiao" "Miaaaawww"]}]}}]
    (are [props template hiccup]
      (= (render props (template->ast template)) hiccup)

      '{nil {:my-user {:name "John"
                       :male? true}}}
      '(let [user (:my-user nil)]
         [:p
          (if (:male? user) "Mr" "Miss")
          (:name user)])
       [:p "Mr" "John"]

      '{user {:name "John"
              :male? true}}
      '[:p
        (if (:male? user) "Mr" "Miss")
        (:name user)]
       [:p "Mr" "John"]

      '{user {:name "Lucie"
              :male? false}}
      '[:p
        (if (:male? user) "Mr" "Miss")
        (:name user)]
       [:p "Miss" "Lucie"]

      default-props
      '[:p
        (:name user)
        (let [age (:age user)]
          age)]
      [:p "John" 45]

      default-props
      '[:ul
        (for [friend (:friends user)]
          [:li
           (:name friend)
           [:ul
            (for [alias (:alias friend)]
              [:li alias])]])]
      [:ul
       [:li "Alice"
        [:ul
         [:li "Al"]
         [:li "Ali"]]]
       [:li "Bob"
        [:ul
         [:li "Bobo"]
         [:li "Boby"]]]
       [:li
        "Cat"
        [:ul
         [:li "MiaoMiao"]
         [:li "Miaaaawww"]]]]

      ; TODO: Improve the handling of sequences of components
      ;default-props
      ;'[:ul
      ;  (for [friend (:friends user)]
      ;    (for [alias (:alias friend)]
      ;      [:li alias]))]
      ;[:ul
      ; [:li "Al"]
      ; [:li "Ali"]
      ; [:li "Bobo"]
      ; [:li "Boby"]
      ; [:li "MiaoMiao"]
      ; [:li "Miaaaawww"]]

      default-props
      '[:ul
        (for [friend (:friends user)
              alias (:alias friend)]
          [:li alias])]
      [:ul
       [:li "Al"]
       [:li "Ali"]
       [:li "Bobo"]
       [:li "Boby"]
       [:li "MiaoMiao"]
       [:li "Miaaaawww"]])))

(deftest defc*-test
  (are [args template]
    (= (select-keys (apply #'v/defc* args)
                    [:id :props :template :template-props])
       template)

    ; the dynamic namespace resolution is incorrect while running tests in the IDE.
    ;'[foo-comp [a b]
    ;  nil
    ;  [:p a b]]
    ;'{:id ::foo-comp
    ;  :props '[a b]
    ;  :template '[:p a b]
    ;  :template-props '#{a b}}

    '[foo-comp [a b c]
      {:id ::foobar}
      [:p a b]]
    '{:id ::foobar
      :props '[a b c]
      :template '[:p a b]
      :template-props '#{a b}}))
