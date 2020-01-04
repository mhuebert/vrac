(ns vrac.core
  (:require [clojure.spec.alpha :as s]
            [vrac.component :as vc]
            [vrac.util :refer [map-vals tag-id-class]]))

;; A spec for the vrac components.
(s/def ::component
  (s/keys :req-un [::vc/id]
          :opt-un [::vc/name
                   ::vc/description
                   ::vc/props
                   ::vc/template]))

;; A spec for the defc macro's arguments.
(s/def ::defc-args
  (s/cat :var symbol?
         :props ::vc/props
         :options (s/? (s/keys :opt-un [::vc/id
                                        ::vc/name
                                        ::vc/description]))
         :template any?))

;; TODO: The process of the parsed-template should be easier if the tree is
;; enriched with all the information that one can grab.
;; Consider using a functional zipper for multi-directional navigation and node enrichment.
;; Consider using rules instead of functions when enriching the tree.
(defn parse-template [template]
  (s/conform ::vc/template template))

;; TODO: Instead of visiting nodes in each different function and collecting the children,
;; use a visitor or maybe a functional zipper.

(defn- get-template-props* [context [kw val :as parsed-template]]
  (case kw
    (:nil :boolean :number :string :keyword) #{}
    :symbol (if (contains? context val) #{} #{val})
    :map (into #{}
               (comp cat ; sequence k0 v0 k1 v1 ...
                     (mapcat (partial get-template-props* context)))
               val)
    :get-kw (let [{:keys [valuable]} val]
              (get-template-props* context valuable))
    (:if :when) (let [{:keys [cond then else]} val]
                  (into #{}
                        (comp (remove nil?)
                              (mapcat (partial get-template-props* context)))
                        [cond then else]))
    (:let :for) (let [{:keys [bindings body]} val
                      [inner-context props] (reduce (fn [[context props] {:keys [symbol valuable]}]
                                                      [(conj context symbol)
                                                       (into props (get-template-props* context valuable))])
                                                    [context #{}]
                                                    bindings)]
                  (into props (get-template-props* inner-context body)))
    :comp (let [{:keys [children]} val]
            (into #{} (mapcat (partial get-template-props* context)) children))
    (:dsl :valuable) (get-template-props* context val)))

(defn get-template-props
  "Returns the set of unbound symbols used in the template."
  [parsed-template]
  (get-template-props* #{} parsed-template))

(defn- group-deps [deps]
  (let [deps (filter seq deps)]
    (cond-> deps
            (= (count deps) 1) first)))

(declare get-comp-deps)

(defn- get-deps
  "Finds the dependencies in a parsed template."
  ; context is a map of: bound-var -> (kw (kw (... (kw unbound-var))))
  [env context [kw val :as parsed-template]]
  (case kw
    (:nil :boolean :number :string :keyword) nil
    :symbol (context val val)
    :map (->> (mapcat identity val) ; sequence k0 v0 k1 v1 ...
              (map (partial get-deps env context))
              group-deps)
    :get-kw (let [{:keys [keyword valuable]} val]
              (list keyword (get-deps env context valuable))) ; TODO: replace position by hashmap
    (:if :when) (let [{:keys [cond then else]} val]
                  (->> [cond then else]
                       (remove nil?)
                       (map (partial get-deps env context))
                       group-deps))
    (:let :for) (let [{:keys [bindings body]} val
                      inner-context (reduce (fn [context {:keys [symbol valuable]}]
                                              (assoc context
                                                symbol (get-deps env context valuable)))
                                            context
                                            bindings)]
                  (get-deps env inner-context body))
    :comp (let [{:keys [keyword props children]} val
                {:keys [tag]} (tag-id-class (str (symbol keyword)))
                component-id (clojure.core/keyword tag)]
            (if (simple-keyword? keyword)
              ; an html node
              (->> (concat (vals props) children)
                   (map (partial get-deps env context))
                   group-deps)
              ; a component
              (let [comp-context (into {}
                                       (map (fn [[k v]]
                                              [(symbol k)
                                               (get-deps env context v)]))
                                       props)]
                (get-comp-deps env comp-context component-id))))
    (:dsl :valuable) (get-deps env context val)))

(defn- get-comp-deps
  "Finds the dependencies in a component."
  [env context component-id]
  (let [deps (get-deps env context (-> env :components component-id :parsed-template))]
    (-> (cond-> deps
          (contains? context 'id) (conj (context 'id))
          (contains? context 'class) (conj (context 'class)))
        group-deps)))

(defn- flatten-deps
  "Returns a vector of flatten deps."
  [deps]
  (cond
    (nil? deps) [[]]

    ; either a keyword, a symbol or primitive value
    (not (sequential? deps)) [[deps]]

    ; this should not happen with correct inputs
    ;(empty? deps) []

    ; (first deps) is either a keyword, a symbol or primitive value
    (not (sequential? (first deps)))
    (mapv #(into [(first deps)] %)
          (flatten-deps (second deps)))

    ; this is a node with multiple deps, like [:p var1 var2]
    :else (into [] (mapcat flatten-deps) deps)))

(defn- deps->eql
  "Transform the flatten deps into eql query trees."
  [deps]
  (->> (filter seq deps)
       (group-by first)
       (mapv (fn [[ctx deps]]
               (let [fields (deps->eql (keep next deps))]
                 (if (empty? fields)
                   ctx
                   {ctx fields}))))))

(defn get-queries
  "Returns the data usage (eql and values together) for this component."
  [env component-id]
  (->> (get-comp-deps env {} component-id)
       (flatten-deps)
       (map reverse)
       (deps->eql)))

(defn render
  "Renders a parsed template and props into hiccup syntax."
  [context [kw val :as parsed-template]]
  (case kw
    (:nil :boolean :number :string :keyword) val
    :symbol (context val)
    :map (into {} (map (fn [[k v]]
                         [(render context k)
                          (render context v)])) val)
    :get-kw (let [{:keys [keyword valuable]} val]
              (keyword (if (= valuable [:nil nil])
                         (context nil)
                         (render context valuable))))
    :if (let [{:keys [cond then else]} val]
          (render context
                  (if (render context cond)
                    then
                    else)))
    :when (let [{:keys [cond then]} val]
            (when (render context cond)
              (render context then)))
    :let (let [{:keys [bindings body]} val
               inner-context (reduce (fn [context {:keys [symbol valuable]}]
                                       (assoc context
                                         symbol (render context valuable)))
                                     context
                                     bindings)]
           (render inner-context body))
    :for (let [{:keys [bindings body]} val
               for (fn for [context [binding & next-bindings]]
                     (if (nil? binding)
                       [(render context body)]
                       (let [{:keys [symbol valuable]} binding
                             coll (render context valuable)]
                         (mapcat (fn [val]
                                   (for (assoc context symbol val) next-bindings))
                                 coll))))]
           (-> (for context bindings)
               (with-meta {:inline true})))
    :comp (let [{:keys [keyword children]} val]
            (into [keyword]
                  (mapcat (fn [child]
                            (let [rendered (render context child)]
                              (if (some-> (meta rendered) :inline)
                                rendered
                                [rendered]))))
                  children))
    (:dsl :valuable) (render context val)))

;; A macro for writing the templates more like a function.
;; Note: in the end, the component is still just a map of data.
(defn- defc* [var props options template]
  (let [ns-str (name (ns-name *ns*))
        var-str (name var)
        default-id (keyword ns-str var-str)
        parsed-template (s/conform ::vc/template template)
        template-props (when-not (= parsed-template :clojure.spec.alpha/invalid)
                         (get-template-props parsed-template))
        missing-props (clojure.set/difference (set template-props)
                                              (set props))]
    (when (= parsed-template :clojure.spec.alpha/invalid)
      (throw (ex-info (str "Invalid template: " (s/explain-str ::vc/template template))
                      (s/explain-str ::vc/template template))))

    (when (seq missing-props)
      (throw (ex-info (str "Props missing from the declaration: " missing-props)
                      missing-props)))

    (merge {:id default-id}
           options
           `{:props '~props
             :template '~template

             ;; Those fields will go away in the long term, once they are used *only* at compilation phase.
             :template-props '~template-props
             :parsed-template '~parsed-template})))

;; A macro for writing the templates more like a function.
;; Note: in the end, the component is still just a map of data.
(defmacro defc [& args]
  (let [{:keys [var props options template]} (s/conform ::defc-args args)]
    `(def ~var ~(defc* var props options template))))

(defn with-components [env components]
  (assoc env
    :components (into {} (map (juxt :id identity)) components)))
