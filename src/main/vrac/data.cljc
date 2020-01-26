(ns vrac.data)

(defn- get-primary-keywords
  "Returns a list of primary key's keywords found in the keys of the map m."
  [primary-kws m]
  (into [] (keep (comp primary-kws first)) m))

(defn normalized-elements
  "Returns a list of elements of the shape [table id entity],
   where entity has each reference to another entity replaced by its ident
   in the shape [table id]."
  [primary-kws entity]
  (cond
    (map? entity)
    (let [kws (get-primary-keywords primary-kws entity)
          kw-count (count kws)
          _ (when (> kw-count 1)
              (throw (ex-info (str "Entity has multiple primary keys: " kws)
                              entity)))
          [table-kw id-in-table] (if (zero? kw-count)
                                   [nil nil]
                                   [(first kws) (get entity (first kws))])
          [elements cut-entity] (reduce (fn [[res cut-entity] [k v]]
                                          (let [child-elements (normalized-elements primary-kws v)
                                                last-element (peek child-elements)
                                                [table id cut-v] last-element]
                                            (if (nil? table)
                                              [(into res (pop child-elements))
                                               (assoc cut-entity k cut-v)]
                                              [(into res child-elements)
                                               (assoc cut-entity k [table id])])))
                                        [[] {}]
                                        entity)]
      (conj elements [table-kw id-in-table cut-entity]))

    (coll? entity)
    (let [[elements cut-entities] (reduce (fn [[res cut-entities] v]
                                            (let [child-elements (normalized-elements primary-kws v)
                                                  last-element (peek child-elements)
                                                  [table id cut-v] last-element]
                                              (if (nil? table)
                                                [(into res (pop child-elements))
                                                 (conj cut-entities cut-v)]
                                                [(into res child-elements)
                                                 (conj cut-entities [table id])])))
                                          [[] (empty entity)]
                                          entity)]
      (conj elements [nil nil cut-entities]))

    :else [[nil nil entity]]))

(defn assoc-db
  "Returns a db with information replaced by the entity."
  [db [table id entity]]
  (assoc-in db [table id] entity))

(defn dissoc-db
  "Returns a db with information removed at specified entity path."
  [db [table id]]
  (update db table dissoc id))

(defn update-db
  "Returns a db with information updated by the entity."
  ([db [table id entity]]
   (update-db merge db [table id entity]))
  ([f-merge db [table id entity]]
   (update-in db [table id] f-merge entity)))
