(ns vrac.data
  (:refer-clojure :exclude [ident?]))

(defn- get-primary-keywords
  "Returns a list of primary key's keywords found in the keys of the map m."
  [primary-kws m]
  (into [] (keep (comp primary-kws first)) m))

(defn ident
  "Returns an ident."
  [table id]
  ^:ident [table id])

(defn normalizer [primary-kws]
  "Takes a set of primary key keywords and returns a function that
   returns a list of elements of the shape [table id entity],
   where entity has each of its references to another entity replaced by
   its ident in the shape ^:ident [table id]."
  (fn normalize [entity]
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
                                            (let [child-elements (normalize v)
                                                  last-element (peek child-elements)
                                                  [table id cut-v] last-element]
                                              (if (nil? table)
                                                [(into res (pop child-elements))
                                                 (assoc cut-entity k cut-v)]
                                                [(into res child-elements)
                                                 (assoc cut-entity k (ident table id))])))
                                          [[] {}]
                                          entity)]
        (conj elements [table-kw id-in-table cut-entity]))

      (coll? entity)
      (let [[elements cut-entities] (reduce (fn [[res cut-entities] v]
                                              (let [child-elements (normalize v)
                                                    last-element (peek child-elements)
                                                    [table id cut-v] last-element]
                                                (if (nil? table)
                                                  [(into res (pop child-elements))
                                                   (conj cut-entities cut-v)]
                                                  [(into res child-elements)
                                                   (conj cut-entities (ident table id))])))
                                            [[] (empty entity)]
                                            entity)]
        (conj elements [nil nil cut-entities]))

      :else [[nil nil entity]])))

(defn db-assoc
  "Assocs data coming from a normalizer to a db."
  [db normalized-elements]
  (reduce (fn [db [table id entity]]
            (assoc-in db [table id] entity))
          db
          normalized-elements))

;; ---------------------------------------------------------
;; 5 small utility functions that users are welcome to read,
;; understand, and inline in their code where they see fit.

(comment
  (defn db-assoc
    "Returns a db with the entity replacing previous data."
    ([db ident entity]
     (assoc-in db ident entity)))

  (defn db-dissoc
    "Returns a db with an entity removed."
    [db [table id :as ident]]
    (update db table dissoc id))

  (defn db-update
    "Returns a db with an updated entity."
    ([db ident f & args]
     (apply update-in db ident f args)))

  (defn db-merge
    "Returns a db with the entity merged on top of previous data."
    ([db ident entity]
     (update-in db ident merge entity)))

  (defn db-entity
    "Returns the entity referred by the ident in the db."
    [db ident]
    (get-in db ident)))

;; ---------------------------------------------------------

(defn ident?
  "Returns true if val is an ident, false otherwise."
  [val]
  (:ident (meta val)))

;; Beware! Does not prevent infinite loops.
;;
;; Denormalization does not work in the general case, as the normalised db
;; represents a directed graph of data with potentially some cycles which
;; cannot be represented as a tree of data.
;;
;; This function is just here as an example, users are encouraged to work
;; directly on the normalized db, or to make their own function to
;; denormalize it according to their needs.
(defn denormalize-entity [db entity]
  (clojure.walk/prewalk (fn [val]
                          (cond->> val
                            (ident? val) (get-in db)))
                        entity))
