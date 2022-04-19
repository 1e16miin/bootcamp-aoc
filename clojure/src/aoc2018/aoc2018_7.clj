(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

(defn input->instruction
  [line]
  (let [[_ from to] (re-matches #"Step (\w) must be finished before step (\w) can begin." line)]
    {:from from :to to}))


;; (defn add-adjacency-vertex
;;   [graph {:keys [from to]}]
;;   (let [adjacency-list-of-from (graph from)]
;;     (assoc graph from (conj adjacency-list-of-from to))))

(defn add-adjacency-vertex
  [graph instructions]
  (reduce
   (fn [result {:keys [from to]}]
     (let [adjacency-list-of-from (result from)]
       (assoc result from (conj adjacency-list-of-from to))))
   graph
   instructions))

(defn topological-sort
  [graph]
  (->> graph
       (iterate #(into {} (for [[k v] % :when (seq v)] [k (mapcat % (sort v))])))
       (take-while seq)
       (mapcat #(for [[k v] % :when (empty? v)] k))
      ;;  str/join
       ))

(comment (let [instructions (->> "aoc2018/day7.sample.txt"
                                 puzzle-input
                                 (map input->instruction)
                                 (sort-by (juxt :from :to)))
               from (map :from instructions)
               to (map :to instructions)
               vertices (set (concat from to))
               graph (reduce #(assoc %1 %2 []) {} vertices)]
           (->> instructions
                (add-adjacency-vertex graph)
                (into (sorted-map))
                topological-sort)))
