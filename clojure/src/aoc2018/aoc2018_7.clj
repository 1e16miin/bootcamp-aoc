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

(defn get-graph
  [instructions graph]
  (reduce
   (fn [result {:keys [from to]}]
     (let [adjacency-list-of-from (result from)]
       (assoc result from (conj adjacency-list-of-from to))))
   graph
   instructions))

(defn get-indegree
  [instructions graph]
  (reduce
   (fn [result {:keys [to]}]
     (update result to inc))
   graph
   instructions))

(defn get-zero-degree-vertices
  [indegree]
  (->> indegree
       (filter #(zero? (val %)))
       (map first)))

(defn update-indegree
  [graph indegree vertex]
  (let [adjacency-vertices (graph vertex)]
    (reduce
     (fn [result adjacency-vertex]
       (update result adjacency-vertex dec))
     indegree
     adjacency-vertices)))

(defn remove-zero-indegree-vertex
  [zero-degree-vertices indegree]
  (reduce
   (fn [result zero-degree-vertex]
     (dissoc result zero-degree-vertex))
   indegree
   zero-degree-vertices))

(defn update-q
  [graph q visited indegree]
  (let [result (drop 1 q)
        vertex (first q)
        ;; adjacency-vertices (graph vertex)
        zero-degree-vertices (get-zero-degree-vertices indegree)]
    (->> zero-degree-vertices
         (filter #(nil? (visited %)))
         (concat result)
         sort)))

(defn work
  [graph {:keys [visited q route indegree]}]
  (let [vertex (first q)
        updated-visited (conj visited vertex)
        updated-indegree (update-indegree graph indegree vertex)
        zero-degree-vertices (get-zero-degree-vertices updated-indegree)
        after-remove-zero-indegree (remove-zero-indegree-vertex zero-degree-vertices updated-indegree)
        updated-q (update-q graph q visited updated-indegree)
        updated-route (conj route vertex)]
    {:visited updated-visited :q updated-q :route updated-route :indegree after-remove-zero-indegree}))


(defn get-order
  [indegree graph]
  (let [zero-degree-vertices (get-zero-degree-vertices indegree)
        updated-indegree (remove-zero-indegree-vertex zero-degree-vertices indegree)
        init-data {:indegree updated-indegree :visited #{} :q zero-degree-vertices :route []}]
    (->> init-data
         (iterate #(work graph %))
         (take-while #(not (empty? (% :q))))
         last)))

(comment (let [instructions (->> "aoc2018/day7.sample.txt"
                                 puzzle-input
                                 (map input->instruction)
                                 (sort-by (juxt :from :to)))
               from (map :from instructions)
               to (map :to instructions)
               vertices (set (concat from to))
               graph (->> vertices
                          (reduce #(assoc %1 %2 []) {})
                          (get-graph instructions))
               indegree (->> vertices
                             (reduce #(assoc %1 %2 0) {})
                             (get-indegree instructions))
               {:keys [q route]} (get-order indegree graph)]
           (->> q
                (concat route)
                (apply str))))

(str/join '("G" "L" "M" "V" "W" "X" "Z" "D" "K" "O" "U" "C" "E" "J" "R" "H" "F" "A" "P" "I" "T" "S" "B" "Q" "N"))