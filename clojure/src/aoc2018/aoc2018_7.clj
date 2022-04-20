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

(defn get-graph
  "{:from A :to C}
   {:from A :to F} 

   {A [] B [] C [] F []}
   "
  [instructions graph]
  (reduce
   (fn [result {:keys [from to]}]
     (let [adjacency-list-of-from (result from)]
       (assoc result from (conj adjacency-list-of-from to))))
   graph
   instructions))

(defn get-indegree
  [instructions indegree]
  (reduce
   (fn [result {:keys [to]}]
     (update result to inc))
   indegree
   instructions))

(defn get-zero-indegree-vertices
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

(defn get-pos-indegrees
  [zero-indegree-vertices indegree]
  (apply dissoc indegree zero-indegree-vertices))

(defn update-next-steps
  ""

  [graph next-steps visited zero-indegree-vertices]
  (let [next-steps' (rest next-steps)]
    (->> zero-indegree-vertices
         (filter #(nil? (visited %)))
         (concat next-steps')
         sort)))

(defn work
  [graph {:keys [visited next-steps orders indegree] :as state}]
  (let [step (first next-steps)
        updated-indegree (update-indegree graph indegree step)
        zero-degree-vertices (get-zero-indegree-vertices updated-indegree)]
    (-> state
        (assoc :visited (conj visited step))
        (assoc :next-steps (update-next-steps graph next-steps visited zero-degree-vertices))
        (assoc :orders (conj orders step))
        (assoc :indegree (get-pos-indegrees zero-degree-vertices updated-indegree)))))

(defn create-init-state
  [indegree]
  (let [zero-indegree-vertices (get-zero-indegree-vertices indegree)
        pos-indegrees (get-pos-indegrees zero-indegree-vertices indegree)
        init-state {}]
    (-> init-state
        (assoc :visited #{})
        (assoc :next-steps zero-indegree-vertices)
        (assoc :orders [])
        (assoc :indegree pos-indegrees))))

(defn get-orders
  [indegree graph]
  (let [init-state (create-init-state indegree)] ;; init-state
    (->> init-state
         (iterate #(work graph %))
         (drop-while #(not (empty? (% :next-steps))))
         first
         :orders)))

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
                             (get-indegree instructions))]
           (->> graph
                (get-orders indegree)
                (apply str))))

(def processing-time
  (zipmap "abcdefghijklmnopqrstuvwxyz" (range 61 87)))