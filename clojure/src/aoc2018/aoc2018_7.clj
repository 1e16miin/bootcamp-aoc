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

(defn get-dec-indegree-vertices
  [graph vertices]
  (reduce
   (fn [result vertex]
     (let [adjacency-vertices (graph vertex)]
       (concat result adjacency-vertices)))
   []
   vertices))

(defn update-indegree
  [graph indegree vertices]
  (let [dec-indegree (->> vertices
                          (get-dec-indegree-vertices graph)
                          frequencies)]
    (reduce
     (fn [result [vertex dec-val]]
       (update result vertex - dec-val))
     indegree
     dec-indegree)))

(defn get-pos-indegrees
  [zero-indegree-vertices indegree]
  (apply dissoc indegree zero-indegree-vertices))



(def take-times
  (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 61 87)))

(defn get-finished-steps
  [in-progress time]
  (->> in-progress
       (filter #(= (:end %) time))
       (map key)
       set))

(defn update-next-steps
  ""
  [graph next-steps visited zero-indegree-vertices]
  (let [next-steps' (rest next-steps)]
    (->> zero-indegree-vertices
         (filter #(nil? (visited %)))
         (concat next-steps')
         sort)))

(defn update-in-progress
  [in-progress workable-steps max-workers time finished-steps]
  (let [in-progress' (->> in-progress
                          (filter #(nil? (finished-steps %)))
                          (into {}))
        workable-steps' (sort workable-steps)
        workers (count in-progress')

        workable-steps'' (take (- max-workers workers) workable-steps')]
    (reduce
     (fn [result step]
       (let [take-time (take-times (first step))]
         (assoc result step {:start time
                             :end (+ time take-time)})))
     in-progress'
     workable-steps'')))
;; (first "c")
(defn work
  [graph {:keys [visited next-steps orders indegree max-workers in-progress time] :as state}]
  (let [finished-steps (get-finished-steps in-progress time)
        updated-indegree (update-indegree graph indegree finished-steps)
        zero-indegree-vertices (get-zero-indegree-vertices updated-indegree)]
    (-> state
        ;; (assoc :visited (conj visited finished-steps))
        ;; (assoc :next-steps (update-next-steps graph next-steps visited zero-degree-vertices))
        (assoc :orders (concat orders finished-steps))
        (assoc :indegree (get-pos-indegrees zero-indegree-vertices updated-indegree))
        (assoc :max-workers max-workers)
        (assoc :in-progress (update-in-progress in-progress zero-indegree-vertices max-workers time finished-steps))
        (update :time inc))))

(defn create-init-state
  [indegree max-workers]
  (let [zero-indegree-vertices (get-zero-indegree-vertices indegree)
        pos-indegrees (get-pos-indegrees zero-indegree-vertices indegree)
        init-state {}
        in-progress (update-in-progress {} zero-indegree-vertices max-workers 0 {})]
    (-> init-state
        ;; (assoc :visited #{})
        ;; (assoc :next-steps zero-indegree-vertices)
        (assoc :orders [])
        (assoc :indegree pos-indegrees)
        (assoc :max-workers max-workers)
        (assoc :in-progress in-progress)
        (assoc :time 0))))

(defn get-orders
  [indegree max-workers graph]
  (let [init-state (create-init-state indegree max-workers)] ;; init-state
    (->> init-state
         (iterate #(work graph %))
         (drop-while #(not (empty? (% :in-progress))))
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
                (get-orders indegree 1)
                (apply str))))


;; (take-times \a)

;; (first "a")