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
  [instructions steps]
  (reduce
   (fn [result {:keys [from to]}]
     (let [adjacency-list-of-from (result from)]
       (assoc result from (conj adjacency-list-of-from to))))
   (reduce #(assoc %1 %2 []) {} steps)
   instructions))

(defn get-prior-work
  [instructions steps]
  (reduce
   (fn [result {:keys [to]}]
     (update result to inc))
   (reduce #(assoc %1 %2 0) {} steps)
   instructions))

(defn get-zero-prior-work-steps
  [prior-works]
  (->> prior-works
       (filter #(zero? (val %)))
       (map first)))

(defn get-next-steps
  [graph steps]
  (reduce
   (fn [result step]
     (let [adjacency-vertices (graph step)]
       (concat result adjacency-vertices)))
   []
   steps))

(defn update-prior-work
  [graph prior-work steps]
  (let [dec-prior-works (->> steps
                             (get-next-steps graph)
                             frequencies)]
    (reduce
     (fn [result [step dec-prior-work]]
       (update result step - dec-prior-work))
     prior-work
     dec-prior-works)))



(defn get-pos-prior-work
  [prior-works zero-prior-work-steps]
  (apply dissoc prior-works zero-prior-work-steps))

(def take-times
  (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 61 87)))

(defn get-finished-steps
  [in-progress time]
  (->> in-progress
       (filter #(= (:end (val %)) time))
       (map key)
       set))


(defn update-in-progress
  [in-progress workable-steps max-workers time finished-steps]
  (let [in-progress' (->> in-progress
                          (filter #(nil? (finished-steps (key %))))
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

(defn work
  [graph {:keys [orders prior-work max-workers in-progress time] :as state}]
  (let [finished-steps (get-finished-steps in-progress time)
        prior-work' (update-prior-work graph prior-work finished-steps)
        zero-prior-work-steps (get-zero-prior-work-steps prior-work')
        in-progress' (update-in-progress in-progress zero-prior-work-steps max-workers time finished-steps)
        prior-work'' (->> in-progress'
                          keys
                          (get-pos-prior-work prior-work'))]
    (-> state
        (assoc :orders (concat orders finished-steps))
        (assoc :prior-work prior-work'')
        (assoc :max-workers max-workers)
        (assoc :in-progress in-progress')
        (update :time inc))))

(defn create-init-state
  [prior-work max-workers]
  (let [zero-prior-work-steps (get-zero-prior-work-steps prior-work)
        init-state {}
        in-progress (update-in-progress {} zero-prior-work-steps max-workers 0 {})
        prior-work' (->> in-progress
                         keys
                         (get-pos-prior-work prior-work))]
    (-> init-state
        (assoc :orders [])
        (assoc :prior-work prior-work')
        (assoc :max-workers max-workers)
        (assoc :in-progress in-progress)
        (assoc :time 0))))

(defn get-orders

  [graph prior-work max-workers]
  (let [init-state (create-init-state prior-work max-workers)] ;; init-state
    (->> init-state
         (iterate #(work graph %))
         (drop-while #(false? (empty? (% :in-progress))))
         first)))

(comment
  ;;part1
  (let [instructions (->> "aoc2018/day7.sample.txt"
                          puzzle-input
                          (map input->instruction)
                          (sort-by (juxt :from :to)))
        from (map :from instructions)
        to (map :to instructions)
        steps (set (concat from to))
        graph (get-graph instructions steps)
        prior-work (get-prior-work instructions steps)]
    (-> graph
        (get-orders prior-work 1)
        :orders
        str/join))
  ;;part2
  (let [instructions (->> "aoc2018/day7.sample.txt"
                          puzzle-input
                          (map input->instruction)
                          (sort-by (juxt :from :to)))
        from (map :from instructions)
        to (map :to instructions)
        steps (set (concat from to))
        graph (get-graph instructions steps)
        prior-work (get-prior-work instructions steps)]
    (-> graph
        (get-orders prior-work 5)
        :time
        dec)))
