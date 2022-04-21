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

(defn add-adjacency-step
  [init-graph {:keys [from to]}]
  (let [adjacency-list-of-from (-> init-graph
                                   from
                                   (conj to))]
    (assoc init-graph from adjacency-list-of-from)))

(defn get-graph
  "step들의 연결관계를 그래프로 표현
   input: ({:from C, :to A}
          {:from C, :to F}
          {:from A, :to B}
          {:from A, :to D}
          {:from B, :to E}
          {:from D, :to E}
          {:from F, :to E}) #{E C F B A D}
   output: {E [], C [A F], F [E], B [E], A [B D], D [E]}"
  [instructions steps]
  (let [init-graph (reduce #(assoc %1 %2 []) {} steps)]
    (->> instructions
         (reduce add-adjacency-step init-graph))))

(defn get-prior-step-num
  "각 스텝의 선행 작업 갯수가 몇개 있는지 얻는 함수
   input: ({:from C, :to A}
          {:from C, :to F}
          {:from A, :to B}
          {:from A, :to D}
          {:from B, :to E}
          {:from D, :to E}
          {:from F, :to E}) #{E C F B A D}
   
   output: {E 3, C 0, F 1, B 1, A 1, D 1}
   "
  [instructions steps]
  (let [init-prior-step-num (reduce #(assoc %1 %2 0) {} steps)]
    (map (fn [{:keys [to]}] (update init-prior-step-num to inc)) instructions)))

(defn get-workable-step
  "선행작업이 없는 step들을 반환
   input: {E 3, C 0, F 1, B 1, A 1, D 1}
   output: (C)
   "
  [prior-step-num]
  (->> prior-step-num
       (filter #(zero? (val %)))
       (map first)))

(defn get-dec-prior-step-num
  "입력으로 들어온 step들의 다음 step들을 한 seq넣어서 반환
   input: {E [], C [A F], F [E], B [E], A [B D], D [E]} (A F)
   output: (B D E)
   "
  [graph steps]
  (->> steps
       (mapcat #(graph %))
       frequencies))

(let [graph {5 [], 3 [1 4], 6 [5], 2 [5], 1 [2 4], 4 [5]}]
  (->> '(1 6)
       (mapcat #(graph %))
       frequencies))

(defn update-prior-step-num
  "각 step의 선행step의 수를 업데이트
   input: {E [], C [A F], F [E], B [E], A [B D], D [E]} {E 3, F 1, B 1, A 1, D 1} (A F)
   output: {E 2, F 1, B 1, A 1, D 1}
   "
  [graph prior-step-num steps]
  (let [dec-prior-step-num (get-dec-prior-step-num graph steps)]
    (->> prior-step-num
         
         )))

;; (reduce
;;  (fn [result [step dec-prior-work]]
;;    (update result step - dec-prior-work))
;;  prior-step
;;  dec-prior-step-num)

(defn get-pos-prior-work
  "선행작업의 수가 0인 step을 dissoc
   input: {E 3, F 0, B 1, A 0, D 1} (A)
   ouput: {E 3, F 0, B 1, D 1}
   "
  [prior-step-num workable-steps]
  (apply dissoc prior-step-num workable-steps))

(def take-process-times
  (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 61 87)))

(defn get-finished-steps
  "진행 중인 작업들 중에서 작업이 끝난 step을 반환
   input: {A {:start 3 :end 4} F {:start 3 :end 9}} 4
   output: (A)
   "
  [in-progress time]
  (->> in-progress
       (filter #(= (:end (val %)) time))
       (map key)
       set))



(defn update-in-progress
  "진행 중인 작업안에서 끝난 작업을 삭제하고, 일을 할 수 있는 worker의 수만큼 선행 작업이 없는 step들을 추가
   input: {C {:start 0 :end 3}} (A F) 1 3 (C)
   output: {A {:start 3 :end 4}}
   "
  [{:keys [in-progress max-workers time]} workable-steps finished-steps]
  (let [in-progress' (->> in-progress
                          (filter #(nil? (finished-steps (key %))))
                          (into {}))
        ;; workable-steps' (sort workable-steps)
        workers-num (count in-progress')
        workable-steps' (take (- max-workers workers) workable-steps)
        in-progress => workers
        new-workers (->> workable-steps'
                         (map (fn [v] {:start :end :step})))] [{:start 0 :end 3 :step "A"}] ;; hash-map => list
       (reduce
        (fn [result step]
          (let [process-time (take-process-times (first step))]
            (assoc result step {:start time
                                :end (+ time process-time)})))
        in-progress'
        workable-steps')))

(defn work
  "1초 마다 작업이 끝났는지 확인하고 끝난 작업들은 :orders에 넣어준다.
   input: {E [], C [A F], F [E], B [E], A [B D], D [E]} 
          {:orders []
           :prior-work {E 3, F 1, B 1, A 1, D 1}
           :max-workers 1
           :in-progress {C {:start 0 :end 3}}
           :time 3}
   output: {:orders [C]
           :prior-work {E 2, F 0, B 1, D 1}
           :max-workers 1
           :in-progress {A {:start 3 :end 4}} ;; state에 finished도 넣어서 관리하기
           :time 4}
   "
  [graph {:keys [orders prior-work max-workers in-progress time] :as state}]
  (let [finished-steps (get-finished-steps in-progress time)
        prior-work' (update-prior-step-num graph prior-work finished-steps)
        zero-prior-work-steps (get-workable-step prior-work')
        in-progress' (update-in-progress state zero-prior-work-steps finished-steps)
        pos-prior-work (->> in-progress'
                            keys
                            (get-pos-prior-work prior-work'))] ;; get-pos-prior-work 네이밍을 고쳐보자
    (-> state
        (assoc :orders (concat orders finished-steps))
        (assoc :prior-work pos-prior-work)
        (assoc :in-progress in-progress')
        (update :time inc))))

(defn create-init-state
  "iterate의 초기 상태에 들어갈 값
   
   "
  [prior-work max-workers]
  (let [zero-prior-work-steps (get-workable-step prior-work)
        init-state {}
        in-progress (update-in-progress {} zero-prior-work-steps max-workers 0 {})
        pos-prior-work (->> in-progress
                            keys
                            (get-pos-prior-work prior-work))]
    (-> init-state
        (assoc :orders [])
        (assoc :prior-work pos-prior-work)
        (assoc :max-workers max-workers)
        (assoc :in-progress in-progress)
        (assoc :time 0))))

(defn get-orders

  [graph prior-work max-workers]
  (let [init-state (create-init-state prior-work max-workers)] ;; init-state
    (->> init-state
         (iterate #(work graph %))
         (drop-while #(false? (empty? (% :in-progress)))) ;; logical false vs false  ;; not-empty로 가능
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
        prior-work (get-prior-step-num instructions steps)]
    (-> graph
        (get-orders prior-work 1)
        :orders
        str/join))

  (-> {}
      (assoc :time 1))
  ;;part2
  (let [instructions (->> "aoc2018/day7.sample.txt"
                          puzzle-input
                          (map input->instruction)
                          (sort-by (juxt :from :to)))
        from (map :from instructions)
        to (map :to instructions)
        steps (set (concat from to))
        graph (get-graph instructions steps)
        prior-work (get-prior-step-num instructions steps)]
    (-> graph
        (get-orders prior-work 5)
        :time
        dec)))


()