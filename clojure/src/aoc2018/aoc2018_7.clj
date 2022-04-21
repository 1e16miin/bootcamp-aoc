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

(defn get-all-steps
  "모든 step을 얻는 함수
   input:  ({:from C, :to A}
          {:from C, :to F}
          {:from A, :to B}
          {:from A, :to D}
          {:from B, :to E}
          {:from D, :to E}
          {:from F, :to E})
   output #{A B C D E F}"
  [instructions]
  (->> instructions
       (mapcat (fn [{:keys [from to]}] [from to]))
       set))

(defn get-graph
  "step들의 연결관계를 그래프로 표현
   input: ({:from C, :to A}
          {:from C, :to F}
          {:from A, :to B}
          {:from A, :to D}
          {:from B, :to E}
          {:from D, :to E}
          {:from F, :to E})
   output: {E (), C (A F), F (E), B (E), A (B D), D (E)}"
  [instructions steps]
  (let [init-graph (reduce #(assoc %1 %2 []) {} steps)]
    (reduce
     (fn [graph {:keys [from to]}]
       (update-in graph [from] conj to))
     init-graph
     instructions)))


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
    (reduce
     (fn [prior-step-num {:keys [to]}]
       (update prior-step-num to inc))
     init-prior-step-num
     instructions)))

(defn get-workable-step
  "선행작업이 없는 step들을 알파벳 순으로 정렬하여 반환
   input: {E 3, C 0, F 1, B 1, A 1, D 1}
   output: (C)
   "
  [prior-step-num]
  (->> prior-step-num
       (filter #(zero? (val %)))
       (map first)
       sort))

(defn get-dec-prior-step-num
  "작업이 끝난 step들의 바로 다음 작업들의 합의 빈도
   input: {E [], C [A F], F [E], B [E], A [B D], D [E]} (A F)
   output: {B 1 D 1 E 1}
   "
  [graph finished-steps]
  (->> finished-steps
       (mapcat #(graph %))
       frequencies))

(defn update-prior-step-num
  "작업이 끝난 step을 제거히여 남은 step의 선행step의 수를 업데이트
   input: {E [], C [A F], F [E], B [E], A [B D], D [E]} {E 3, B 1, D 1} (A F)
   output: {E 2, B 0, D 0}
   "
  [graph prior-step-num finished-steps]
  (let [dec-prior-step-num (get-dec-prior-step-num graph finished-steps)]
    (merge-with - prior-step-num dec-prior-step-num)))


(defn get-remain-prior-step-num
  "작업이 진행되지 않은 step들의 선행작업의 수를 반환
   input: {E 3, F 0, B 1, A 0, D 1} (A)
   ouput: {E 3, F 0, B 1, D 1}
   "
  [prior-step-num workers]
  (let [working-steps (map :step workers)]
    (apply dissoc prior-step-num working-steps)))

(def take-process-times
  (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 61 87)))

(defn get-finished-steps
  "진행 중인 작업들 중에서 작업이 끝난 step을 반환
   input: [{:step A :start 3 :end 4} {:step F :start 3 :end 9}] 4
   output: #{A}
   "
  [workers time]
  (->> workers
       (filter #(= (:end %) time))
       (map :step)
       set))

(defn update-workers
  "진행 중인 작업안에서 끝난 작업을 삭제하고, 일을 할 수 있는 worker의 수만큼 선행 작업이 없는 step들을 추가
   input: {C {:start 0 :end 3}} (A F) 1 3 (C)
   output: {A {:start 3 :end 4}}
   "
  [{:keys [workers time]} max-workers-num workable-steps finished-steps]
  (let [workers' (->> workers
                      (filter #(nil? (finished-steps (:step %)))))
        workers-num (count workers')
        workable-steps' (take (- max-workers-num workers-num) workable-steps)]
    (->> workable-steps'
         (map (fn [step] {:step step
                          :start time
                          :end (+ (take-process-times (first step)) time)}))
         (concat workers'))))


(defn work
  "1초 마다 작업이 끝났는지 확인하고 끝난 작업들은 :orders에 넣어준다.
   input: {E [], C [A F], F [E], B [E], A [B D], D [E]} 
           1
          {:orders []
           :prior-work {E 3, F 1, B 1, A 1, D 1}
           :in-progress [{:step C :start 0 :end 3}]
           :time 3}
   output: {:orders [C]
           :prior-work {E 2, F 0, B 1, D 1}
           :in-progress [{:step A :start 3 :end 4}]
           :time 4}
   "
  [graph max-workers-num {:keys [orders prior-step-num workers time] :as state}]
  (let [finished-steps (get-finished-steps workers time)
        prior-step' (update-prior-step-num graph prior-step-num finished-steps)
        workable-steps (get-workable-step prior-step')
        workers' (update-workers state max-workers-num workable-steps finished-steps)
        remain-prior-step-num (get-remain-prior-step-num prior-step' workers')] ;; get-pos-prior-work 네이밍을 고쳐보자
    (-> state
        (assoc :orders (concat orders finished-steps))
        (assoc :prior-step-num remain-prior-step-num)
        (assoc :workers workers')
        (update :time inc))))

(defn create-init-state
  "iterate의 초기 상태에 들어갈 값
   input: {E 3, C 0, F 1, B 1, A 1, D 1} 1
   output: {:orders []
           :prior-work {E 3, F 1, B 1, A 1, D 1}
           :in-progress [{:step C :start 0 :end 3}]
           :time 3}
   "
  [prior-step-num max-workers-num]
  (let [workable-steps (get-workable-step prior-step-num)
        init-state {}
        workers (update-workers {:time 0 :workers []} max-workers-num workable-steps #{})
        remain-prior-step-num (get-remain-prior-step-num prior-step-num workers)]
    (-> init-state
        (assoc :orders [])
        (assoc :prior-step-num remain-prior-step-num)
        (assoc :workers workers)
        (assoc :time 0))))

(defn get-orders
  "모든 step이 완료 될 때까지 iterate
   input: {E [], C [A F], F [E], B [E], A [B D], D [E]} 
          1
          {E 3, C 0, F 1, B 1, A 1, D 1}
   output: {:orders (C A B D F E), :prior-step-num {}, :workers (), :time 382}
   "
  [graph prior-work max-workers-num]
  (let [init-state (create-init-state prior-work max-workers-num)]
    (->> init-state
         (iterate #(work graph max-workers-num %))
         (drop-while #(not-empty (% :workers)))
         first)))

(comment
  ;;part1
  (let [instructions (->> "aoc2018/day7.sample.txt"
                          puzzle-input
                          (map input->instruction))
        steps (get-all-steps instructions)
        graph (get-graph instructions steps)
        prior-work (get-prior-step-num instructions steps)]
    (-> graph
        (get-orders prior-work 1)
        :orders
        str/join))

  ;;part2
  (let [instructions (->> "aoc2018/day7.sample.txt"
                          puzzle-input
                          (map input->instruction))
        steps (get-all-steps instructions)
        graph (get-graph instructions steps)
        prior-work (get-prior-step-num instructions steps)]
    (-> graph
        (get-orders prior-work 5)
        :time
        dec)))