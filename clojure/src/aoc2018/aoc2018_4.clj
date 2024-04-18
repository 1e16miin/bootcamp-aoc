(ns aoc2018-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

(defn input->time-and-action
  "각 라인별 인풋을 time과 행동부분으로 나누는 함수
   input: [1518-11-03 00:29] wakes up
   output: {:action wakes up :time 151811030029}"
  [line]
  (let [[_ YYYY MM DD hh mm action] (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)$" line)
        time (read-string (str/join [YYYY MM DD hh mm]))] ;; 파싱을 좀 더 깔끔하게
    {:time time :action action}))

(defn sort-by-time
  "input을 시간 순으로 정렬
   input: [1518-11-01 00:00] Guard #10 begins shift
          [1518-11-01 00:55] wakes up
          [1518-11-01 00:30] falls asleep
          [1518-11-01 00:25] wakes up
          [1518-11-01 00:05] falls asleep
   output: {:time 151811010000, :action Guard #10 begins shift}
           {:time 151811010005, :action falls asleep}
           {:time 151811010025, :action wakes up}
           {:time 151811010030, :action falls asleep}
           {:time 151811010055, :action wakes up}
   "
  [input]
  (->> input
       puzzle-input
       (map input->time-and-action)
       (sort-by :time)))

(defn action->guard-id
  "행동에서 가드의 id를 추출하는 함수
   input: Guard #10 begins shift
   output: 10
   "
  [action]
  (->> action
       (re-find #"\d+")))

(defn update-sleep-time-by-guard
  "keyword에 해당하는 time-seq에 분을 추가 
   input: {:last-guard-id 10, 10 {:start (5) :end (25)}} 10 :start 30
   output: {:last-guard-id 10, 10 {:start (30 5) :end (25)}}
   "
  [result id key-word mm]
  (let [times (result id)
        time-seq (key-word times)]
    (->> time-seq
         (cons mm)
         (assoc times key-word)
         (assoc result id))))

(defn get-sleep-time-by-guard
  "가드들의 수면 시작과 끝을 각각 seq로 얻는 함수
   input: {:time 151811010000, :action Guard #10 begins shift}
          {:time 151811010005, :action falls asleep}
          {:time 151811010025, :action wakes up}
          {:time 151811010030, :action falls asleep}
          {:time 151811010055, :action wakes up}
          {:time 151811012358, :action Guard #99 begins shift}
          {:time 151811020040, :action falls asleep}
          {:time 151811020050, :action wakes up}
          {:time 151811030005, :action Guard #10 begins shift}
          {:time 151811030024, :action falls asleep}
          {:time 151811030029, :action wakes up}
   output: {:last-guard-id 10, 10 {:start (24 30 5), :end (29 55 25)}, 99 {:start (40), :end (50)}}
   "
  [time-and-action]
  (reduce
   (fn [result {:keys [time action]}]
     (let [id (:last-guard-id result)
           mm (mod time 100)
           times (result id)
           {:keys [start end]} times
           new-id (re-find #"\d+" action)]
       (if new-id
         (assoc result :last-guard-id new-id)
         (cond
           (= action "falls asleep") (update-sleep-time-by-guard result id :start mm);; => 함수로 꺼내보기
           (= action "wakes up") (update-sleep-time-by-guard result id :end mm)))))
   {}
   time-and-action))


(defn get-slept-minutes-during-one-sleep
  "1회 수면동안의 분들을 구하는 함수
   input: [58 2]
   output: [58 59 0 1]
   "
  [start end]
  (if (> end start)
    (range start end)
    (concat (range start 60) (range 0 end))))

(defn get-slept-time
  "전체 수면 분을 구하는 함수
   input: [1 2] [58 2]
   ouput: (1 2 58 59 0 1)"
  [start-end-pairs]
  (reduce
   (fn [result [start end]]
     (concat result (get-slept-minutes-during-one-sleep start end)))
   []
   start-end-pairs))


;; before output: {10 (24 25 26 27 28) 99 (40 41 42 43 44 45 46 47 48 49)}
(defn get-all-slept-times-by-guard
  "가드가 수면 중인 모든 분을 구하는 함수
   input: {:last-guard-id 10, 10 {:start [24], :end [29]}, 99 {:start [40], :end [50]}}
   output: [{:id 10 :slept-times (24 25 26 27 28)} {:id 99 :slept-times(40 41 42 43 44 45 46 47 48 49)}]
   "
  [input]
  (reduce
   (fn [result [id {:keys [start end]}]]
     (let [start-end-pairs (map vector start end)
           slept-times (get-slept-time start-end-pairs)]
       (conj result {:id (Integer/parseInt id) :slept-times slept-times})))
   []
   (dissoc input :last-guard-id)))

(defn sort-by-frequency
  "빈도 순으로 정렬
   input: [1 2 3 4 2]
   output: [1 1] [3 1] [4 1] [2 2]
   "
  [slept-times]
  (->> slept-times
       frequencies
       (sort-by second)))

(defn freq-slept-minute*guard
  "가드의 id와 그 가드가 가장 빈번하게 잔 분을 곱하는 함수
   input: {:id 1 :slept-times [2 3 4 3]}
   output 3 = (1 * 3)"
  [{:keys [id slept-times]}]
  (let [sorted-slept-times (sort-by-frequency slept-times)
        [mm _] (last sorted-slept-times)]
    (* mm id)))


(defn get-most-freq-slept-time-by-guard
  "가드가 가장 빈번하게 잔 분과 그 횟수와 id를 반환하는 함수
   input: {:id 1 :slept-times [2 3 4 3]}
   output: {:id 1 :freq 2 :mm 3}
   "
  [{:keys [id slept-times]}]
  (->> slept-times
       frequencies ;; map {3 2 5 7} ;; (apply min-key val ) => [5 7]
       (apply max-key val)
       (zipmap [:mm :freq])
       (conj {:id id})))

(defn get-most-slept-same-time-guard
  "가장 같은 시간에 빈번하게 잔 가드를 구하는 함수
   input: {:id 10, :mm 24, :freq 2} {:id 99, :mm 45, :freq 3}
   output: {:id 99, :mm 45, :freq 3}
   "
  [guard]
  (->> guard
       (apply max-key :freq)))


(defn mimute*guard
  "분과 가드 id를 곱하는 함수
   input: {:id 99, :mm 45, :freq 3}
   output: 4455
   "
  [{:keys [mm id]}]
  (* mm id))

(comment (let [sorted-input (sort-by-time "aoc2018/day4.sample.txt")]
           (->> sorted-input
                get-sleep-time-by-guard
                get-all-slept-times-by-guard
                (apply max-key #(count (% :slept-times)))
                get-most-freq-slept-time-by-guard
                mimute*guard))
         (let [sorted-input (sort-by-time "aoc2018/day4.sample.txt")]
           (->> sorted-input
                get-sleep-time-by-guard
                get-all-slept-times-by-guard
                (map get-most-freq-slept-time-by-guard)
                get-most-slept-same-time-guard
                mimute*guard)))
