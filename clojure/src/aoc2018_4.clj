(ns aoc2018_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))


(defn input->time-and-behavior
  [line]
  (let [behavior (subs line 19)]
    (->> line
         (re-seq #"\d+")
         str/join
         (take 12)
         str/join
         read-string
         (hash-map :time)
         (conj {:behavior behavior}))))

(defn get-guard-id
  [behavior]
  (->> behavior
       (re-find #"\d+")
       read-string))


(defn get-sleep-time-by-guard
  [time-and-behavior]
  (reduce
   (fn [result {:keys [time behavior]}]
     (let [id (:id (peek result))
           mm (mod time 100)]
       (if (str/starts-with? behavior "Guard")
         (conj result {:id (get-guard-id behavior)})
         (cond
           (= behavior "falls asleep") (conj result {:id id :start mm})
           (= behavior "wakes up") (conj result {:id id :end mm})))))
   [{:id 0}]
   time-and-behavior))

(defn get-slept
  [start end]
  (if (> end start)
    (- end start)
    (- (+ end 60) start)))

(defn get-slept-minutes
  [start end]
  (if (> end start)
    (range start end)
    (concat (range start 60) (range 0 end))))

(defn get-sleep-times
  "가드가 잠에 빠져있는 모든 분을 구하는 함수
   input: [{:id 1 :start 2} {:id 1 :end 4}]
   output: {sleep-times: [2 3] :id 1 :start 2}"
  [id-and-time-list]
  (reduce
   (fn [result id-and-time]
     (let [start (result :start)
           end (id-and-time :end)
           sleep-times (result :sleep-times)
           id (id-and-time :id)]
       (if (nil? end)
         (conj result {:start (:start id-and-time) :id id})
         (assoc result :sleep-times (concat sleep-times (get-slept-minutes start end))))))
   {:sleep-times [] :start 0 :id 0}
   id-and-time-list))


(defn freq*guard
  "가드의 id와 그 가드가 가장 많이 잠든 횟수를 곱하는 함수
   input: {:id 1 :sleep-times [2 3 4 3]}
   output 2 = (1 * 2)"
  [{:keys [id sleep-times]}]
  (->> sleep-times
       frequencies
       (sort-by second)
       last
       first
       (* id)))

(defn get-most-freq-slept-time-by-guard
  [{:keys [sleep-times id]}]
  (->> sleep-times
       frequencies
       (sort-by second)
       last
       (zipmap [:mm :freq])
       (conj {:id id})))

(defn get-most-slept-same-time-guard
  [guard-info]
  (->> guard-info
       (sort-by :freq)
       last))

(defn mimute*guard
  [{:keys [mm id]}]
  (* mm id))

(comment (->> "day4.sample.txt"
              puzzle-input
              (map input->time-and-behavior)
              (sort-by :time)
              get-sleep-time-by-guard
              (filter #(> (count %) 1))
              (group-by :id)
              (map second)
              (map get-sleep-times)
              (sort-by #(count (% :sleep-times)))
              last
              freq*guard)
         (->> "day4.sample.txt"
              puzzle-input
              (map input->time-and-behavior)
              (sort-by :time)
              get-sleep-time-by-guard
              (filter #(> (count %) 1))
              (group-by :id)
              (map second)
              (map get-sleep-times)
              (map get-most-freq-by-guard)
              get-most-slept-same-time-guard
              mimute*guard))



;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.