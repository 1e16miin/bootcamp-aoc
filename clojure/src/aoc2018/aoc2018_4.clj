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


(defn input->time-and-action
  "각 라인별 인풋을 time과 행동부분으로 나누는 함수
   input: [1518-11-03 00:29] wakes up
   output: {:action wakes up :time 15181103}"
  [line]
  (let [[_ YYYY MM DD hh mm action] (re-matches #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)$" line)
        time (read-string (str/join [YYYY MM DD hh mm]))
        ] ;; 파싱을 좀 더 깔끔하게
    {:time time :action action}
    ))


(defn action->guard-id
  "행동에서 가드의 id를 추출하는 함수
   input: Guard #10 begins shift
   output: 10
   "
  [action]
  (->> action
       (re-find #"\d+")
       read-string))


(defn get-sleep-time-by-guard ;; 적어놓고 설명이 힘들다 => 이런 함수 X 
  ;;{1 {:start ... :end ...}
   ;;2 {:start ... :end ...}} ;; guard id별로 start, end 타임 모아보기
  [time-and-action]
  (reduce
   (fn [result {:keys [time action]}]
     (let [id (:last-guard-id result)
           mm (mod time 100)
           new-id (action->guard-id action)]
       (if (str/starts-with? action "Guard")
         (conj result {:last-guard-id new-id new-id {:start [] :end []}})
         (cond
           (= action "falls asleep") (conj result {id {:start (conj (:start (result id)) mm)}})
           (= action "wakes up") (conj result {id {:start (conj (:end (result id)) mm)}})))))
   {:last-guard-id 0}
   time-and-action))

(def test {1 {:start []}})

(:start (test 1))
(conj test {1 {:start (conj (:start (test 1)) 1)}})

;; (defn get-slept
;;   [start end]
;;   (if (> end start)
;;     (- end start)
;;     (- (+ end 60) start)))

(defn get-slept-minutes-during-one-sleep
  "1회 수면동안의 분들을 구하는 함수
   input: [58 2]
   output: [58 59 0 1]
   "
  [start end]
  (if (> end start)
    (range start end)
    (concat (range start 60) (range 0 end))))

(defn get-all-slept-times
  "가드가 수면 중인 모든 분을 구하는 함수
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
         (assoc result :sleep-times (concat sleep-times (get-slept-minutes-during-one-sleep start end))))))
   {:sleep-times [] :start 0 :id 0}
   id-and-time-list))


(defn freq*guard
  "가드의 id와 그 가드가 가장 많이 잠든 횟수를 곱하는 함수
   input: {:id 1 :sleep-times [2 3 4 3]}
   output 2 = (1 * 2)"
  [{:keys [id sleep-times]}]
  (->> sleep-times [ 1 2 2 3] {1 }
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
{id: [1 2 ]}

;; 중간중간 데이터 => let-binding
(comment (->> "aoc2018/day4.sample.txt"
              puzzle-input
              (map input->time-and-action)
              (sort-by :time)
              get-sleep-time-by-guard
              ;; (filter #(> (count %) 1))
              ;; (group-by :id) {[id] {:id :start } {:id :end}}
              ;; (map second)
              ;; (map get-all-slept-times){[ {:id :start} {:id :end} ]}
              ;; (sort-by #(count (% :sleep-times)))
              ;; last
              ;; freq*guard
              )
         (->> "aoc2018/day4.sample.txt"
              puzzle-input
              (map input->time-and-action)
              (sort-by :time)
              get-sleep-time-by-guard
              (filter #(> (count %) 1))
              (group-by :id)
              (map second)
              (map get-all-slept-times)
              (map get-most-freq-slept-time-by-guard)
              get-most-slept-same-time-guard
              mimute*guard))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.