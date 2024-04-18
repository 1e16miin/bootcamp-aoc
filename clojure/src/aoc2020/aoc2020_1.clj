(ns aoc2020-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

;; (defn combinations [k seq]
;;   "seq에서 k개의 원소를 선택하는 모든 경우의 수를 반환
;;    input: 2 [1 2 3]
;;    output: [1 2] [2 3] [1 3]

;;    ;; 간단한 코드가 더 좋은 코드일 수 있다.
;;    ;; 하나의 함수 => partial => 새로운 두개 함수
;;    "
;;   (cond
;;     (> k (count seq)) nil
;;     (= k (count seq)) [seq]
;;     (= 1 k) (map vector seq)
;;     :else (reduce concat (map-indexed
;;                           (fn [i x] (map #(cons x %) (combinations (dec k) (drop (inc i) seq))))
;;                           seq))))
                          ;;for로 구현해보기
;; (def combinations-2
;;   (partial combinations 2))

;; (def combinations-3
;;   (partial combinations 3))

(defn comibination-2-elements
  "seq내의 서로 다른 2개의 원소의 조합을 반환
   input: '(1 2 3)
   ouput: '([1 2] [1 3] [2 1] [2 3] [3 1] [3 2])
   "
  [numbers]
  (for [x numbers
        y numbers
        :when (not= x y)]
    [x y]))

(defn comibination-3-elements
  "seq내의 서로 다른 3개의 원소의 조합을 반환
   input: '(1 2 3)
   output: '([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])
   "
  [numbers]
  (for [x numbers
        y numbers
        z numbers
        :when (and (not= x y)
                   (not= y z)
                   (not= z x))]
    [x y z]))

(comment (let [input (->> "aoc2020/day1.sample.txt"
                          puzzle-input
                          (map #(Integer/parseInt %)))]
           (->> input
                comibination-2-elements
                (filter #(= (apply + %) 2020))
                first
                (apply *)))
         (let [input (->> "aoc2020/day1.sample.txt"
                          puzzle-input
                          (map #(Integer/parseInt %)))]
           (->> input
                comibination-3-elements
                (filter #(= (apply + %) 2020))
                first
                (apply *))))
