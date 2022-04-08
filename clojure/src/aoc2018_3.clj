(ns aoc2018_3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn get-strings-from-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
(defn get-numbers-from-string
  "string에서 숫자만 추출하는 함수
   input: #1 @ 1,3: 41x4
   output: (1 1 3 41 4)"
  [string]
  (let [result (re-seq (re-pattern #"\d+") string)]
    (mapv read-string result)))

(defn get-point-and-range-from-numbers
  "시작점과 범위를 구하는 함수
   input: '(1 1 3 41 4)
   output: [1 3 41 4]"
  [[id x y x-range y-range]]
  (let [point [x y]
        range [x-range y-range]]
    [point range]))

(defn get-coord-list
  "시작점으로 부터 범위내의 모든 좌표를 구하는 함수
   input: [2 2] [1 1]
   output: ([2 2] [2 3] [3 2] [3 3])"
  [[[x y] [width height]]]
  (for [dx (range width)
        dy (range height)]
    [(+ x dx) (+ y dy)]))

(comment (->> "day3.sample.txt"
              get-strings-from-input
              (map get-numbers-from-string)
              (map get-point-and-range-from-numbers)
              (map get-coord-list)
              (apply concat)
              frequencies
              (filter #(>= (val %) 2))
              count))
;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(comment (->> "day3.sample.txt"
              get-strings-from-input
              (map get-numbers-from-string)
              (map get-point-and-range-from-numbers)
              (map get-coord-list)
              (apply concat)
              frequencies
              (filter #(>= (val %) 2))
              count))