(ns aoc2018_3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(defn puzzle-input
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
(defn input->fabric-info
  "string에서 숫자만 추출하는 함수
   input: #1 @ 1,3: 41x4
   output: {:id 1 :x 1 :y 3 :width 41 :height 4}"
  [string]
  (->> string
       (re-seq #"\d+")
       (map read-string)
       (zipmap [:id :x :y :width :height])))

;; ->는 영어로 to로 읽습니다.`
;; a->b = a to b

(defn get-coord ;; 맵을 받는 함수 => destructuring 이용
  "시작점으로 부터 범위내의 모든 좌표를 구하는 함수
   input: {:id 1 :x 2 :y 2 :width 2 :height 2}
   output: ({:id 1 :coord [2 2]} {:id 1 :coord [2 3]} {:id 1 :coord [3 2]} {:id 1 :coord [3 3]})"
  [{:keys [id x y width height]}]
  ;; map destructing https://clojure.org/guides/destructuring#_associative_destructuring
  (for [dx (range width)
        dy (range height)]
    {:id id :coord [(+ x dx) (+ y dy)]}))


(defn extract-coord
  [lst]
  (->> lst
       (map #(:coord %))))

(comment (->> "day3.sample.txt"
              puzzle-input
              (map input->fabric-info)
              (map get-coord) ;; hash-map
              (map extract-coord)
              (apply concat)
              frequencies
              (filter #(>= (val %) 2))
              count))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)



(defn extract-id
  "coordinate에 binding된 id들을 추출하는 함수
   input: [841 660] [{:id 349, :coord [841 660]} {:id 355, :coord [841 660]}]
   output: (349 355)
   "
  [[_coord coord-map-list]]
  (->> coord-map-list
       (map #(:id %))))

(comment
  (let [fabric-info (->> "day3.sample.txt"
                         puzzle-input
                         (map input->fabric-info))
        fabric-ids (->> fabric-info
                        (map #(:id %))
                        set)]
    (->> fabric-info
         (map get-coord)
         (apply concat)
         (group-by :coord)
         (filter #(>= (count (val %)) 2))
         (map extract-id)
         (apply concat)
         set
         (set/difference fabric-ids)
         first)))