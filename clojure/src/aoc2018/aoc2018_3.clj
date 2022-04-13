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

(defn input->fabric-info
  "string에서 숫자만 추출하는 함수
   input: #1 @ 1,3: 41x4
   output: {:id 1 :x 1 :y 3 :width 41 :height 4}"
  [string]
  (->> string
       (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)") ;;["#1 @ 1,3: 41x4" "1" "1" "3" "41" "4"]
       (take-last 5) ;;["1" "1" "3" "41" "4"]
       (map read-string)
       (zipmap [:id :x :y :width :height])))


(defn get-coord ;; 맵을 받는 함수 => destructuring 이용
  "시작점으로 부터 범위내의 모든 좌표를 구하는 함수
   input: {:id 1 :x 2 :y 2 :width 2 :height 2}
   output: ({:id 1 :coord [2 2]} {:id 1 :coord [2 3]} {:id 1 :coord [3 2]} {:id 1 :coord [3 3]})"
  [{:keys [id x y width height]}]
  (for [x' (range x (+ x width))
        y' (range y (+ y height))]
    {:id id :coord [x' y']}))

(defn extract-id
  "coordinate에 binding된 id들을 추출하는 함수
   input: [841 660] [{:id 349, :coord [841 660]} {:id 355, :coord [841 660]}]
   output: (349 355) (1 2 3)"
  [[_coord coord-map-list]]
  (->> coord-map-list
       (map #(:id %))))

(defn get-overlapped-ids
  "영역이 중복되는 id들을 얻어내는 함수
   input: {:id 1, :coord [53 238]}
          {:id 1, :coord [53 239]}
          {:id 1, :coord [53 240]}
          {:id 2, :coord [53 238]}
          {:id 2, :coord [53 239]}
          {:id 3, :coord [54 241]}
   output: #{1 2}"
  [fabric-info]
  (->> fabric-info
       (group-by :coord)
       (filter #(>= (count (val %)) 2))
       (map extract-id)
       (apply concat)
       set))

(comment
  (->> "aoc2018/day3.sample.txt"
       puzzle-input
       (map input->fabric-info)
       (map get-coord) ;; hash-map
       (map #(map :coord %))
       (apply concat)
       frequencies
       (filter #(>= (val %) 2))
       count)

  (let [fabric-info (->> "aoc2018/day3.sample.txt"
                         puzzle-input
                         (map input->fabric-info))
        fabric-ids (->> fabric-info
                        (map :id)
                        set)]
    (->> fabric-info
         (map get-coord)
         (apply concat)
         get-overlapped-ids
         (set/difference fabric-ids)
         first)))