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

(get-coord-by-id {:id 3 :x 5 :y 5 :width 2 :height 2})

(defn get-coord-by-id ;; 맵을 받는 함수 => destructuring 이용
  "시작점으로 부터 범위내의 모든 좌표를 구하는 함수
   input: {:id 1 :x 2 :y 2 :width 2 :height 2}
   output: {:id 1, :coords ([2 2] [2 3] [3 2] [3 3])}"
  [{:keys [id x y width height]}]
  {:id id
   :coords (for [x' (range x (+ x width))
                 y' (range y (+ y height))]
             [x' y'])})


(defn extract-id
  "coordinate에 binding된 id들을 추출하는 함수
   input: [841 660] [{:id 349, :coord [841 660]} {:id 355, :coord [841 660]}]
   output: (349 355) (1 2 3)"
  [[_coord coord-map-list]]
  (->> coord-map-list
       (map #(:id %))))

(defn get-overlapped-coords
  "영역이 겹치는 좌표들을 구하는 함수
   input: {:id 1, :coords ([1 3] [1 4] [1 5] [1 6] [2 3] [2 4] [2 5] [2 6] [3 3] [3 4] [3 5] [3 6] [4 3] [4 4] [4 5] [4 6])}
          {:id 2, :coords ([3 1] [3 2] [3 3] [3 4] [4 1] [4 2] [4 3] [4 4] [5 1] [5 2] [5 3] [5 4] [6 1] [6 2] [6 3] [6 4])}
          {:id 3, :coords ([5 5] [5 6] [6 5] [6 6])}
   output: #{[4 3] [3 3] [3 4] [4 4]}"
  [id-coord-list]
  (->> id-coord-list
       (map :coords)
       (apply concat)
       frequencies
       (filter #(> (val %) 1))
       (map first)
       set))

(defn have-overlapped-coord?
  "중복된 좌표를 가지고 있는지 확인하는 함수
   input: #{[4 3] [3 3] [3 4] [4 4]}  {:id 3, :coords ([5 5] [5 6] [6 5] [6 6])}
   output: false
   "
  [overlapped-coords {:keys [id coords]}]
  (let [intersections (set/intersection overlapped-coords (set coords))]
    (pos? (count intersections))))

(defn get-overlapped-ids
  "좌표가 중복되는 id들을 얻어내는 함수
   input: {:id 1, :coords ([1 3] [1 4] [1 5] [1 6] [2 3] [2 4] [2 5] [2 6] [3 3] [3 4] [3 5] [3 6] [4 3] [4 4] [4 5] [4 6])}
          {:id 2, :coords ([3 1] [3 2] [3 3] [3 4] [4 1] [4 2] [4 3] [4 4] [5 1] [5 2] [5 3] [5 4] [6 1] [6 2] [6 3] [6 4])}
          {:id 3, :coords ([5 5] [5 6] [6 5] [6 6])}
   output: #{1 2}"
  [id-coords-list]
  (let [overlapped-coords (get-overlapped-coords id-coords-list)]
    (->> id-coords-list
         (filter #(have-overlapped-coord? overlapped-coords %))
         (map :id)
         set)))


(comment
  (->> "aoc2018/day3.sample.txt"
       puzzle-input
       (map input->fabric-info)
       (map get-coord-by-id)
       get-overlapped-coords
       count)

  (let [fabric-info (->> "aoc2018/day3.sample.txt"
                         puzzle-input
                         (map input->fabric-info))
        fabric-ids (->> fabric-info
                        (map :id)
                        set)]
    (->> fabric-info
         (map get-coord-by-id)
         get-overlapped-ids
         (set/difference fabric-ids)
         first)))