(ns aoc2018_6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))


(defn input->point
  "input을 point로 매핑
   input: 1, 1
          1, 6
          8, 3
          3, 4
          5, 5
          8, 9
   output: [1 1] [1 6] [8 3] [3 4] [5 5] [8 9]
   "
  [line]
  (let [[_ x y] (re-matches #"(\d+), (\d+)" line)]
    (map #(Integer/parseInt %) [x y])))

(defn get-boundary
  "전체 좌표를 통해 x의 최대 최소 y의 최대 최소를 return
   input: [1 1] [1 6] [8 3] [3 4] [5 5] [8 9]
   output: {:min-x 1, :min-y 1, :max-x 8, :max-y 9}
   "
  [points]
  (let [[xs ys] (apply map vector points)]
    {:min-x (apply min xs)
     :min-y (apply min ys)
     :max-x (apply max xs)
     :max-y (apply max ys)}))

(get-points-in-boundary {:min-x 1, :min-y 1, :max-x 4, :max-y 4})

(defn get-points-in-boundary
  "경계 내부의 점들을 return
   input: {:min-x 1, :min-y 1, :max-x 8, :max-y 9}
   output: [2 2] [2 3] [3 2] [3 3]
   "
  [{:keys [min-x min-y max-x max-y]}]
  (for [x (range (inc min-x) max-x)
        y (range (inc min-y) max-y)]
    [x y]))

(defn abs 
  "절댓값을 구하는 함수
   input: -3
   output: 3
   "
  [number] (max number (- number)))

(defn get-manhattan-distance
  "맨허튼 거리 |x1-x2| + |y1 - y2|를 구하는 함수
   input: [1 1] {:id 1 :x 2 :y 2}
   output: 2
   "
  [[x' y'] {:keys [id x y]}]
  (let [dx (- x' x)
        dy (- y' y)
        distance (+ (abs dx) (abs dy))]
    {:id id :distance distance}))

(defn get-min-distance
  "가장 작은 distance를 return
   input: {:id 0, :distance 2} {:id 1, :distance 4} {:id 2, :distance 7} {:id 3, :distance 10})
   output: 2
   "
  [distances]
  (->> distances
       (map :distance)
       (apply min)))

(defn get-nearest-starting-point
  "한 점에서 가장 가까운 starting-point들의 id를 구하는 함수
   input: '({:id 0, :x 1, :y 2} {:id 1, :x 2, :y 3} {:id 2, :x 3, :y 5} {:id 3, :x 4, :y 7}) '(2 2)
   output: 0 1
   "
  [starting-points point]
  (let [distances (map #(get-manhattan-distance point %) starting-points)
        min-distance (get-min-distance distances)]
    (->> distances
         (filter #(= (:distance %) min-distance))
         (map :id))))

(get-boundary-points {:min-x 1, :min-y 1, :max-x 3, :max-y 3})

(defn get-boundary-points
  "경계에 있는 점들을 return
   input: {:min-x 1, :min-y 1, :max-x 3, :max-y 3}
   output: [1 1] [2 1] [3 1] [3 2] [2 3] [3 3] [1 2] [1 3]
   "
  [{:keys [min-x min-y max-x max-y]}]
  (let [left-side (for [y (range (inc min-y) (inc max-y))]
                    [min-x y])
        right-side (for [y (range min-y max-y)]
                     [max-x y])
        down-side (for [x (range min-x max-x)]
                    [x min-y])
        up-side (for [x (range (inc min-x) (inc max-x))]
                  [x max-y])]
    (concat down-side right-side up-side left-side)))

(get-infinite-area-ids '({:id 0, :x 1, :y 2} {:id 1, :x 2, :y 3} {:id 2, :x 3, :y 5} {:id 3, :x 4, :y 7})
                       '([1 2] [2 2] [3 2] [4 2] [4 3] [4 4] [4 5] [4 6] [2 7] [3 7] [4 7] [1 3] [1 4] [1 5] [1 6] [1 7]))

(defn get-infinite-area-ids
  "무한한 크기의 영역을 가지는 영역의 id를 반환
   input: '({:id 0, :x 1, :y 1}
            {:id 1, :x 1, :y 6}
            {:id 2, :x 8, :y 3}
            {:id 3, :x 3, :y 4}
            {:id 4, :x 5, :y 5}
            {:id 5, :x 8, :y 9})
          '([1 1][2 1][3 1][4 1][5 1][6 1][7 1][8 1][8 2][8 3][8 4][8 5][8 6][8 7][8 8]
            [2 9][3 9][4 9][5 9][6 9][7 9][8 9][1 2][1 3][1 4][1 5][1 6][1 7][1 8][1 9])
   output: #{0 1 2 5}
   "
  [indexed-starting-points boundary-points]
  (->> boundary-points
       (map #(get-nearest-starting-point indexed-starting-points %))
       (filter #(= (count %) 1))
       (apply concat)
       set))

(defn finite-area?
  "유한한 영역인지 체크
   input: #{0 1 2 5} 1
   output: false
   "
  [infinite-area-ids id]
  (if (infinite-area-ids id)
    false
    true))

(defn get-size-of-finite-areas
  "유한한 영역들을 return
   input: (1 1) (1 6) (8 3) (3 4) (5 5) (8 9)
   output: [3 9] [4 17]
   "
  [starting-points]
  (let [boundary (get-boundary starting-points)
        points-in-boundary (get-points-in-boundary boundary)
        boundary-points (get-boundary-points boundary)
        indexed-starting-points (map-indexed (fn [idx [x y]] {:id idx :x x :y y}) starting-points)
        nearest-starting-points-by-point (map #(get-nearest-starting-point indexed-starting-points %) points-in-boundary)
        infinite-area-ids (get-infinite-area-ids indexed-starting-points boundary-points)]
    (->> nearest-starting-points-by-point
         (filter #(= (count %) 1))
         (apply concat)
         frequencies
         (filter #(finite-area? infinite-area-ids (first %))))))


(defn get-size-of-max-finite-area
  "유한한 영역들 중 가장 면적이 큰 것의 size를 return
   input: [3 9] [4 17]
   output: 17
   "
  [finite-areas]
  (->> finite-areas
       (sort-by second)
       last
       last))

(defn sum-of-manhattan-distance
  "점으로 부터 모든 starting-point들의 거리의 합을 구하는 함수
   input: '({:id 1 :x 2 :y 2} {:id 2 :x 2 :y 3}) [2 2]
   output: 1
   "
  [starting-points point]
  (let [distances (map #(get-manhattan-distance point %) starting-points)]
    (->> distances
         (map :distance)
         (reduce +))))

;;1) 최소 영역을구하고
;;2) 영역 내의 모든좌표를 구하고
;;3) 각 좌표에 대해서 starting point의 거리를 구하고
;;4) 가장가까운 starting point를 구하고
;;5) starting point가 유일한지 검사
(comment (->> "aoc2018/day6.sample.txt"
              puzzle-input
              (map input->point)
              get-size-of-finite-areas
              get-size-of-max-finite-area
              )

         (let [starting-points (->> "aoc2018/day6.sample.txt"
                                    puzzle-input
                                    (map input->point))
               boundary (get-boundary starting-points)
               points-in-boundary (get-points-in-boundary boundary)
               indexed-starting-points (map-indexed (fn [idx [x y]] {:id idx :x x :y y}) starting-points)]
           (->> points-in-boundary
                (map #(get-size-of-safe-area indexed-starting-points %))
                (filter #(> 10000 %))
                count
                ))