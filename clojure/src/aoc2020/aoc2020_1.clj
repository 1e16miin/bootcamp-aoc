(ns aoc2020_1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

(defn combinations [k seq]
  (cond
    (> k (count seq)) nil
    (= k (count seq)) [seq]
    (= 1 k) (map vector seq)
    :else (reduce concat (map-indexed
                          (fn [i x] (map #(cons x %) (combinations (dec k) (drop (inc i) seq))))
                          seq))))

(def combinations-2
  (partial combinations 2))

(def combinations-3
  (partial combinations 3))


(comment (let [input (->> "aoc2020/day1.sample.txt"
                          puzzle-input
                          (map #(Integer/parseInt %)))]
           (->> input
                combinations-2
                (filter #(= (apply + %) 2020))
                first
                (apply *)))
         (let [input (->> "aoc2020/day1.sample.txt"
                          puzzle-input
                          (map #(Integer/parseInt %)))]
           (->> input
                (combinations 3)
                (filter #(= (apply + %) 2020))
                first
                (apply *))))