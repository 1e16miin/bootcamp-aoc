(ns aoc2018-8
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn puzzle-input
  [filename]
  (let [parsed (-> filename
                   io/resource
                   slurp
                   string/trim
                   (string/split #" "))]
    (mapv parse-long parsed)))

(declare create-node)

(defn create-tree
  [xs] 
  (let [[num-children num-metadata-entries & rest] xs
        [children xs] (create-node rest num-children [])
        [metadata xs] (split-at num-metadata-entries xs)]
    [{:children (if (empty? children) nil children) :metadata metadata} xs]))

(defn create-node
  [xs num-nodes acc]
  (if (zero? num-nodes)
    [acc xs]
    (let [[node xs] (create-tree xs)]
      (create-node xs (dec num-nodes) (conj acc node)))))

(defn solve-part1
  [filename]
  (let  [tree (-> filename
                  puzzle-input
                  create-tree
                  first)]
    (->> tree
         (tree-seq :children :children)
         (mapcat :metadata)
         (reduce +))))

(comment 
  (solve-part1 "aoc2018/day8.sample.txt"))
