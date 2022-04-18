(ns aoc2020_8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

(defn input->instruction
  "input을 operation과 argument로 분할
   input: nop -3
   output: {:operation nop :arg -3}
   "
  [line]
  (let [[_ operation arg] (re-matches #"(\w+) (.*)$" line)]
    {:operation operation :arg (Integer/parseInt arg)}))

(defn indexed-instructions
  "각 instruction에 index 부여
   input: {:operation nop :arg -3} {:operation acc :arg 1}
   ouput: {:operation nop :arg -3 :idx 0} {:operation acc :arg 1 :idx 1}
   "
  [instructions]
  (map-indexed (fn [idx item] (conj {:idx idx} item)) instructions))


(defn parse-instruction
  "instruction을 같은 형태로 parsing
   ex) {:operation nop, :arg 0, :idx 0} => {:idx 0 :arg 0 :moves 1}
       {:operation acc, :arg 1, :idx 1} => {:idx 1 :arg 1 :moves 1}
       {:operation jmp, :arg 4, :idx 2} => {:idx 2 :arg 0 :moves 4}
   "
  [{:keys [idx operation arg]}]
  (cond (= operation "acc") {:idx idx :arg arg :moves 1}
        (= operation "nop") {:idx idx :arg 0 :moves 1}
        (= operation "jmp") {:idx idx :arg 0 :moves arg}))



;;iterate drop-while
(defn calculate
  ""
  [instructions {:keys [acc visited cur-idx]}]
  (let [{:keys [arg moves]} (get instructions cur-idx)]
    (if (visited cur-idx)
      {:acc (+ arg acc)
       :visited (visited cur-idx)
       :cur-idx (+ cur-idx moves)
       :dup-visited true}
      {:acc (+ arg acc)
       :visited (conj visited cur-idx)
       :cur-idx (+ cur-idx moves)
       :dup-visited false})))


;; (reduce
;;      (fn [{:keys [acc next-idx visited]} {:keys [operation arg idx]}]
;;        (cond (= next-idx idx) (if (visited idx)
;;                                 (reduced {:acc acc :finish-type "visit-dup-idx"})
;;                                 (cond
;;                                   (= operation "acc") {:acc (+ acc arg) :next-idx (inc idx) :visited (conj visited idx)}
;;                                   (= operation "nop") {:acc acc :next-idx (inc idx) :visited (conj visited idx)}
;;                                   (= operation "jmp") {:acc acc :next-idx (+ idx arg) :visited (conj visited idx)}))
;;              (>= next-idx length) (reduced {:acc acc :finish-type "loop-finished"})
;;              :else {:acc acc :next-idx next-idx :visited visited}))
;;      {:acc 0 :next-idx 0 :visited #{}}
;;      (cycle instructions)))

(defn change-operation
  "jmp->nop or nop->jmp로 변경
   input: jmp
   output: nop
   "
  [{:keys [idx operation arg]}]
  (if (= operation "jmp")
    {:idx idx :operation "nop" :arg arg}
    {:idx idx :operation "jmp" :arg arg}))

(defn change-operation-instructions
  [instructions jmp-or-nop-instruction]
  (let [idx (jmp-or-nop-instruction :idx)
        changed-instruction (change-operation jmp-or-nop-instruction)]
    (->> instructions
         (filter #(not= (% :idx) idx))
         (cons changed-instruction)
         (sort-by :idx))))

(defn get-result-of-before-step
  [instructions {:keys [acc cur-idx]}]
  (let [{:keys [arg]} (get instructions cur-idx)]
    arg))

(defn part1
  [instructions]
  (let [init-values {:acc 0 :visited #{} :cur-idx 0 :dup-visited false}]
    (->> init-values
         (iterate #(calculate instructions %))
         (drop-while #(% :dup-visited))
        ;;  first
        ;;  (get-result-of-before-step instructions)
         )))

;; (def init-values {:acc 0 :visited #{} :cur-idx 0})

(defn part2
  [instructions])

(comment
  (let [parsed-instructions (->> "aoc2020/day8.sample.txt"
                                 puzzle-input
                                 (map input->instruction)
                                 indexed-instructions
                                 (map #(parse-instruction %))
                                 vec)]
    (->> parsed-instructions
         part1))
  (let [instructions (->> "aoc2020/day8.sample.txt"
                          puzzle-input
                          (map input->instruction)
                          indexed-instructions)
        jmp-or-nop-instructions (filter #(not= "acc" (:operation %)) instructions)
        instructions-iteration (map #(change-operation-instructions instructions %) jmp-or-nop-instructions)
        length (count instructions)]
    (->> instructions-iteration
         (map (fn [inst] (map parse-instruction inst)))
         (map vec)
         (map #(accumulator %))
        ;;  (drop-while #(= (% :cur-idx) length))
         )))