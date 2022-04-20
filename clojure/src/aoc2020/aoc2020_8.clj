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


;; (defn parse-instruction
;;   "instruction을 같은 형태로 parsing
;;    ex) {:operation nop, :arg 0, :idx 0} => {:idx 0 :arg 0 :moves 1}
;;        {:operation acc, :arg 1, :idx 1} => {:idx 1 :arg 1 :moves 1}
;;        {:operation jmp, :arg 4, :idx 2} => {:idx 2 :arg 0 :moves 4}
;;    "
;;   [{:keys [operation arg]}]
;;   (case operation
;;     "acc" {:arg arg :moves 1} ;; keyword
;;     "nop" {:arg 0 :moves 1}
;;     "jmp" {:arg 0 :moves arg})) ;; case


(defmulti parse-instruction
  (fn [{:keys [operation]}] operation))

(defmethod parse-instruction "acc"
  [{:keys [arg]}]
  {:arg arg :moves 1})

(defmethod parse-instruction "nop"
  [_]
  {:arg 0 :moves 1})

(defmethod parse-instruction "jmp"
  [{:keys [arg]}]
  {:arg 0 :moves arg})


(defn calculate
  [instructions {:keys [acc visited cur-idx]}]
  (let [{:keys [arg moves]} (get instructions cur-idx)]
    (if (nil? arg)
      {:acc acc
       :visited visited
       :cur-idx cur-idx
       :finished? true}
      (if (visited cur-idx)
        {:acc (+ arg acc)
         :visited (conj visited cur-idx)
         :cur-idx (+ cur-idx moves)
         :finished? true}
        {:acc (+ arg acc)
         :visited (conj visited cur-idx)
         :cur-idx (+ cur-idx moves)
         :finished? false}))))

(defn change-operation
  "jmp->nop or nop->jmp로 변경
   input: {:operation nop :arg -3}
   output: {:operation jmp :arg -3}
   "
  [instruction]
  (let [convert {"jmp" "nop" "nop" "jmp"}]
    (update instruction :operation convert)))

(defn change-operation-instructions
  [instructions jmp-or-nop-instruction]
  (let [idx (jmp-or-nop-instruction :idx)
        changed-instruction (change-operation jmp-or-nop-instruction)]
    (-> instructions
        (assoc idx changed-instruction))))


(defn calculator
  [instructions]
  (let [init-values {:acc 0 :visited #{} :cur-idx 0 :finished? false}]
    (->> init-values
         (iterate #(calculate instructions %))
         (drop-while #(false? (% :finished?)))
         first)))

(comment
  (let [parsed-instructions (->> "aoc2020/day8.sample.txt"
                                 puzzle-input
                                 (map input->instruction)
                                 indexed-instructions
                                 (mapv #(parse-instruction %))  ;; mapv {0 {:arg :moves}}
                                 )]
    (->> parsed-instructions
         calculator
         :acc))
  (let [instructions (->> "aoc2020/day8.sample.txt"
                          puzzle-input
                          (map input->instruction)
                          indexed-instructions
                          vec)
        jmp-or-nop-instructions (filter #(not= "acc" (:operation %)) instructions)
        instructions-iteration (map #(change-operation-instructions instructions %) jmp-or-nop-instructions)
        length (count instructions)]
    (->> instructions-iteration
         (map #(mapv parse-instruction %)) ;; mapv
         (map calculator)
         (filter #(>= (:cur-idx %) length))
         first
         :acc)))