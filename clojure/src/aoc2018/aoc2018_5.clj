(ns aoc2018_5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp))

(defn case-swap
  "대문자를 소문자로 소문자를 대문자로 변환시켜 주는 함수
   input: a
   output: A
   "
  [character]
  (let [char->int (int character)]
    (if (< char->int 97)
      (str/lower-case character) ;; lower-case/upper-case
      (str/upper-case character))))



(defn react
  "새로운 문자가 들어왔을 때 마지막 문자와 비교하여, 서로 같은 문자지만 
   1) 대/소가 다를 때 기존 vector의 마지막 문자를 제거 
   2) 그 외의 경우는 새로운 문자를 기존 vector의 맨 뒤에 추가
   
   case 1)
   input: dabAc C
   output: dabA
   
   case 2)
   input: dab A
   output: dabA
   "
  [result
   next-char]
  (let [cur (peek result)] ;; peek
    (if (zero? (count result))
      (conj result next-char)
      (if (= (case-swap cur) (str next-char))
        (pop result)
        (conj result next-char)))))


(def units "abcdefghijklmnopqrstuvwxyz")

(defn remove-by-unit
  "polymer에 있는 해당 unit (대소문자 모두)를 삭제하는 함수
   input: a dabAcCaCBAcCcaDA
   output: dbcCCBcCcD"
  [unit polymer]
  (-> polymer
      (str/replace (str unit) "")
      (str/replace (str (case-swap unit)) "")))


(defn get-polymer-length-by-unit ;;map을 두번하는 방식
  "각 unit으로 polymer에서 삭제하고 반응시킨 결과의 길이를 list로 출력하는 함수
   input: dabAcCaCBAcCcaDA
   output: 6 8 4 6 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10"
  [polymer]
  (->> units
       (map (fn [unit] ;; map 단위를 더 작게!!
              (let [removed-polymer (remove-by-unit unit polymer)]
                (count (reduce react [] removed-polymer)) ;;(6) (8) (6) (8)))) ;; (6) (8) (6) (8) (6) (8) (6) (8) 
                )))))

(get-polymer-length-by-unit "dabAcCaCBAcCcaDA")

(comment (->> "aoc2018/day5.sample.txt"
              puzzle-input
              (reduce react [])
              count)

         (let [polymer (->> "aoc2018/day5.sample.txt"
                            puzzle-input
                            (reduce react [])
                            str/join)]
           (->> polymer
                get-polymer-length-by-unit
                (apply min))))
;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.