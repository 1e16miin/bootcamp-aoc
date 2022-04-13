(ns aoc2020_4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

(defn puzzle-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

(defn input->passport
  [input]
  (->> input
       (partition-by #(= "" %))
       (filter #(not= '("") %))
       (map #(str/join " " %))
       (map #(str/split % #" "))))

(def required-fields
  #{"byr" "ecl" "eyr" "hcl" "hgt" "iyr" "pid"})

(defn mapping-required-field
  ""
  [infos]
  (reduce
   (fn [result info]
     (let [field (first info)
           data (last info)]
       (conj result {field data})))
   {}
   infos))


(defn passport->fields
  "passport를 field를 key로 가지도록 각각의 value와 mapping
   input: [eyr:2023 pid:675106386 ecl:brn byr:2021 iyr:2011 hgt:153cm hcl:#888785]
   output: {eyr 2023 pid 675106386 ecl brn byr 2021 iyr 2011 hgt 153cm hcl #888785}"
  [passport]
  (->> passport
       (map #(str/split % #":"))
       mapping-required-field))

(comment (->> "aoc2020/day4.sample.txt"
              puzzle-input
              input->passport
              (map passport->fields)
              (map keys)
              (map set)
              (filter #(set/subset? required-fields %))
              count)
         (->> "aoc2020/day4.sample.txt"
              puzzle-input
              input->passport
              (map passport->fields)
              (map valid-passport?)
              (filter #(true? %))
              count))

(def ecl-type #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(def valid-byr?
  (partial s/int-in-range? 1920 2003))

(def valid-iyr?
  (partial s/int-in-range? 2010 2021))

(def valid-eyr?
  (partial s/int-in-range? 2020 2031))

(def valid-cm?
  (partial s/int-in-range? 150 194))

(def valid-in?
  (partial s/int-in-range? 59 77))

(defn valid-pid?
  [pid]
  (= (count pid) 9))

(defn valid-hgt?
  [hgt]
  (let [[_ height unit] (re-matches #"(\d+)(.*)$" hgt)]
    (if (= unit "cm")
      (valid-cm? (Integer/parseInt height))
      (valid-in? (Integer/parseInt height)))))

(defn valid-hcl?
  [hcl]
  (let [six-characters (re-seq #"[0-9a-f]" hcl)]
    (and (= (count six-characters) 6)
         (str/starts-with? hcl "#"))))

(def valid-ecl?
  (partial contains? ecl-type))


(defn valid-passport?
  [passport]
  (let [byr (passport "byr")
        ecl (passport "ecl")
        eyr (passport "eyr")
        hcl (passport "hcl")
        hgt (passport "hgt")
        iyr (passport "iyr")
        pid (passport "pid")]
    (try
      (and (valid-byr? (Integer/parseInt byr))
           (valid-ecl? ecl)
           (valid-eyr? (Integer/parseInt eyr))
           (valid-hcl? hcl)
           (valid-hgt? hgt)
           (valid-iyr? (Integer/parseInt iyr))
           (valid-pid? pid))
      (catch NullPointerException e
        (println "null pointer exception")
        nil)
      (catch NumberFormatException e
        (println "number format exception")
        nil))))