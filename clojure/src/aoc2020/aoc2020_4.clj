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
  "input을 empty string을 기준으로 partition을 만듦
   
   input: hcl:#341e13 ecl:lzr eyr:2024 iyr:2014 pid:161cm byr:1991 cid:59 hgt:150cm
          
          iyr:2018 eyr:2027
          hgt:153cm
          pid:642977294 ecl:gry hcl:#c0946f byr:1999
   
   output: [hcl:#341e13 ecl:lzr eyr:2024 iyr:2014 pid:161cm byr:1991 cid:59 hgt:150cm]
           [iyr:2018 eyr:2027 hgt:153cm pid:642977294 ecl:gry hcl:#c0946f byr:1999]
   "
  [input]
  (->> input
       (partition-by #(= "" %))
       (filter #(not= '("") %))
       (map #(str/join " " %))
       (map #(str/split % #" "))))

(def required-fields
  #{:byr :ecl :eyr :hcl :hgt :iyr :pid})

(defn mapping-required-field
  "[field value] 형태의 list를 {field1 value1 field2 value2 ...} 형태로 변환
   input:[eyr 2023] [pid 675106386] [ecl brn] [byr 2021] [iyr 2011] [hgt 153cm] [hcl #888785]
   output: {eyr 2023 pid 675106386 ecl brn byr 2021 iyr 2011 hgt 153cm hcl #888785}"
  [infos]
  (reduce
   (fn [result info]
     (let [field (first info)
           data (last info)]
       (conj result {(keyword field) data})))
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


(defn part1
  [input]
  (->> input
       input->passport
       (map passport->fields)
       (map keys)
       (map set)
       (filter #(set/subset? required-fields %))
       count))


(def ecl-type #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(def valid-ecl?
  (partial contains? ecl-type))

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



(defn nil-not-in-fields?
  "field의 값들 중 nil이 존재하는지 않는지 확인
   input: [2002 amd #asd123 158cm 2022 012345667]
   ouput: true
   "
  [fields]
  (nil? (some nil? fields)))


(s/def :passport/byr valid-byr?)
(s/def :passport/ecl valid-ecl?)
(s/def :passport/eyr valid-eyr?)
(s/def :passport/hcl valid-hcl?)
(s/def :passport/hgt valid-hgt?)
(s/def :passport/iyr valid-iyr?)
(s/def :passport/pid valid-pid?)

(s/def :passport/validation
  (s/keys :req [:passport/byr
                :passport/ecl
                :passport/eyr
                :passport/hcl
                :passport/hgt
                :passport/iyr
                :passport/pid]))

(defn valid-all-fields?
  "passport에 필수 field가 모두 존재하는지 확인하고, 각 필드의 유효성을 검증
   input: {:hcl #341e13 :ecl lzr :eyr 2024 :iyr 2014 :pid 161cm :byr 1991 :cid 59 :hgt 150cm}
   output: false (pid가 유효하지 않음)
   "
  [{:keys [byr ecl eyr hcl hgt iyr pid]}]
  (if (every? some? [byr ecl eyr hcl hgt iyr pid]) ;; 모든게 nil이 아니여야한다! => every? some?
    (s/valid? :passport/validation
              {:passport/byr (Integer/parseInt byr)
               :passport/ecl ecl
               :passport/eyr (Integer/parseInt eyr)
               :passport/hcl hcl
               :passport/hgt hgt
               :passport/iyr (Integer/parseInt iyr)
               :passport/pid pid})
    false))


(defn part2
  [input]
  (->> input
       input->passport
       (map passport->fields)
       (filter valid-all-fields?)
       count))

(comment (->> "aoc2020/day4.sample.txt"
              puzzle-input
              part1)
         (->> "aoc2020/day4.sample.txt"
              puzzle-input
              part2))