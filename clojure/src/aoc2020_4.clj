(ns aoc2020-4
  (:require [clojure.java.io :as io] 
            [clojure.string :as string]
            [clojure.spec.alpha :as s]))

(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp
      (string/split #"\n\n")))

(defn parse
  [inputs]
  (->> inputs
       inputs->blocks
       (map blocks->passport)))

(defn inputs->blocks
  "인풋 데이터에서 파싱할 수 있는 블록 단위로 변환
   input: (\"ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\"
           \"iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\")
   output: ([\"ecl:gry\" \"pid:860033327\" \"eyr:2020\" \"hcl:#fffffd\"])
            [\"iyr:2013\" \"ecl:amb\" \"cid:350\" \"eyr:2023\" \"pid:028048884\"])"
  [inputs]
  (->> inputs
       (map #(string/replace % #"\n" " "))
       (map #(string/split % #" "))))

(defn blocks->passport
  "블록에서 passport 데이터를 맵으로 생성
   이 때, int로 파싱 가능한 데이터는 int로 파싱하여 제공
   input: [\"ecl:gry\" \"pid:860033327\" \"eyr:2020\" \"hcl:#fffffd\"]
   output: {:ecl \"gry\" :pid \"860033327\" :eyr 2020 :hcl \"#fffffd\"}"
  [blocks]
  (let [passport (->> blocks
                      (map #(string/split % #":"))
                      (reduce (fn [acc [key value]] (assoc acc (keyword key) value)) {}))]
    (-> passport
        (change-to-int-val [:byr :iyr :eyr]))))

(defn change-to-int-val
  "passport 데이터에서 특정 필드들은 int로 파싱하여 반환
   input: {:byr \"2020\" :pid \"860033327\" :eyr \"2020\" :hcl \"#fffffd\"}
          [:byr :eyr] 
   output: {:byr 2020 :pid \"860033327\" :eyr 2020 :hcl \"#fffffd\"}"
  [passport fields]
  (reduce (fn [acc val] (if (acc val)
                          (update acc val #(Integer/parseInt %))
                          acc))
          passport
          fields))

(defn have-all-required-fields?
  "passport가 필요한 모든 field들을 가지고 있는지 체크
   input: {:byr 2020 :iyr 2020 :eyr 2020 :hgt 20cm :hcl \"#fffffd\" :ecl \"gry\" :pid \"860033327\"}
   output: true
   
   input: {:iyr 2020 :eyr 2020 :hgt 20cm :hcl \"#fffffd\" :ecl \"gry\" :pid \"860033327\"}
   output: false"
  [passport]
  (let [required-fields [:byr :iyr :eyr :hgt :hcl :ecl :pid]]
    (every? #(contains? passport %) required-fields)))

(defn get-passports-have-all-required-fields
  "필요한 모든 field들을 가지고 있는 여권들만 반환
   input: ({:byr 2020 :iyr 2020 :eyr 2020 :hgt 20cm :hcl \"#fffffd\" :ecl \"gry\" :pid \"860033327\"}
           {:byr 2018 :iyr 2018 :eyr 2018 :hgt 20cm :hcl \"#fffffd\" :ecl \"gry\" :pid \"860033327\"}
           {:iyr 2020 :eyr 2020 :hgt 20cm :hcl \"#fffffd\" :ecl \"gry\" :pid \"860033327\"})
   output: ({:byr 2020 :iyr 2020 :eyr 2020 :hgt 20cm :hcl \"#fffffd\" :ecl \"gry\" :pid \"860033327\"}
            {:byr 2018 :iyr 2018 :eyr 2018 :hgt 20cm :hcl \"#fffffd\" :ecl \"gry\" :pid \"860033327\"})"
  [passports]
  (->> passports
       (filter have-all-required-fields?)))

;; part1
(comment
  (->> (get-input-puzzle "2020/day4.sample.txt")
       parse
       get-passports-have-all-required-fields
       count))

;; part2
(defn hgt-valid?
  "hgt 필드값이 valid한지 체크
   조건) 1) 맨 뒤에 cm 혹은 in 단위가 있어야 함
        2) cm인 경우 150 ~ 193 범위 안에 있어야 함
        3) in인 경우 59 ~ 76 범위 안에 있어야 함
   
   input: 193cm
   output: true
   
   input: 59in
   output: true
   
   input: 150
   output: false"
  [hgt]
  (let [unit (subs hgt (- (count hgt) 2))
        cm? (= unit "cm")
        in? (= unit "in")
        has-unit? (or cm? in?)
        hgt-int (when has-unit? (Integer/parseInt (subs hgt 0 (- (count hgt) 2))))]
    (and
     has-unit?
     (cond
       cm? (int-in-range? hgt-int [150 193])
       in? (int-in-range? hgt-int [59 76])))))

(defn pid-valid?
  "pid 필드값이 valid한지 체크
   조건) 9자리 숫자이고, 빈 자리는 0으로 채워져있어야 함
   input: \"000000001\"
   output: true
   
   input: \"0123456789\"
   output: false"
  [pid]
  (let [nine-digit? (= (count pid) 9)
        pid-int (when nine-digit? (Integer/parseInt pid))]
    (and
     nine-digit?
     (= pid (format "%09d" pid-int)))))

(s/def :passport/byr (s/int-in 1920 2003))
(s/def :passport/iyr (s/int-in 2010 2021))
(s/def :passport/eyr (s/int-in 2020 2031))
(s/def :passport/hgt hgt-valid?)
(s/def :passport/hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def :passport/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def :passport/pid pid-valid?)

(s/def :passport/passport (s/keys :req-un [:passport/byr
                                           :passport/iyr
                                           :passport/eyr
                                           :passport/hgt
                                           :passport/hcl
                                           :passport/ecl
                                           :passport/pid]))

(comment
  (->> (get-input-puzzle "2020/day4.sample.txt")
       parse
       get-passports-have-all-required-fields
       (filter #(s/valid? :passport/passport %))
       count))

;; clojure.spec
;; https://clojure.org/guides/spec

;; iterate / drop-while / take-while
;; ->> init-state (iterate x) (drop-while/take-while y) (agg...) (...) (...)
;; iteration -> 1.11.0
;; https://www.juxt.pro/blog/new-clojure-iteration