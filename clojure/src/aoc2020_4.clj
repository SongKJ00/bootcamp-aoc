(ns aoc2020-4
  (:require [clojure.java.io :as io] 
            [clojure.string :as string]))

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
  [inputs]
  (->> inputs
       (map #(string/replace % #"\n" " "))
       (map #(string/split % #" "))))

(defn blocks->passport
  [blocks]
  (let [passport (->> blocks
                      (map #(string/split % #":"))
                      (reduce (fn [acc [key value]] (assoc acc (keyword key) value)) {}))]
    (-> passport
        (change-to-int-val [:byr :iyr :eyr]))))

(defn change-to-int-val
  [passport fields]
  (reduce (fn [acc val] (if (acc val)
                          (update acc val #(Integer/parseInt %))
                          acc))
          passport
          fields))

(defn have-all-required-fields?
  [passport]
  (let [required-fields [:byr :iyr :eyr :hgt :hcl :ecl :pid]]
    (every? #(contains? passport %) required-fields)))

(defn get-passports-have-all-required-fields
  [passports]
  (->> passports
       (filter have-all-required-fields?)))

;; part1
(comment
  (->> (get-input-puzzle "2020/day4.sample.txt")
       parse
       get-passports-have-all-required-fields
       count))

;;part2
(defn all-fields-valid?
  [passport]
  ((every-pred
    byr-valid?
    iyr-valid?
    eyr-valid?
    hgt-valid?
    hcl-valid?
    ecl-valid?
    pid-valid?) passport))

(defn four-digits?
  [n]
  (int-in-range? (quot n 1000) [1 9]))

(defn int-in-range?
  [x [a b]]
  (and
   (>= x a)
   (<= x b)))

(defn byr-valid?
  [{:keys [:byr]}]
  (and
   (four-digits? byr)
   (int-in-range? byr [1920 2002])))

(defn iyr-valid?
  [{:keys [iyr]}]
  (and
   (four-digits? iyr)
   (int-in-range? iyr [2010 2020])))

(defn eyr-valid?
  [{:keys [eyr]}]
  (and
   (four-digits? eyr)
   (int-in-range? eyr [2020 2030])))

(defn hgt-valid?
  [{:keys [hgt]}]
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

(defn hcl-valid?
  [{:keys [hcl]}]
  (let [match (re-matches #"#[0-9a-f]{6}" hcl)]
    (some? match)))

(defn ecl-valid?
  [{:keys [ecl]}]
  (some #(= % ecl) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))

(defn pid-valid?
  [{:keys [pid]}]
  (let [nine-digit? (= (count pid) 9)
        pid-int (when nine-digit? (Integer/parseInt pid))]
    (and
     nine-digit?
     (= pid (format "%09d" pid-int)))))

;; part2
(comment
  (->> (get-input-puzzle "2020/day4.sample.txt")
       parse
       get-passports-have-all-required-fields
       (filter all-fields-valid?)
       count))