(ns aoc2018-2
  (:require [clojure.java.io :as io] [clojure.string :as str]))

;; part1
(defn get-input-puzzle
  [filename]
  (let [input-file (io/resource filename)]
    (str/split (slurp input-file) #"\n")))

(defn get-frequencies-set
  [box-id]
  (->> box-id
       frequencies
       vals
       set))

(defn contains-frequency
  [frequencies-set frequency]
  (if (frequencies-set frequency) 1 0))

(defn get-frequencies-info
  [frequencies frequencies-set]
  (map (partial contains-frequency frequencies-set) frequencies))

;; (defn get-frequencies-info
;;   [frequencies frequencies-set]
;;   (list
;;    contains-frequency frequencies-set 2
;;    contains-frequency frequencies-set 3))

(comment
  (->> (get-input-puzzle "day2.sample.txt")
       (map get-frequencies-set)
       (map (partial get-frequencies-info [2 3]))
       (reduce (fn [x y] (map + x y)))
       (apply *)))

;; part2
(defn diff-only-one-letter
  [[s1 s2]]
  (->> (map not= s1 s2)
       (filter true?)
       count
       (= 1)))

(defn get-same-part
  [[s1 s2]]
  (loop [idx 0
         max-length (count s1)
         same-part ""]
    (if (< idx max-length)
      (if (= (nth s1 idx) (nth s2 idx))
        (recur (+ idx 1) max-length (str same-part (nth s1 idx)))
        (recur (+ idx 1) max-length same-part))
      same-part)))

(defn get-combination
  [box-ids]
  (for [x box-ids
        y box-ids
        :when (not= x y)]
    (vector x y)))

(comment
  (->> (get-input-puzzle "day2.sample.txt")
       (get-combination)
       (filter diff-only-one-letter)
       (map get-same-part)
       first))