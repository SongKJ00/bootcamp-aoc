(ns aoc2020-1
  (:require [clojure.java.io :as io] 
            [clojure.string :as string]))

(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp
      (string/split #"\n")))

(defn parse
  [inputs]
  (map #(Integer/parseInt %) inputs))

(defn get-combination-two
  "주어진 정수 리스트에서 두 개씩 묶을 때 가능한 모든 조합을 리턴
   input: (1 2 3)
   output: ([1 2] [1 3] [2 1] [2 3] [3 1] [3 2])"
  [entries]
  (for [x entries
        y entries
        :when (not= x y)]
    [x y]))

;; part1
(comment
  (let [combinations (-> (get-input-puzzle "2020/day1.sample.txt")
                         parse
                         get-combination-two)]
    (->> combinations
         (filter (fn [[x y]] (= (+ x y) 2020)))
         first
         (apply *))))
  
;; part2
(defn get-combination-three
  "주어진 정수 리스트에서 세 개씩 묶을 때 가능한 모든 조합을 리턴
   input: (1 2 3)
   output: ([1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1])"
  [entries]
  (for [x entries
        y entries
        z entries
        :when (and 
               (not= x y)
               (not= y z)
               (not= x z))]
    [x y z]))

(comment
  (let [combinations (-> (get-input-puzzle "2020/day1.sample.txt")
                         parse
                         get-combination-three)]
    (->> combinations
         (filter (fn [[x y z]] (= (+ x y z) 2020)))
         first
         (apply *))))