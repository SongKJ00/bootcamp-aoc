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

(defn find-two-entries-sum-to-2020
  "주어진 정수 리스트에서 두 개씩 묶어 서로 더했을 때 2020인 조합 리턴
   input: (1721 979 366 299 675 1456)
   output: ([1721 299] [299 1721])"
  [entries]
  (for [x entries
        y entries
        :when (= (+ x y) 2020)]
    [x y]))

;; part1
(comment
  (let [two-entries-sum-to-2020 (-> (get-input-puzzle "2020/day1.sample.txt")
                                    parse
                                    find-two-entries-sum-to-2020
                                    first)]
    (->> two-entries-sum-to-2020
         (apply *))))
  
;; part2
(defn find-three-entries-sum-to-2020
  "주어진 정수 리스트에서 세 개씩 묶어 서로 더했을 때 2020인 조합 리턴
   input: (1721 979 366 299 675 1456)
   output: ([1721 299] [299 1721])"
  [entries]
  (for [x entries
        y entries
        z entries
        :when (= (+ x y z) 2020)]
    [x y z]))

(comment
  (let [three-entries-sum-to-2020 (-> (get-input-puzzle "2020/day1.sample.txt")
                                      parse
                                      find-three-entries-sum-to-2020
                                      first)]
    (->> three-entries-sum-to-2020
         (apply *))))