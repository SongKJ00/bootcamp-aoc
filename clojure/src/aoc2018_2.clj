(ns aoc2018-2
  (:require [clojure.java.io :as io] [clojure.string :as str]))

;; part1
(defn get-input-puzzle
  [filename]
  (let [input-file (io/resource filename)]
    (str/split (slurp input-file) #"\n")))

(defn get-frequencies-set
  "문자열에서 문자가 중복된 횟수에 대한 set을 제공하는 함수
   input: 문자열(box-id)
   output: 중복된 횟수에 대한 set"
  [box-id]
  (->> box-id
       frequencies
       vals
       set))

(defn contains-frequency
  "set에 찾고자 하는 값의 여부를 정수(1 or 0)로 반환하는 함수
   input: 중복 횟수 set, 찾고자 하는 값
   output: 값이 있는 경우) 1, 없는 경우) 0"
  [frequencies-set frequency]
  (if (frequencies-set frequency) 1 0))

(defn get-frequencies-info
  "set에서 찾고자 하는 값들의 존재 여부를 리스트로 반환
   input: 찾고자 하는 값 리스트, 중복 횟수 set
   output: ex) 인풋으로 [2, 3]이 들어오고 모두 있는 경우) - (1, 1)
                                     2만 있는 경우) - (1, 0)
                                     모두 없는 경우) - (0, 0)"
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
  "두 문자열들끼리 한 문자만 서로 다른지 체크하는 함수"
  [[s1 s2]]
  (->> (map not= s1 s2)
       (filter true?)
       count
       (= 1)))

(defn get-same-part
  "두 문자열간 같은 부분 문자열을 찾는 함수"
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
  "리스트에서 각 요소별 조합 가능한 모든 경우를 벡터로 만들어 반환
   ex) [1 2 3] -> [[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]]"
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