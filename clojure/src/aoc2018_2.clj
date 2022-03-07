(ns aoc2018-2
  (:require [clojure.java.io :as io] [clojure.string :as str]))

;; part1
(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp
      (str/split #"\n")))

(defn get-frequencies-set
  "문자열에서 문자가 중복된 횟수에 대한 set을 제공하는 함수
   input: \"ivyhczwokexltwhsfamqprbnuy\"
   output: #{1 2}"
  [box-id]
  (->> box-id
       frequencies
       vals
       set))

(defn contains-frequency
  "set에 찾고자 하는 값의 여부를 반환하는 함수
   input: #{1 2}, 2
   output: 1"
  [frequencies-set frequency]
  (if (frequencies-set frequency) 1 0))

(defn get-frequencies-info
  "set에서 찾고자 하는 값들의 존재 여부를 리스트로 반환
   input: [2 3], #{1 2}
   output: (1 0)"
  [frequencies frequencies-set]
  (map (partial contains-frequency frequencies-set) frequencies))

(comment
  (->> (get-input-puzzle "day2.sample.txt")
       (map get-frequencies-set)
       (map (partial get-frequencies-info [2 3]))
       (apply map +) ; goodgood
       (apply *))
  
;; part1 리뷰 반영
(defn keep-only-n 
  "각 set마다 n을 포함하고 있으면 n을 keep함
   input: 2, (#{1 2} #{1 3} #{1 2})
   output: (2 2)"
  [n sets]
  (keep (fn [s] (s n)) sets))

(def keep-only-2 (partial keep-only-n 2))
(def keep-only-3 (partial keep-only-n 3))
  
(comment
  (->> (get-input-puzzle "day2.sample.txt")
       (map get-frequencies-set)
       ((juxt keep-only-2 keep-only-3))
       (map count)
       (apply *))
  )

;; part2
(defn only-one?
  "리스트의 엘리먼트 갯수가 하나만 있는지 체크하는 함수
  input: [a]
   output: true"
  [v]
  (= 1 (count v)))

(defn diff-only-one-letter
  "두 문자열들끼리 한 문자만 서로 다른지 체크하는 함수
   input: [\"fghij\" \"fguij\"]
   output: true"
  [[s1 s2]]
  (->> (map vector s1 s2)
       (filter (fn [[c1 c2]] (not= c1 c2)))
       only-one?))

;; loop/recur 사용 버전(초기)
(defn get-same-part
  "두 문자열간 같은 부분 문자열을 찾는 함수
   input: [\"fghij\" \"fguij\"]
   output: \"fgij\""
  [[s1 s2]]
  (loop [idx 0
         max-length (count s1)
         same-part ""]
    (if (< idx max-length)
      (if (= (nth s1 idx) (nth s2 idx))
        (recur (inc idx) max-length (str same-part (nth s1 idx)))
        (recur (+ idx 1) max-length same-part))
      same-part)))

;; reduce 사용 버전
(defn get-same-part
  "두 문자열간 같은 부분 문자열을 찾는 함수
   input: [\"fghij\" \"fguij\"]
   output: \"fgij\""
  [[s1 s2]]
  (->> (map vector s1 s2)
       (reduce (fn [same-part [c1 c2]] 
          (if (= c1 c2)
            (str same-part (str c1))
            same-part)) "")))

;; keep 사용 버전
(defn get-same-part
  "두 문자열간 같은 부분 문자열을 찾는 함수
   input: [\"fghij\" \"fguij\"]
   output: \"fgij\""
  [[s1 s2]]
  (->> (map vector s1 s2)
       (keep (fn [[c1 c2]] (when (= c1 c2) c1)))
       (apply str)))


(defn get-combination
  "리스트에서 각 요소별 조합 가능한 모든 경우를 벡터로 만들어 반환
   input: [1 2 3]
   output: [[1 2] [1 3] [2 1] [2 3] [3 1] [3 2]]"
  [box-ids]
  (for [x box-ids
        y box-ids
        :when (not= x y)]
    [x y]))

(comment
  (->> (get-input-puzzle "day2.sample.txt")
       get-combination
       (filter diff-only-one-letter)
       (map get-same-part)
       first)
  )
