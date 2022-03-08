(ns aoc2018_5
  (:require [clojure.java.io :as io] 
            [clojure.string :as string]))

(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp))

(defn abs
  "주어진 숫자의 절댓값을 리턴하는 함수
   input: -3
   output: 3
   
   input: 3
   output: 3"
  [n] 
  (max n (- n)))

(defn diff-only-case?
  "두 문자가 서로 case만 다른지 체크하는 함수
   input: \a \A
   output: true

   input: \a \a
   output: false

   input: \a \b
   output: false
   
   input: \a \B
   output: false"
  [a b]
  (= 
   (abs (- (int a) (int b))) 
   32))

(defn react
  "문자열을 반응시켜 인접한 두 문자가 서로 case만 다른 경우 해당 두 문자는 삭제하는 함수
   input: \"abBA\"
   output: \"\"
   
   input: \"abAB\"
   output: \"abAB\"
   
   input: \"aabAAB\"
   output: \"aabAAB\"
   
   input: \"dabAcCaCBAcCcaDA\"
   output: \"dabCBAcaDA\""
  [s]
  (reduce (fn [acc v] 
            (let [last-ch (last acc)]
              (cond
                (empty? acc) (conj acc v)
                (diff-only-case? last-ch v) (pop acc)
                :else (conj acc v)))) [] (into [] s)))

;; part1
(comment
  (->> (get-input-puzzle "day5.sample.txt")
       react
       count))

(defn find-all-chars-with-lower-case
  "주어진 문자열에서 등장하는 모든 문자를 소문자 case로 강제 변환해서 반환
   input: \"abBA\"
   output: [\\a \\b]
   
   input: \"dabCBAcaDA\"
   output: [\\d \\a \\b \\c]"
  [s]
  (->> s
       (map string/lower-case)
       set
       (into [])))

(defn remove-chars-with-lower-case
  "문자열에서 등장하는 모든 문자를 소문자 case로 바꾸어, 주어진 문자와 일치하는 경우 해당 문자 제거
   input: \"abBa\"
          \\b
   output: \"aa\"
   
   input: \"dabCBAcaDA\"
          \\c
   output: \"dabBAaDa"
  [s c]
  (reduce (fn [acc v]
            (if (= (string/lower-case v) c)
              acc
              (str acc v))) "" s))

;; part2
(comment
  (let [input-str (get-input-puzzle "day5.sample.txt")
        chars-with-lower-case-in-input-str (find-all-chars-with-lower-case input-str)
        char-removed-input-strs (for [c chars-with-lower-case-in-input-str]
                                  (remove-chars-with-lower-case input-str c))]
    (->> char-removed-input-strs
         (map react)
         (map count)
         (apply min))))