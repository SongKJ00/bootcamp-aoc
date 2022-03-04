(ns aoc2018-1
  (:require [clojure.java.io :as io] 
            [clojure.string :as str]))

(defn get-input-puzzle
  "문제의 인풋 데이터를 읽은 다음 파싱된 값을 제공하는 함수
   input: 인풋 데이터 파일 이름
   output: newline 문자 기준으로 split된 리스트"
  [filename]
  (let [input-file (io/resource filename)]
    (map #(Integer/parseInt %) (str/split (slurp input-file) #"\n"))))

;; part1
(defn solve-part1
  [inputs]
  (apply + inputs))

(comment
  (solve-part1 (get-input-puzzle "day1.sample.txt")))


;; part2
(defn find-first-duplicated
  "시퀀스의 가장 앞에서부터 최초로 중복 등장하는 값을 찾는 함수
   input: 시퀀스
   output: 가장 최초로 중복된 값"
  [inputs]
  (loop [inputs inputs
         visited #{0}]
    (let [curr-num (first inputs)]
      (if (visited curr-num)
        curr-num
        (recur (next inputs) (conj visited curr-num))))))


;; loop/recur -> reduce/reduced로 리팩토링
(defn find-first-duplicated
  "시퀀스의 가장 앞에서부터 최초로 중복 등장하는 값을 찾는 함수
   input: 시퀀스
   output: 가장 최초로 중복된 값"
  [inputs]
  (reduce
   (fn [visited curr-num]
     (if (visited curr-num)
        (reduced curr-num)
        (conj visited curr-num)))
   #{0}
   inputs))

(defn solve-part2
  [inputs]
  (find-first-duplicated (reductions + (cycle inputs))))

(comment
  (solve-part2 (get-input-puzzle "day1.sample.txt")))

;; part2 번외) 쓰레딩 매크로 활용해보기
(comment
  (->> "day1.sample.txt"
       get-input-puzzle
       cycle
       (reductions +)
       find-first-duplicated))