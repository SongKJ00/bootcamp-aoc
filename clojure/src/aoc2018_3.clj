(ns aoc2018_3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp
      (str/split #"\n")))

(defn parse-input
  "re-find를 이용하여 인풋 파싱
   input: #947 @ 775,93: 16x14
   output: [947, 775, 93, 16, 14]"
  [input]
  (->> input
       (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
       rest
       (map #(Integer/parseInt %))))

(defn get-claim
  "파싱한 인풋으로 claim map 생성
   input: [947, 775, 93, 16, 14]
   output: {:id 947 :x 775 :y 93 :w 16 :h 14}"
  [[id x y w h]]
   {:id id
   :x x
   :y y
   :w w
   :h h})

;; part 1
(defn get-coords
  "claim에서 좌표 벡터 생성
   input: {:x 1 :y 1 :w 2 :h 2}
   output: [[1 1] [1 2] [2 1] [2 2]]"
  [{:keys [x y w h]}]
  (for [x (range x (+ x w))
        y (range y (+ y h))]
    [x y]))

(defn get-duplicated-coords
  "claim 리스트에서 겹치는 좌표 벡터 반환
   input: [{:x 1 :y 1 :w :h 2}, {:x 2 :y 2 :w 2 :h 2}]
   output: [[2 1] [2 2]]"
  [inputs]
  (->> inputs
       (mapcat get-coords)
       frequencies
       (filter #(> (second %) 1))
       (map first)))

(comment
  (->> (get-input-puzzle "day3.sample.txt")
       (map parse-input)
       (map get-claim)
       get-duplicated-coords
       count))

;; part 2
(defn get-coords-with-id
  "id와 해당 id의 좌표 벡터를 들고 있는 맵 생성
   input: {:id 1 :x 1 :y 1 :w 2 :h 2}
   output: {:id 1 :coords [[1 1] [1 2] [2 1] [2 2]]}"
  [input]
  {:id (:id input)
   :coords (get-coords input)})

(defn disjoint-sets?
  "두 set이 서로소 집합인지 체크
   input: #{1 2} #{3 4}
   output: true"
  [s1 s2]
  (zero? (count (intersection s1 s2))))

;; part2 무지성 풀이
(defn solve-part2
  [inputs]
  (let [duplicated-coords (get-duplicated-coords inputs)]
    (->> inputs
         (map get-coords-with-id)
         (filter (fn [v] (disjoint-sets? (set (:coords v)) (set duplicated-coords))))
         first
         :id)))

(comment
  (->> (get-input-puzzle "day3.sample.txt")
       (map parse-input)
       (map get-claim)
       (solve-part2)))

;; part2 리팩토링
(comment
  (let [claims (->> (get-input-puzzle "day3.sample.txt")
                    (map parse-input)
                    (map get-claim))
        duplicated-coords (get-duplicated-coords claims)]
    (->> claims
         (map get-coords-with-id)
         (filter #(disjoint-sets? (set (:coords %)) (set duplicated-coords)))
         (map #(:id %))
         first)))
  
