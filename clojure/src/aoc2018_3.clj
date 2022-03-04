(ns aoc2018_3
  (:require [clojure.java.io :as io] [clojure.string :as str]))

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
       (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
       rest
       (map #(Integer/parseInt %))))

(defn get-claim
  "파싱한 인풋으로 claim map 생성
   input: [947, 775, 93, 16, 14]
   output: {:id 947 :start-x 775 :start-y 93 :len-x 16 :len-y 14}"
  [[id start-x start-y len-x len-y]]
  {:id id
   :start-x start-x
   :start-y start-y
   :len-x len-x
   :len-y len-y})

;; (defn parse-inputs
;;   [inputs]
;;   (let [blocks (map #(Integer/parseInt %) (drop 1 (str/split inputs #"#|( @ )|,|: |x")))] 
;;     {:id (nth blocks 0),
;;      :start-x (nth blocks 1),
;;      :start-y (nth blocks 2),
;;      :len-x (nth blocks 3),
;;      :len-y (nth blocks 4)}))

;; part 1
(defn get-coords
  "claim에서 좌표 벡터 생성
   input: {:start-x 1 :start-y 1 :len-x 2 :len-y 2}
   output: [[1 1] [1 2] [2 1] [2 2]]"
  [{:keys [start-x start-y len-x len-y]}]
  (for [x (range len-x) y (range len-y)] [(+ start-x x) (+ start-y y)]))

(defn get-duplicated-coords
  "claim 리스트에서 겹치는 좌표 벡터 반환
   input: [{:start-x 1 :start-y 1 :len-x 2 :len-y 2}, {:start-x 2 :start-y 2 :len-x 2 :len-y 2}]
   output: [[2 1] [2 2]]"
  [inputs]
  (->> inputs
       (map get-coords)
       (apply concat)
       frequencies
       (filter #(> (second %) 1))
       (map first)))

(comment
  (->> (get-input-puzzle "day3.sample.txt")
       (map parse-input)
       (map get-claim)
       (get-duplicated-coords)
       count))


;; part 2
(defn get-coords-with-id
  "id와 해당 id의 좌표 벡터를 들고 있는 맵 생성
   input: {:id 1 :start-x 1 :start-y 1 :len-x 2 :len-y 2}
   output: {:id 1 :coords [[1 1] [1 2] [2 1] [2 2]]}"
  [input]
  {:id (:id input) :coords (get-coords input)})

(defn is-disjoint-sets
  "두 set이 서로소 집합인지 체크
   input: #{1 2} #{3 4}
   output: true"
  [s1 s2]
  (= 0 (count (clojure.set/intersection s1 s2))))

(defn solve-part2
  [inputs]
  (let [duplicated-coords (get-duplicated-coords inputs)]
    (->> inputs
         (map get-coords-with-id)
         (filter (fn [v] (is-disjoint-sets (set (:coords v)) (set duplicated-coords))))
         first
         :id)))

(comment
  (->> (get-input-puzzle "day3.sample.txt")
       (map parse-input)
       (map get-claim)
       (solve-part2)))