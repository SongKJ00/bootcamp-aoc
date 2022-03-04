(ns aoc2018_3
  (:require [clojure.java.io :as io] [clojure.string :as str]))

(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp
      (str/split #"\n")))

(defn parse-inputs
  [inputs]
  (let [blocks (map #(Integer/parseInt %) (drop 1 (str/split inputs #"#|( @ )|,|: |x")))] 
    {:id (nth blocks 0),
     :start-x (nth blocks 1),
     :start-y (nth blocks 2),
     :len-x (nth blocks 3),
     :len-y (nth blocks 4)}))

;; part 1
(defn get-coords
  [{:keys [start-x start-y len-x len-y]}]
  (for [x (range len-x) y (range len-y)] [(+ start-x x) (+ start-y y)]))

(defn get-duplicated-coords
  [inputs]
  (->> inputs
       (map get-coords)
       (apply concat)
       frequencies
       (filter #(> (second %) 1))
       (map first)))

(comment
  (->> (get-input-puzzle "day3.sample.txt")
       (map parse-inputs)
       (get-duplicated-coords)
       count))


;; part 2
(defn get-coords-with-id
  [input]
  {:id (:id input) :coords (get-coords input)})

(defn has-intersection
  [s1 s2]
  (= 0 (count (clojure.set/intersection s1 s2))))

(defn solve-part2
  [inputs]
  (let [duplicated-coords (get-duplicated-coords inputs)]
    (->> inputs
         (map get-coords-with-id)
         (filter (fn [v] (has-intersection (set (:coords v)) (set duplicated-coords))))
         first
         :id)))

(comment
  (->> (get-input-puzzle "day3.sample.txt")
       (map parse-inputs)
       (solve-part2)))