(ns aoc2018_6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp
      (string/split #"\n")))

(defn get-start-coords-with-id
  "인풋 데이터로부터 시작 좌표들을 id와 함께 가져오는 함수
   input:  (\"1, 1\" \"1, 6\" \"8, 3\")
   output: ({:id 0 :x 1 :y 1} {:id 1 :x 1 :y 6} {:id 2 :x 8 :y 3})"
  [inputs]
  (->> inputs
       (map input->coord)
       (coords->start-coords-with-id)))

(defn input->coord
  "인풋 데이터를 좌표로 변환
   input: \"1 1\"
   output: (1 1)"
  [input]
  (let [match (re-matches #"(\d+), (\d+)" input)]
    (->> match
         rest
         (map #(Integer/parseInt %)))))

(defn coords->start-coords-with-id
  "좌표 리스트들을 id가 담긴 맵으로 묶어 반환
   input: ((1 1) (2 2))
   output: ({:id 0 :x 1 :y 1} {:id 1 :x 2 :y 2})"
  [coords]
  (map-indexed (fn [idx [x y]] ; destructuring
                 {:id idx
                  :x x
                  :y y}) coords))

(defn get-border-coords
  "x, y 최솟값 최댓값을 받아 해당 범위 안에 있는 좌표들을 반환
   input: {:min-x 1 :min-y 1 :max-x 3 :max-y 3}
   output: ([1 1] [1 2] [1 3] [2 1] [2 2] [2 3] [3 1] [3 2] [3 3])"
  [{:keys [min-x min-y max-x max-y]}]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    [x y]))

(defn find-min-max-coords
  "주어진 좌표들에서 x, y 최솟값 최댓값을 반환
   input: ({:x 1 :y 1} {:x 1 :y 3} {:x 3 :y 1})
   output: {:min-x 1 :min-y 1 :max-x 3 :max-y 3}"
  [coords]
  (let [xs (map :x coords)
        ys (map :y coords)]
    {:min-x (apply min xs)
     :min-y (apply min ys)
     :max-x (apply max xs)
     :max-y (apply max ys)}))

(defn get-distances-from-start-coords
  "특정 좌표을 기준으로 하여 시작 좌표들로부터의 거리를 맵으로 담아 리턴
   input: ({:id 0 :x 1 :y 1} {:id 1 :x 2 :y 2} {:id 2 :x 3 :y 3})
          [1 1]
   output: {:x 1 :y 1 :distances-from-start-coords ({:id 0 :distance 0} 
                                                    {:id 1 :distance 2} 
                                                    {:id 2 :distance 4})}"
  [start-coords-with-id [x y]]
  (let [distances-from-start-coords (->> start-coords-with-id
                                         (map (fn [{start-coord-id :id
                                                    start-coord-x :x
                                                    start-coord-y :y}] ;; 구조분해!!
                                                {:id start-coord-id
                                                 :distance (get-manhattan-distance
                                                            [x y]
                                                            [start-coord-x start-coord-y])})))]
    {:x x
     :y y
     :distances-from-start-coords distances-from-start-coords}))

(defn get-manhattan-distance
  "두 좌표간 맨해튼 거리 반환
   input: [1 1] [3 3]
   output: 4"
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn abs
  "절댓값 계산
   input: 1
   output: 1
   
   input: -1
   output: 1"
  [n]
  (max n (- n)))

(defn get-closest-ids
  "특정 좌표를 기준으로 하여, 시작 좌표로부터의 거리값들이 주어졌을 때, 가장 가까운 id들을 찾아 리턴
   input: {:x 1 :y 1 :distances-from-start-coords ({:id 0 :distance 0} 
                                                   {:id 1 :distance 2} 
                                                   {:id 2 :distance 2})}
   output: {:x 1 :y 1 :closest-ids (1 2)}"
  [{:keys [x y distances-from-start-coords]}]
  (let [closest-distance (->> distances-from-start-coords
                              (map :distance)
                              (apply min))
        closest-ids (->> distances-from-start-coords
                         (filter #(= (:distance %) closest-distance))
                         (map :id))]
    {:x x
     :y y
     :closest-ids closest-ids}))

(defn get-coords-which-have-only-one-closest-id
  "가장 가까운 id가 단 하나만 있는 좌표만 리턴
   input: ({:x 1 :y 1 :distances-from-start-coords ({:id 0 :distance 0} 
                                                    {:id 1 :distance 1} 
                                                    {:id 2 :distance 2})}
           {:x 2 :y 2 :distances-from-start-coords ({:id 0 :distance 2} 
                                                    {:id 1 :distance 1} 
                                                    {:id 2 :distance 0})}
           {:x 3 :y 3 :distances-from-start-coords ({:id 0 :distance 2} 
                                                    {:id 1 :distance 2} 
                                                    {:id 2 :distance 5})})
   output: ({:x 1 :y 1 :closest-id 0}
            {:x 2 :y 2 :closest-id 2)"
  [distance-from-start-coords]
  (let [closest-ids (->> distance-from-start-coords
                         (map get-closest-ids))]
  (->> closest-ids
       (filter #(= 1 (count (:closest-ids %))))
       (map (fn [{:keys [x y closest-ids]}] 
              {:x x :y y :closest-id (first closest-ids)})))))

(defn get-each-ids-closest-coords
  "각 id별로 가장 가까운 좌표 리스트들을 담아 리턴
   단, 특정 좌표에 여러 id가 가장 가까운 경우는 배제
   input: ({:x 1 :y 1 :distances-from-start-coords ({:id 0 :distance 0} 
                                                    {:id 1 :distance 1} 
                                                    {:id 2 :distance 2})}
           {:x 2 :y 2 :distances-from-start-coords ({:id 0 :distance 2} 
                                                    {:id 1 :distance 1} 
                                                    {:id 2 :distance 0})}
           {:x 3 :y 3 :distances-from-start-coords ({:id 0 :distance 2} 
                                                    {:id 1 :distance 2} 
                                                    {:id 2 :distance 5})})
   output: ({:id 0 :closest-coords [{:x 1 :y 1}]}
            {:id 2 :closest-coords [{:x 3 :y 3}]})"
  [distance-from-start-coords]
  (let [coords-have-only-one-closest-id (get-coords-which-have-only-one-closest-id distance-from-start-coords)]
    (->> coords-have-only-one-closest-id
         (group-by :closest-id)
         (map (fn [[id coords]] 
                {:id id 
                 :closest-coords (map (fn [{:keys [x y]}] {:x x :y y}) coords)})))))

(defn finite-id?
  "유한하게 확장할 수 있는 id인지 체크
   보유하고 있는 좌표 중 하나라도 min이나 max와 같다면 유한하게 확장할 수 없음.
   input: {:min-x 0 :min-y 0 :max-x 5 :max-y 5}
          {:closest-coords [{:x 1 :y 1} {:x 3 :y 3}]}
   output: false
   
   input: {:min-x 0 :min-y 1 :max-x 5 :max-y 5}
          {:closest-coords [{:x 0 :y 1} {:x 3 :y 3}]}
   output: true"
  [{:keys [min-x min-y max-x max-y]} {:keys [closest-coords]}]
  (let [coords-at-border-edges (->> closest-coords
                                    (filter (fn [{:keys [x y]}]
                                              (or
                                               (and (= x min-x) (= y min-y))
                                               (and (= x max-x) (= y max-y))))))]
    (-> coords-at-border-edges
        count
        zero?)))

(comment
  (let [inputs (get-input-puzzle "day6.sample.txt")
        start-coords-with-id (get-start-coords-with-id inputs)
        min-max-coords (find-min-max-coords start-coords-with-id)
        border-coords (get-border-coords min-max-coords)
        distances-from-start-coords (->> border-coords
                                         (map (partial get-distances-from-start-coords start-coords-with-id)))
        each-ids-closest-coords (get-each-ids-closest-coords distances-from-start-coords)]
    (->> each-ids-closest-coords
         (filter (partial finite-id? min-max-coords))
         (map :closest-coords)
         (map count)
         (apply max))))

;; part 2
(defn sum-distances-from-start-coords
  [distances-from-start-coords]
  (->> distances-from-start-coords
       (map :distance)
       (apply +)))

(comment
  (let [inputs (get-input-puzzle "day6.sample.txt")
        start-coords-with-id (get-start-coords-with-id inputs)
        min-max-coords (find-min-max-coords start-coords-with-id)
        border-coords (get-border-coords min-max-coords)
        distances-from-start-coords (->> border-coords
                                         (map (partial get-distances-from-start-coords start-coords-with-id)))]
    (->> distances-from-start-coords
         (map :distances-from-start-coords)
         (map sum-distances-from-start-coords)
         (filter #(< % 10000))
         count)))