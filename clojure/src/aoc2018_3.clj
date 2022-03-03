(ns aoc2018_3
  (:require [clojure.java.io :as io] [clojure.string :as str]))


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
(defn get-input-puzzle
  [filename]
  (let [input-file (io/resource filename)]
    (str/split (slurp input-file) #"\n")))

(defn parse-inputs
  [inputs]
  (let [blocks (map #(Integer/parseInt %) (drop 1 (str/split inputs #"#|( @ )|,|: |x")))] 
    {:id (nth blocks 0),
     :start-x (nth blocks 1),
     :start-y (nth blocks 2),
     :len-x (nth blocks 3),
     :len-y (nth blocks 4)}))

(defn get-used-coords
  [{:keys [start-x start-y len-x len-y]}]
  (for [x (range len-x) y (range len-y)] [(+ start-x x) (+ start-y y)]))

(comment
  (->> (get-input-puzzle "day3.sample.txt")
       (map parse-inputs)
       (map get-used-coords)
       (apply concat)
       frequencies
       vals
       (filter #(> % 1))
       count))


;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)