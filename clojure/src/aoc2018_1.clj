(ns aoc2018-1
  (:require [clojure.java.io :as io] [clojure.string :as str]))
  
(def input-file (io/resource "day1.sample.txt"))
(def inputs (map 
             #(Integer/parseInt %) 
             (str/split (slurp input-file) #"\n")))

;; (def inputs `[3, 3, 4, -2, -4, 3, 3, 3])
;; ;; 파트 1
;; ;; 주어진 입력의 모든 숫자를 더하시오.
;; ;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(reduce + inputs)

;; ;; 파트 2
;; ;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; ;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; ;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
;; (def inputs-length (count inputs))
;; (loop [idx 0 sum 0 results #{0}]
;;   (let [sum (+ sum (nth inputs idx))]
;;     (if (contains? results sum)
;;       sum
;;       (recur (mod (+ idx 1) inputs-length) sum (conj results sum)))))
  
(loop [inputs (cycle inputs) sum 0 results #{0}]
  (let [sum (+ sum (first inputs))]
    (if (contains? results sum)
      sum
      (recur (next inputs) sum (conj results sum)))))

;; #################################
;; ###        Refactoring        ###
;; #################################

;; cycle 혹은 reductions 사용하기
;; loop-recur 시 let으로 바인딩하기