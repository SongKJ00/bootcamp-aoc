(ns aoc2018_7
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn get-input-puzzle
  [filename]
  (-> filename
      io/resource
      slurp
      string/split-lines))

(defn input->step-dep
  "인풋 데이터로부터 각 step과 그 이전에 처리되어야 할 step으로 이뤄진 맵으로 변환
   input: Step C must be finished before step A can begin.
   output: {:prev C
            :step A}"
  [input]
  (let [match (re-matches
               #"Step ([A-Z]) must be finished before step ([A-Z]) can begin."
               input)
        [prev step] (rest match)]
    {:prev prev
     :step step}))

(defn step-deps->graph
  "step들 간의 처리 순서 관계로부터 그래프 생성
   각 key는 이전에 미리 처리 되어야할 step들을 value로 들고 있음
   input: [{:prev C :step A} {:prev C :step F}]
   output: {C #{}, A #{C}, F #{C}}"
  [step-deps]
  (let [steps (->> step-deps
                   (mapcat vals)
                   set)
        init-graph (reduce (fn [acc v]
                         (assoc acc v #{}))
                       {}
                       steps)]
    (reduce (fn [acc {:keys [prev step]}]
              (update acc step #(conj % prev)))
            init-graph
            step-deps)))

(defn parse
  "인풋데이터 파싱하여 그래프 생성
   input: (Step C must be finished before step A can begin.
           Step C must be finished before step F can begin.)
   output: {C #{}, A #{C}, F #{C}}"
  [inputs]
  (->> inputs
       (map input->step-dep)
       step-deps->graph))

(defn get-max-available-steps
  "현재 최대로 처리 가능한 step 리스트 반환
   input: {:finished #{}
           :graph {C #{}, A #{C}, F #{C}}}
          1
   output: (C)
   
   input: {:finished #{C}
           :graph {A #{C}, D #{C}, F #{C}, G #{F}}
          2
   output: (A D)"
  [{:keys [finished graph]} available-workers-num]
  (let [available-steps (if (empty? finished)
                          (filter (fn [[step prevs]] (empty? prevs)) graph)
                          (filter (fn [[step prevs]] (set/subset? prevs finished)) graph))]
    (->> available-steps
         (map first)
         sort
         (take available-workers-num))))

(defn get-process-time
  "step의 처리 시간 반환
   input: A 60
   output: 61
   
   input: C 60
   output: 63"
  [step offset]
  (let [step-ch (first step)]
    (-> (- (int step-ch) (int \A))
        inc
        (+ offset))))

(defn remove-finished-works
  "workers에서 처리 완료된 step들 삭제
   input: {:time 3
           :finished #{}
           :workers [{:end-sec 3 :step A}
                     {:end-sec 5 :step B}]}
   output: {:time 3
            :finished #{A}
            :workers [{end-sec 5 :step B}]}"
  [{:keys [time workers] :as state}]
  (let [finished-steps (->> workers
                            (filter (fn [{:keys [end-sec]}] (>= time end-sec)))
                            (map :step))
        new-workers (->> workers
                         (remove (fn [{:keys [step]}] 
                                   (some #(= % step) finished-steps))))]
    (-> state
        (update :finished #(apply conj % finished-steps))
        (assoc :workers new-workers))))

(defn alloc-works
  "workers에 처리 가능한 steps 할당
   input: {:time 63
           :finished #{C}
           :order [C]
           :graph {A #{C}, F #{C}, G #{F}}
           :offset 61
           :workers-num 2
           :workers []}
   output: {:time 63
            :finished #{C}
            :order [C A F]
            :graph {G #{F}}
            :offset 61
            :workers-num 2
            :workers [{:end-sec 124 :step A} {:end-sec 128 :step F}]}"
  [{:keys [time offset workers-num workers] :as state}]
  (let [available-workers-num (- workers-num (count workers))
        available-steps (get-max-available-steps state available-workers-num)
        new-workers (->> available-steps
                         (map (fn [v]
                                {:end-sec (+ time (get-process-time v offset))
                                 :step v})))]
    (-> state
        (update :workers #(concat % new-workers))
        (update :order #(concat % available-steps))
        (update-in [:graph] #(apply dissoc % available-steps)))))
    
(defn get-next-state
  "1초가 지난 시점에서 workers에서 처리 완료된 step들을 삭제하고,
   새로운 step을 할당하면서 state 변경
   input: {:time 62
           :finished #{}
           :order [C]
           :graph {A #{C}, F #{C}, B #{A}, D #{A}, E #{F B D}}
           :steps-num 6
           :offset 60
           :workers-num 2
           :workers [{:end-sec 63 :step C}]}
   output: {:time 63
            :finished #{C}
            :order [C A F]
            :graph {B #{A}, D #{A}, E #{F B D}}
            :steps-num 6
            :offset 60
            :workers-num 2
            :workers [{:end-sec 124 :step A} {:end-sec 128 :step F}]}"
  [state]
  (-> state
      (update :time inc)
      remove-finished-works
      alloc-works))

(defn create-init-state
  "초기 state 생성
   input: {:offset 0 :workers-num 1}
          {A #{C}, F #{C}, B #{A}, D #{A}, E #{F B D}}
   output: {:time -1
            :finished #{}
            :order []
            :graph {A #{C}, F #{C}, B #{A}, D #{A}, E #{F B D}}
            :steps-num 6
            :offset 0
            :workers-num 1
            :workers []}"
  [{:keys [offset workers-num]} graph]
  {:time -1
   :finished #{}
   :order []
   :graph graph
   :steps-num (count graph)
   :offset offset
   :workers-num workers-num
   :workers []})

(defn processing?
  [{:keys [finished steps-num]}]
  (not= (count finished) steps-num))

(comment
  ;; part1
  (let [graph (-> (get-input-puzzle "day7.sample.txt")
                  parse)
        init-state (create-init-state {:offset 0 :workers-num 1} graph)]
    (->> init-state
         (iterate get-next-state)
         (drop-while processing?)
         first
         :order
         string/join))
  
  ;; part2
  (let [graph (-> (get-input-puzzle "day7.sample.txt")
                  parse)
        init-state (create-init-state {:offset 60 :workers-num 5} graph)]
    (->> init-state
         (iterate get-next-state)
         (drop-while processing?)
         first
         :time)))
