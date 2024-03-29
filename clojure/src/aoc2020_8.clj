(ns aoc2020_8
  (:require [clojure.java.io :as io] 
            [clojure.string :as string]))

(defn get-input-puzzle
  [filename]
  (-> filename
      io/resource
      slurp
      (string/split #"\n")))

(defn inputs->instructions
  "인풋 데이터들을 명령어 맵으로 변환
   input: (\"nop +0\" \"acc +1\")
   output: [{:op nop :arg 0} {:op acc :arg 1}]"
  [inputs]
  (->> inputs
       (map #(string/split % #" "))
       (mapv (fn [[op arg]]
               {:op op
                :arg (Integer/parseInt arg)}))))


;; case로 구현한 것 -> multimethod로 바꿔보기
(defmulti execute-instruction
  "명령어를 실행하여 다음에 실행할 명령어 idx, acc, 실행했던 명령어 idx들을 업데이트한 새로운 state를 반환
   input: {:idx 0
           :acc 0
           :executed-idxs #{}
           :instructions [{:op acc :arg 3} {:op jmp :arg 4}]}
   output: {:idx 1
            :acc 3
            :executed-idxs #{0}
            :instructions [{:op acc :arg 3} {:op jmp :arg 4}]}
   
   input: {:idx 1
           :acc 3
           :executed-idxs #{0}
           :instructions [{:op acc :arg 3} {:op jmp :arg 4}]}
   output: {:idx 5
            :acc 3
            :executed-idxs #{0 1}
            :instructions [{:op acc :arg 3} {:op jmp :arg 4}]}"
  (fn [{:keys [idx instructions]}]
    ((nth instructions idx) :op)))

(defmethod execute-instruction "acc"
  [{:keys [idx acc executed-idxs instructions] :as state}]
  (let [{arg :arg} (nth instructions idx)]
    (assoc state
           :idx (inc idx)
           :acc (+ acc arg)
           :executed-idxs (conj executed-idxs idx))))

(defmethod execute-instruction "jmp"
  [{:keys [idx executed-idxs instructions] :as state}]
  (let [{arg :arg} (nth instructions idx)]
    (assoc state
           :idx (+ idx arg)
           :executed-idxs (conj executed-idxs idx))))

(defmethod execute-instruction "nop"
  [{:keys [idx executed-idxs instructions] :as state}]
  (let [{arg :arg} (nth instructions idx)]
    (assoc state
           :idx (inc idx)
           :executed-idxs (conj executed-idxs idx))))

(defn update-running-state
  "현재 state를 확인하여 running-state 값이 업데이트된 새로운 state 반환
   1) 다음에 실행할 명령어 idx가 실행했던 명령어 idx에 이미 있는 경우 -> in-loop로 변경
   input: {:idx 0
           :acc 3
           :executed-idxs #{0 1}
           :instructions [{:op acc :arg 3} {:op jmp :arg -1}]
           :running-state :running}
   output: {:idx 0
            :acc 3
            :executed-idxs #{0 1}
            :instructions [{:op acc :arg 3} {:op jmp :arg -1}]
            :running-state :in-loop}
   
   2) 다음에 실행할 명령어 idx가 전체 명령어 길이를 넘는 경우 -> terminated로 변경
   input: {:idx 5
           :acc 3
           :executed-idxs #{1 2}
           :instructions [{:op acc :arg 3} {:op jmp :arg 4}]
           :running-state :running}
   output: {:idx 5
           :acc 3
           :executed-idxs #{1 2}
           :instructions [{:op acc :arg 3} {:op jmp :arg 4}]
           :running-state :terminated}
           
   3) 그 이외 경우 -> running으로 유지"
  [{:keys [idx executed-idxs instructions] :as state}]
  (cond
    (executed-idxs idx)
    (assoc state :running-state :in-loop)

    (>= idx (count instructions))
    (assoc state :running-state :terminated)

    :else (assoc state :running-state :running)))

(defn get-next-state
  "명령어 하나를 실행하여 다음 state를 얻는 함수
   명령어 실행 결과 이외에도 다음 명령어를 실행할 수 있는지에 대한 여부(running-state)도 같이 업데이트
   input: {:idx 0
           :acc 0
           :executed-idxs #{}
           :instructions [{:op acc :arg 3} {:op jmp :arg 4}]
           :running-state :running}
   output: {:idx 1
            :acc 3
            :executed-idxs #{0}
            :instructions [{:op acc :arg 3} {:op jmp :arg 4}]
            :running-state :running}"
  [state]
  (-> state
      execute-instruction
      update-running-state))

(defn get-first-state-not-running
  "running-state가 처음으로 running이 아닌 state를 찾아 반환
   input: [{:op acc :arg 3} {:op jmp :arg 1} {:op jmp :arg -2}]
   output: {:idx 0
            :acc 3
            :executed-idxs #{0 1 2}
            :instructions [{:op acc :arg 3} {:op jmp :arg 1} {:op jmp :arg -2}]
            :running-state :in-loop}"
  [instructions]
  (let [initial-state {:idx 0
                       :acc 0
                       :executed-idxs #{}
                       :instructions instructions
                       :running-state :running}]
    (->> initial-state
         (iterate get-next-state)
         (drop-while #(= (% :running-state) :running)) ;; keyword로 관ㄹ
         first)))

(defn find-jmp-or-nop-idxs
  "명령어 벡터에서 op값이 jmp 혹은 nop인 명령어의 인덱스들을 반환
   input: [{:op acc :arg 3} {:op nop :arg 0} {:op jmp :arg -2}]
   output: (1 2)"
  [instructions]
  (->> instructions
       (keep-indexed (fn [idx {:keys [op]}] 
                       (when (#{"jmp" "nop"} op) idx)))))  

(defn jmp<->nop-once
  "명령어 벡터에서 op값이 jmp인 것은 nop으로, nop인 것은 jmp으로 하나씩만 스왑한 명령어 벡터를 반환
   input: [{:op acc :arg 3} {:op nop :arg 1} {:op jmp :arg -2}]
   output: ([{:op acc :arg 3} {:op jmp :arg 1} {:op jmp :arg -2}]
            [{:op acc :arg 3} {:op nop :arg 1} {:op nop :arg -2}])"
  [instructions]
  (let [jmp-or-nop-idxs (find-jmp-or-nop-idxs instructions)]
    (->> jmp-or-nop-idxs
         (map (fn [idx]
                (let [instruction (nth instructions idx)
                      {:keys [op arg]} instruction]
                  (case op
                    "jmp" (assoc instructions idx {:op "nop" :arg arg})
                    "nop" (assoc instructions idx {:op "jmp" :arg arg}))))))))

(comment
  ;; part1
  (let [inputs (get-input-puzzle "2020/day8.sample.txt")
        instructions (inputs->instructions inputs)]
    (-> instructions
        get-first-state-not-running
        :acc))
  
  ;; part2
  (let [inputs (get-input-puzzle "2020/day8.sample.txt")
        instructions (inputs->instructions inputs)
        jmp<->nop-once-instructions (jmp<->nop-once instructions)]
    (->> jmp<->nop-once-instructions
         (map get-first-state-not-running)
         (filter #(= (% :running-state) :terminated))
         first
         :acc))
  )