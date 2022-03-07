(ns aoc2018_4
  (:require [clojure.java.io :as io] 
            [clojure.string :as str]))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 2시 5분~11분, 3시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.
(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp
      (str/split #"\n")))

(defn parse
  [inputs]
  (->> inputs
       ->logs
       (map ->sleep-wake-events)
       ->sleep-wake-events-by-guard-id))

(defn ->logs
  "인풋 데이터로부터 가드별로 시작 로그, sleep 로그, wake 로그가 담긴 map 리스트를 생성하여 반환
   input: (\"[1518-11-21 23:46] Guard #2969 begins shift\"
           \"[1518-11-22 00:05] falls asleep\"
           \"[1518-11-22 00:55] wakes up\"
           \"[1518-11-23 00:04] Guard #2969 begins shift\"
           \"[1518-11-23 00:30] falls asleep\"
           \"[1518-11-23 00:36] wakes up\")
   output: ({:begin-shift-log \"[1518-11-21 23:46] Guard #2969 begins shift\"
             :sleep-logs (\"[1518-11-22 00:05] falls asleep\")
             :wake-logs (\"[1518-11-22 00:55] wakes up\")}
            {:begin-shift-log \"[1518-11-23 00:04] Guard #2969 begins shift\"
             :sleep-logs (\"[1518-11-23 00:30] falls asleep\")
             :wake-logs (\"[1518-11-23 00:36] wakes up\")})"
  [inputs]
  (let [logs (->> inputs
                  (partition-by #(str/includes? % "Guard"))
                  (partition 2))]
    (->> logs
         (map #(hash-map :begin-shift-log (last (first %))
                         :sleep-logs (take-nth 2 (second %))
                         :wake-logs (take-nth 2 (rest (second %))))))))

(defn ->sleep-wake-events
  "로그에서 guard의 id, 자기 시작한 시간, 일어난 시간을 파싱하는 함수
   input: {:begin-shift-log \"[1518-11-21 23:46] Guard #2969 begins shift\"
           :sleep-logs (\"[1518-11-22 00:05] falls asleep\")
           :wake-logs (\"[1518-11-22 00:55] wakes up\")
   output: {:guard-id 2969
            :sleep-begin-times (5)
            :wake-times (55)}"
  [inputs]
  (let [guard-id (->> inputs
                       :begin-shift-log
                       (re-matches #".* #(\d+) .*")
                       last
                       Integer/parseInt)
        sleep-begin-times (->> inputs
                              :sleep-logs
                              (map #(re-matches #".*:(\d+).*" %))
                              (map last)
                              (map #(Integer/parseInt %)))
        wake-times (->> inputs
                       :wake-logs
                       (map #(re-matches #".*:(\d+).*" %))
                       (map last)
                       (map #(Integer/parseInt %)))]
    {:guard-id guard-id
     :sleep-begin-times sleep-begin-times
     :wake-times wake-times}))

(defn ->sleep-wake-events-by-guard-id
  "이벤트들을 guard-id를 기준으로 group으로 묶는 함수
   input: ({:guard-id 2969
            :sleep-begin-times (5)
            :wake-times (55)}
           {:guard-id 2969
            :sleep-begin-times (10)
            :wake-times (60)}
           {:guard-id 2970
            :sleep-begin-times (5)
            :wake-times (55)}
           {:guard-id 2970
            :sleep-begin-times (10)
            :wake-times (60)})
   output: ({:guard-id 2969
             :sleep-begin-times (5 10)
             :wake-times (55 60)}
            {:guard-id 2970
             :sleep-begin-times (5 10)
             :wake-times (55 60)})"
  [inputs]
  (let [logs (group-by :guard-id inputs)]
    (for [[guard-id logs] logs]
      {:guard-id guard-id
       :sleep-begin-times (mapcat :sleep-begin-times logs)
       :wake-times (mapcat :wake-times logs)})))

(defn ->total-sleep-time-and-sleep-minutes
  "가드의 총 수면 시간과 잤던 분(minute)을 map으로 제공하는 함수
   input: {:guard-id 2969 
           :sleep-begin-times (5 10) 
           :wake-times (8 13)}
   output: {:guard-id 2969
            :total-sleep-time 6
            :sleep-minutes (5 6 7 10 11 12)}"
  [{:keys [guard-id sleep-begin-times wake-times]}]
  (let [sleep-wake-times (map vector sleep-begin-times wake-times)
        total-sleep-time (->> sleep-wake-times
                              (map (fn [[x y]] (- y x)))
                              (apply +))
        sleep-minutes (->> sleep-wake-times
                           (mapcat #(range (first %) (second %))))]
    {:guard-id guard-id
     :total-sleep-time total-sleep-time
     :sleep-minutes sleep-minutes}))

(defn find-max-sleep-minute
  "가드가 잤던 분(minute)들 중에 가장 많이 겹치는 분과 해당 분의 frequency를 제공하는 함수
   input: {:guard-id 2969 
           :sleep-minutes (1 3 1 1 3 0)}
   output: {:guard-id 2969
            :max-sleep-minute 1
            :freq 3}"
  [{:keys [guard-id sleep-minutes]}]
  (let [sleep-minutes-freq (frequencies sleep-minutes)
        max-sleep-minute (apply max-key val sleep-minutes-freq)]
    {:guard-id guard-id
     :max-sleep-minute (key max-sleep-minute)
     :freq (val max-sleep-minute)}))

;; part1
(comment
  (->> (get-input-puzzle "day4.sample.txt")
       sort
       parse
       (map ->total-sleep-time-and-sleep-minutes)
       (apply max-key :total-sleep-time)
       find-max-sleep-minute
       ((juxt :guard-id :max-sleep-minute))
       (apply *)))

;; part2
(defn find-max-freq
  "같은 분(minute)에 가장 자주 잔 가드의 정보를 제공하는 함수
   input: ({:guard-id 2969
            :sleep-minutes (1 3 1 1 3)}
           {:guard-id 2970
            :sleep-minutes (2 2 3)})
   output: {:guard-id 2969
            :max-sleep-minute 1
            :freq 3}"
  [inputs]
  (->> inputs
       (map find-max-sleep-minute)
       (apply max-key :freq)))
        
(comment
  (->> (get-input-puzzle "day4.sample.txt")
       sort
       parse
       (map ->total-sleep-time-and-sleep-minutes)
       find-max-freq
       ((juxt :guard-id :max-sleep-minute))
       (apply *)))