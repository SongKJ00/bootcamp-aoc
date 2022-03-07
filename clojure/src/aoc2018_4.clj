(ns aoc2018_4
  (:require [clojure.java.io :as io] 
            [clojure.string :as string]))

(defn get-input-puzzle [filename]
  (-> filename
      io/resource
      slurp
      (string/split #"\n")))

(defn parse
  [inputs]
  (->> inputs
       input-data->logs
       (map log->sleep-wake-times)
       group-sleep-wake-times-by-guard-id
       (map sleep-wake-times->total-sleep-time-and-sleep-minutes)))

(defn input-data->logs
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
  (->> inputs
       (partition-by #(string/includes? % "Guard"))
       (partition 2)
       (map (fn [v] {:begin-shift-log (last (first v))
                     :sleep-logs (filter #(string/includes? % "falls asleep") (second v))
                     :wake-logs (filter #(string/includes? % "wakes up") (second v))}))))

(defn log->sleep-wake-times
  "로그에서 guard의 id, 자기 시작한 시간, 일어난 시간을 파싱하는 함수
   input: {:begin-shift-log \"[1518-11-21 23:46] Guard #2969 begins shift\"
           :sleep-logs (\"[1518-11-22 00:05] falls asleep\")
           :wake-logs (\"[1518-11-22 00:55] wakes up\")
   output: {:guard-id 2969
            :sleep-begin-times (5)
            :wake-times (55)}"
  [{:keys [:begin-shift-log :sleep-logs :wake-logs]}]
  (let [guard-id (find-first-matched-int-data #".* #(\d+) .*" begin-shift-log)
        sleep-begin-times (map #(find-first-matched-int-data #".*:(\d+).*" %) sleep-logs)
        wake-times (map #(find-first-matched-int-data #".*:(\d+).*" %) wake-logs)]
    {:guard-id guard-id
     :sleep-begin-times sleep-begin-times
     :wake-times wake-times}))

(defn find-first-matched-int-data
  "주어진 정규표현식으로 매칭되는 정수 데이터 중에 가장 첫번째를 리턴하는 함수
   input: #\".* #(\\d+) .*\"
          \"[1518-11-21 23:46] Guard #2969 begins shift\"
   output: 2969"
  [re s]
  (let [match (re-matches re s)]
    (-> match
        second
        Integer/parseInt)))

(defn group-sleep-wake-times-by-guard-id
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
  [sleep-wake-times]
  (let [sleep-wake-times (group-by :guard-id sleep-wake-times)]
    (for [[guard-id times] sleep-wake-times]
      {:guard-id guard-id
       :sleep-begin-times (mapcat :sleep-begin-times times)
       :wake-times (mapcat :wake-times times)})))

(defn sleep-wake-times->total-sleep-time-and-sleep-minutes
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

(defn find-most-sleep-minute
  "가드가 잤던 분(minute)들 중에 가장 많이 겹치는 분과 해당 분의 frequency를 제공하는 함수
   input: {:guard-id 2969 
           :sleep-minutes (1 3 1 1 3 0)}
   output: {:guard-id 2969
            :most-sleep-minute 1
            :freq 3}"
  [{:keys [guard-id sleep-minutes]}]
  (let [sleep-minutes-freq (frequencies sleep-minutes)
        most-sleep-minute (apply max-key val sleep-minutes-freq)]
    {:guard-id guard-id
     :most-sleep-minute (key most-sleep-minute)
     :freq (val most-sleep-minute)}))

;; part1
(comment
  (->> (get-input-puzzle "day4.sample.txt")
       sort
       parse
       (apply max-key :total-sleep-time)
       find-most-sleep-minute
       ((juxt :guard-id :most-sleep-minute))
       (apply *)))

;; part2
(defn find-most-sleep-guard-at-same-time
  "같은 분(minute)에 가장 자주 잔 가드의 정보를 제공하는 함수
   input: ({:guard-id 2969
            :sleep-minutes (1 3 1 1 3)}
           {:guard-id 2970
            :sleep-minutes (2 2 3)})
   output: {:guard-id 2969
            :most-sleep-minute 1
            :freq 3}"
  [inputs]
  (->> inputs
       (map find-most-sleep-minute)
       (apply max-key :freq)))
        
(comment
  (->> (get-input-puzzle "day4.sample.txt")
       sort
       parse
       find-most-sleep-guard-at-same-time
       ((juxt :guard-id :most-sleep-minute))
       (apply *)))
