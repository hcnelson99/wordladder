(ns wordladder.core
  (:require [clojure.string :refer [join split-lines]])
  (:gen-class))

(def words (split-lines (slurp "words.txt")))
(def words-set (apply hash-set words))
(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn gen-all-peers [word]
  (filter #(not= word %) (flatten (map (fn [i] (map #(apply str (assoc (vec word) i %)) 
                                                   alphabet)) 
                            (range (count word))))))

(defn gen-peers [word]
  (vec (filter #(contains? words-set %) (gen-all-peers word))))

(def adj-dict 
  (into {} 
        (filter #(not (empty? (second %))) 
                (map vec (partition 2 (interleave words (mapv gen-peers words)))))))

(defn search [start end] 
  (loop [visited (conj #{} start), queue (conj clojure.lang.PersistentQueue/EMPTY [start])]
    (if (empty? queue)
      nil
      (let [t (peek queue), queue (pop queue), 
            add-to-queue (filter #(not (contains? visited (peek %))) (map #(conj t %) (get adj-dict (peek t))))]
        (if (= (peek t) end)
          t
          (recur (conj visited (peek t)) 
                 (if (empty? add-to-queue) 
                   queue 
                   (apply conj queue add-to-queue))))))))

(defn -main
  [& args]
  (cond (not= 2 (count args)) (prn "Error: Needs 2 arguments")
        (not  (contains? words-set (first args)))  (println "Error:" (first args)  "is not a valid 4-letter word")
        (not  (contains? words-set (second args))) (println "Error:" (second args) "is not a valid 4-letter word")
        :else (println (join " -> " (search (first args) (second args))))))
