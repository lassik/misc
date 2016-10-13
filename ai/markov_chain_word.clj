(defdeps ; lein-oneoff
  [[org.clojure/clojure "1.6.0"]])

(ns markov-chain-word
  (:require [clojure.string :as str]))

(defn text-from-string [string]
  (->> (-> string
           (str/lower-case)
           (str/replace #"[\W\s]+" " ")
           (str/split #" "))
       (remove #(< (count %) 3))))

(defn string-from-text [text]
  (str/join " " text))

(defn jump-table [n text]
  {:pre [(> n 0)]}
  ((fn [phrase text table]
     (if-not (seq text)
       table
       (recur (conj (vec (rest phrase)) (first text))
              (rest text)
              (assoc table phrase (conj (table phrase #{}) (first text))))))
   (vec (take n text)) (drop n text) {}))

(defn markov [n text]
  (let [table (jump-table n text)]
    ((fn [phrase output]
       (let [next-word (rand-nth (seq (table phrase nil)))]
         (if-not next-word
           output
           (recur (conj (vec (rest phrase)) next-word)
                  (conj output (first phrase))))))
     (rand-nth (keys table)) [])))

(println (string-from-text (markov 2 (text-from-string (slurp *in*)))))
