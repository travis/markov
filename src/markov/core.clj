(ns markov.core
  "Ported from code here:

http://travis-whitton.blogspot.com/2009/06/markov-chaining.html
"
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns nil."
  [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))

(defn rand-elt
  "Return a random element of this seq"
  [s]
  (nth s (rand-int (count s))))

(defn clean [txt]
  "clean given txt for symbols disruptive to markov chains"
  (-> txt
      (str/replace #"[:;,^\"()]" "")
      (str/replace #"'(?!(d|t|ve|m|ll|s|de|re))" "")))

(defn chain-lengths [markov-chain]
  "return a set of lengths for each element in the collection"
  (let [markov-keys (map keys markov-chain)]
    (set (for [x markov-keys] (count x)))))

(defn max-chain-length [markov-chain]
  "return the length lf the longest chain"
  (apply max (chain-lengths markov-chain)))

(defn chain
  "Take a list of words and build a markov chain out of them.
  The length is the size of the key in number of words."
  ([words]
   (chain words 3))
  ([words length]
   (loop [markov-chain {}
          keychain (for [x (range length)] nil)
          words (map clean words)]
     (let [first-word (first words)]
       (if (seq words)
         (recur (assoc markov-chain keychain
                       (cons first-word (get markov-chain keychain)))
                (concat (rest keychain) [first-word])
                (rest words))
         (assoc markov-chain keychain []))))))

(defn split-sentence [text]
  "Convert a string to a collection on common boundaries"
  (filter seq (str/split text #"[,.!?()\d]+\s*")))

(defn files-to-sentences [files]
  (apply concat
         (map #(split-sentence (slurp %))
              (if (instance? String files) [files] files))))

(defn file-chain
  "Create a markov chain from the contents of a given file"
  ([files]
   (file-chain files 3))
  ([files length]
   (let [sentences (files-to-sentences files)
         flatten-list (fn [& x] (flatten (vec x)))]
     (loop [markov-chain {} words sentences]
       (if (seq words)
         (recur (merge-with flatten-list
                            markov-chain
                            (chain (str/split (first words) #"\s+")))
                (rest words))
         markov-chain)))))

(defn construct-sentence
   "Build a sentence from a markov chain structure.  Given a
   Markov chain (any size key),  Seed (to start the sentence) and
   Proc (a function for choosing the next word), returns a sentence
   composed until is reaches the end of a chain (an end of sentence)."
  ([markov-chain]
   (construct-sentence markov-chain nil rand-elt))
  ([markov-chain seed]
   (construct-sentence markov-chain seed rand-elt))
  ([markov-chain seed proc]
   (loop [words (if seed seed (rand-elt (keys markov-chain)))
          sentence (str/join " " (filter identity words))]
     (if (seq (markov-chain words))
       (let [word-new (proc (markov-chain words))]
         (recur (concat (rest words) [word-new])
                (str/join " " (into [sentence] [word-new]))))
       sentence))))


(comment

  (def markov (file-chain ["/Users/travis/Desktop/alice_in_wonderland.txt"
                           "/Users/travis/Desktop/revelation.txt"]
                          3))
(construct-sentence markov)

(doseq [x (range 100)]
  (doseq [x (range 3)] (println (construct-sentence markov)))
  (println))

)
