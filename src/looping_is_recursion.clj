(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* base acc) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [prev a-seq]
                 (if (empty? a-seq)
                   prev
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (empty? seq1) (empty? seq2)
                   (empty? seq2) false
                   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
                   :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur (inc index) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [index 0
         sum 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum index)
      (recur (inc index) (+ sum (first a-seq)) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [result #{}
         seq a-seq]
    (if (empty? seq)
      result
      (recur (toggle result (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (loop [sum 0
         prev 1
         prev-prev 1
         n n]
    (if (< n 1)
      sum
      (recur (+ sum prev) sum prev (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         seq a-seq
         result []]
    (if (or (empty? seq) (contains? seen (first seq)) )
      result
      (recur (toggle seen (first seq)) (rest seq) (conj result (first seq))))))

;;   (loop [prev nil
;;          seq a-seq
;;          result '()]
;;     (if (empty? seq)
;;       result
;;       (recur (first seq) (rest seq) (conj result (first seq))))))
