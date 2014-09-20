(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [prev b-seq]
                 (if (empty? b-seq)
                   prev
                   (recur (first b-seq) (rest b-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [i 0
         b-seq a-seq]
    (cond 
      (empty? b-seq) nil
      (pred (first b-seq)) i
      :else (recur (inc i) (rest b-seq)))))

(defn avg [a-seq]
  (loop [i 0
         sum 0
         b-seq a-seq]
    (if (empty? b-seq)
      (if (zero? i)
        0
        (/ sum i))
      (recur (inc i) (+ sum (first b-seq)) (rest b-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [acc #{}
          b-seq a-seq]
     (if (empty? b-seq)
       acc
       (recur (toggle acc (first b-seq)) (rest b-seq))))))


(defn fast-fibo [n]
  (loop [i 0
         next 1 
         curr 0]
    (if (== n i)
      curr
      (recur (inc i) (+ next curr) next))))
      

(defn cut-at-repetition [a-seq]
  (loop [acc []
         setacc #{}
         b-seq a-seq]
    (if (or (empty? b-seq) (contains? setacc (first b-seq)))
      acc
      (recur (conj acc (first b-seq)) (conj setacc (first b-seq)) (rest b-seq))))) 
